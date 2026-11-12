{-# LANGUAGE OverloadedStrings #-}

{- | usta as it runs with accounts: a cache synced from the sample forges
(github.com and git.azizovich.uz), and the window's source built from
it as usta builds it. The forges keep what they are sent, and answer
what the samples lack: a diff, commits and checks.
-}
module World
    ( World (..)
    , withWorld
    )
where

import Control.Exception (bracket)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Timeout (timeout)

import Data.Text qualified as T

import Usta.Accounts (Accounts (..), Problem (..))
import Usta.Bus (listen, newBus)
import Usta.DB (run, withDatabase)
import Usta.DB.Account (saveAccount)
import Usta.Forge (Forge)
import Usta.Sync (State (..), Status (..))
import Usta.Types.Account (Account (..), AuthMethod (..))
import Usta.Types.Check (Check (..))
import Usta.Types.Commit (Commit (..))
import Usta.Types.Diff (parseDiff)
import Usta.Types.Parse (parseM, unparse)
import Usta.UI.Source (Source)

import Usta.Details qualified as Details
import Usta.Forge qualified as F
import Usta.Forge.Sample qualified as Sample
import Usta.Mirrors qualified as Mirrors
import Usta.Sync qualified as Sync
import Usta.Types.Check qualified as Check
import Usta.Types.Discussion qualified as D
import Usta.Types.Forge qualified as Kind
import Usta.UI.Source qualified as Source

data World = World
    { source :: Source
    , sent :: IORef [Text]
    -- ^ What the forges were sent, oldest first.
    }

{- | A world's folder, in the run's home, which goes when the run ends:
a window's last loads may still read its cache after its test ends,
and a callback failing on a removed cache ends the run.
-}
worldDirectory :: IO FilePath
worldDirectory = getHomeDirectory >>= (`createTempDirectory` "usta-world")

withWorld :: (World -> IO a) -> IO a
withWorld act =
    worldDirectory >>= \dir -> withDatabase (dir </> "usta.db") $ \pool -> do
        now <- getCurrentTime
        github <- Account Kind.GitHub <$> parseM "github.com" <*> parseM "abdivasiyev" <*> pure AccessToken
        forgejo <- Account Kind.Forgejo <$> parseM "git.azizovich.uz" <*> parseM "abdivasiyev" <*> pure AccessToken
        sent <- newIORef []
        forges <-
            traverse
                ( \account -> do
                    key <- run pool (saveAccount now account)
                    s <- Sample.new now account
                    f <- keeping sent s.forge
                    pure (key, f)
                )
                [github, forgejo]
        changes <- newBus
        statuses <- newBus
        next <- listen statuses
        bracket (Sync.start Sync.Settings {pool = pool, changes = changes, statuses = statuses, every = 3600} forges) (.stop) $ \sync -> do
            -- Both accounts' first syncs, before the window reads the cache.
            let
                synced n =
                    if n == (0 :: Int)
                        then pure ()
                        else
                            next >>= \s -> case s.state of
                                Synced _ -> synced (n - 1)
                                _ -> synced n
            _ <- timeout 10000000 (synced (length forges))
            let
                accounts =
                    Accounts
                        { signIn = \_ _ _ -> pure (Left NotSignedIn)
                        , remove = \_ -> pure (Left NotSignedIn)
                        , mirror = \_ _ _ -> pure (Left NotSignedIn)
                        , origins = \_ _ -> pure (Left NotSignedIn)
                        , forgeOf = \k -> pure (lookup k forges)
                        , beginBrowser = \_ _ -> pure (Left "no browser here")
                        , signInBrowser = \_ _ _ _ -> pure (Left NotSignedIn)
                        }
            mirrorStatuses <- newBus
            bracket (Mirrors.start Mirrors.Settings {pool = pool, changes = changes, statuses = mirrorStatuses, root = dir </> "mirrors"}) (.stop) $ \mirrors -> do
                let
                    details = Details.new Details.Settings {pool = pool, accounts = accounts, mirrors = mirrors}
                    source = Source.withDetails pool details (Source.withMirrors pool accounts mirrors mirrorStatuses (Source.withAccounts pool accounts (Source.fromDatabase pool changes statuses sync)))
                act World {source = source, sent = sent}

-- | The sample forge, keeping what it is sent and with code to show.
keeping :: IORef [Text] -> Forge -> IO Forge
keeping sent f = do
    me <- parseM "abdivasiyev"
    now <- getCurrentTime
    sha <- parseM (T.replicate 40 "a")
    diff <- either (fail . T.unpack) pure (parseDiff longDiff)
    let
        keep line = atomicModifyIORef' sent (\xs -> (xs <> [line], ()))
    pure
        f
            { F.comment = \_ n text -> do
                keep ("comment #" <> T.pack (show (unparse n)) <> ": " <> text)
                pure D.Comment {D.author = me, D.body = text, D.createdAt = now, D.id = Just 1, D.edited = False, D.url = "", D.reactions = [], D.review = Nothing}
            , F.createIssue = \_ new -> keep ("issue: " <> new.title) >> parseM 9
            , F.pullDiff = \_ _ -> pure diff
            , F.pullCommits = \_ _ -> pure [Commit {sha = sha, author = "Asliddin", authoredAt = now, subject = "Add the GTK 4 window", body = "", login = Just "abdivasiyev"}]
            , F.checks = \_ _ ->
                pure
                    [ Check {name = "build (push)", state = Check.Passed, description = "Successful in 2m", url = Nothing, job = Nothing}
                    , Check {name = "test (push)", state = Check.Failed, description = "Failing after 48s", url = Nothing, job = Nothing}
                    ]
            }

longDiff :: Text
longDiff =
    T.unlines
        $ ["diff --git a/usta.cabal b/usta.cabal", "--- a/usta.cabal", "+++ b/usta.cabal", "@@ -1,80 +1,81 @@"]
            <> [" line " <> T.pack (show n) | n <- [1 .. 40 :: Int]]
            <> ["-  build-depends: base", "+  build-depends:", "+    base,"]
            <> [" line " <> T.pack (show n) | n <- [43 .. 80 :: Int]]
