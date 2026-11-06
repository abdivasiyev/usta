{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Mirrors cloned and fetched in the background, and reads that take the
-- mirror when it has the answer and the forge otherwise.
module Usta.MirrorsTests (tests) where

import Control.Exception (bracket, throwIO)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Usta.Accounts (Accounts (..), Problem (..))
import Usta.Bus (listen, newBus)
import Usta.DB (run)
import Usta.DB.Account (saveAccount)
import Usta.DB.Mirror (choose)
import Usta.DB.PullRequest (savePullRequests)
import Usta.DB.Schema.Account (AccountId)
import Usta.Details (Details (..), From (..))
import qualified Usta.Details as Details
import Usta.Forge (Failure (..), Forge)
import qualified Usta.Forge as F
import qualified Usta.Forge.Sample as Sample
import qualified Usta.Git as Git
import Usta.GitMirrorTests (dayAfter, git, gitAt)
import Usta.Mirrors (Mirrors (..), State (..), Status (..))
import qualified Usta.Mirrors as Mirrors
import Usta.SyncTests (githubAccount, withCache)
import Usta.Types.Commit (Commit (..))
import Usta.Types.Diff (FileDiff (..))
import Usta.Types.Mirror (Choice (..), Transport (..))
import Usta.Types.Number (Number)
import Usta.Types.Parse (parseM)
import Usta.Types.PullRequest (PullRequest (..))
import qualified Usta.Types.PullRequest as P
import Usta.Types.Ref (Ref (..))
import Usta.Types.RepoName (RepoName)

tests :: TestTree
tests =
  testGroup
    "Mirrors"
    [ testCase "a chosen mirror is cloned, and then read" $ withMirrors (Just Git.Full) $ \t -> do
        t.mirrors.ensure t.account t.repo
        waitReady t
        t.mirrors.ready t.account t.repo >>= assertBool "not ready" . maybe False (const True)
    , testCase "a declined repository has no mirror" $ withMirrors Nothing $ \t ->
        t.mirrors.ready t.account t.repo >>= (@?= Nothing)
    , testCase "a clone that fails says git's words, and nothing is ready" $ withMirrorsAt "/no/such/origin" (Just Git.Full) $ \t -> do
        t.mirrors.ensure t.account t.repo
        failed <- waitFor t (\case Failed _ -> True; _ -> False)
        assertBool "no words" (failed /= Failed "")
        t.mirrors.ready t.account t.repo >>= (@?= Nothing)
    , testCase "a failure of the disk fails that mirror, and the next updates go on" $ withMirrors (Just Git.Full) $ \t -> do
        -- A file where the host's folder should be.
        writeFile (t.root </> "github.com") ""
        t.mirrors.ensure t.account t.repo
        _ <- waitFor t (\case Failed _ -> True; _ -> False)
        removeFile (t.root </> "github.com")
        t.mirrors.ensure t.account t.repo
        waitReady t
    , testCase "a mirror no repository wants any more is pruned" $ withMirrors (Just Git.Full) $ \t -> do
        createDirectoryIfMissing True (t.root </> "github.com" </> "someone" </> "old.git")
        pruned <- Mirrors.prune t.pool t.root
        pruned @?= 1
        doesDirectoryExist (t.root </> "github.com" </> "someone" </> "old.git") >>= (@?= False)
    , testCase "commits come from the mirror once it is there" $ withMirrors (Just Git.Full) $ \t -> do
        before <- t.details.recentCommits t.account t.repo "main"
        fmap fst before @?= Right FromForge
        t.mirrors.ensure t.account t.repo >> waitReady t
        found <- t.details.recentCommits t.account t.repo "main"
        fmap (\(from, cs) -> (from, map (.subject) cs)) found @?= Right (FromMirror, ["Main moves on", "First"])
    , testCase "a pull request's files from the mirror" $ withMirrors (Just Git.Full) $ \t -> do
        t.mirrors.ensure t.account t.repo >> waitReady t
        found <- t.details.pullFiles t.account t.repo t.pull
        fmap (\(from, fs) -> (from, map (.path) fs)) found @?= Right (FromMirror, ["feature.txt"])
    , testCase "what the mirror lacks comes from the forge" $ withMirrors (Just Git.Full) $ \t -> do
        t.mirrors.ensure t.account t.repo >> waitReady t
        fmap fst <$> t.details.recentCommits t.account t.repo "no-such-branch" >>= (@?= Right FromForge)
    , testCase "a forge out of reach says so" $ withMirrorsForge (\f -> f {F.recentCommits = \_ _ -> ioErrorOffline}) (Just Git.Full) $ \t ->
        fmap fst <$> t.details.recentCommits t.account t.repo "main" >>= (@?= Left "The forge could not be reached.")
    , testCase "blame needs the mirror" $ withMirrors Nothing $ \t ->
        t.details.blame t.account t.repo "main" "README.md" >>= assertBool "blamed without a mirror" . either (T.isInfixOf "mirror") (const False)
    , testCase "blame from the mirror" $ withMirrors (Just Git.Full) $ \t -> do
        t.mirrors.ensure t.account t.repo >> waitReady t
        fmap length <$> t.details.blame t.account t.repo "main" "README.md" >>= (@?= Right 2)
    ]
  where
    ioErrorOffline = throwIO Offline

data Running = Running
  { pool :: ConnectionPool
  , account :: AccountId
  , repo :: RepoName
  , pull :: Number
  , root :: FilePath
  , mirrors :: Mirrors
  , details :: Details
  , next :: IO Status
  }

withMirrors :: Maybe Git.Kind -> (Running -> IO ()) -> IO ()
withMirrors = withMirrorsForge id

withMirrorsForge :: (Forge -> Forge) -> Maybe Git.Kind -> (Running -> IO ()) -> IO ()
withMirrorsForge adapt kind act = withSystemTempDirectory "usta-origin" $ \dir -> do
  let origin = dir </> "origin"
  build origin
  withMirrorsOf adapt ("file://" <> origin) kind act

withMirrorsAt :: String -> Maybe Git.Kind -> (Running -> IO ()) -> IO ()
withMirrorsAt url = withMirrorsOf id url

-- | The github.com account with abdivasiyev/usta mirrored from @url@ as
-- @kind@ (declined without one), its pull request #1 saved, and mirrors
-- and details running on the sample forge.
withMirrorsOf :: (Forge -> Forge) -> String -> Maybe Git.Kind -> (Running -> IO ()) -> IO ()
withMirrorsOf adapt url kind act = withCache $ \pool -> withSystemTempDirectory "usta-mirrors" $ \root -> do
  now <- getCurrentTime
  account <- githubAccount
  key <- run pool (saveAccount now account)
  repo <- parseM "abdivasiyev/usta"
  n <- parseM 1
  sample <- Sample.new now account
  let forge = adapt sample.forge {F.recentCommits = \_ _ -> pure []}
  host <- parseM "github.com"
  author <- parseM "abdivasiyev"
  zero <- parseM 0
  run pool $ do
    choose now key repo (maybe Declined (\k -> Mirrored k Https) kind) (T.pack url <$ kind)
    savePullRequests key now
      [ PullRequest
          { ref = Ref {host = host, repo = repo, number = n}, title = "Add feature", author = author
          , headBranch = "feature", headOwner = Nothing, baseBranch = "main", state = P.Open
          , checks = Nothing, review = Nothing, comments = zero, changedFiles = Nothing, commits = Nothing
          , involvement = [], createdAt = now, updatedAt = now } ]
  changes <- newBus
  statuses <- newBus
  next <- listen statuses
  bracket (Mirrors.start Mirrors.Settings {pool = pool, changes = changes, statuses = statuses, root = root}) (.stop) $ \mirrors -> do
    let accounts = Accounts
          { signIn = \_ _ _ -> pure (Left NotSignedIn)
          , remove = \_ -> pure (Left NotSignedIn)
          , mirror = \_ _ _ -> pure (Left NotSignedIn)
          , origins = \_ _ -> pure (Left NotSignedIn)
          , forgeOf = \k -> pure (if k == key then Just forge else Nothing)
          , beginBrowser = \_ _ -> pure (Left "no browser here")
          , signInBrowser = \_ _ _ _ -> pure (Left NotSignedIn)
          }
        details = Details.new Details.Settings {pool = pool, accounts = accounts, mirrors = mirrors}
    act Running {pool = pool, account = key, repo = repo, pull = n, root = root, mirrors = mirrors, details = details, next = next}

-- | Waits for the mirror to say it is up to date.
waitReady :: Running -> IO ()
waitReady t = () <$ waitFor t (\case Ready _ -> True; _ -> False)

waitFor :: Running -> (State -> Bool) -> IO State
waitFor t wanted = timeout 20000000 go >>= maybe (assertFailure "the mirror never got there" >> pure Cloning) pure
  where
    go = t.next >>= \s -> if s.repo == t.repo && wanted s.state then pure s.state else go

-- | main: First, Main moves on; feature from First: Add feature, which
-- refs/pull/1/head points at.
build :: FilePath -> IO ()
build origin = do
  createDirectoryIfMissing True origin
  git origin ["init", "-q", "-b", "main"]
  writeFile (origin </> "README.md") "# demo\n"
  git origin ["add", "-A"] >> gitAt (dayAfter 0) origin ["commit", "-q", "-m", "First"]
  git origin ["checkout", "-q", "-b", "feature"]
  writeFile (origin </> "feature.txt") "new\n"
  git origin ["add", "-A"] >> gitAt (dayAfter 1) origin ["commit", "-q", "-m", "Add feature"]
  git origin ["update-ref", "refs/pull/1/head", "feature"]
  git origin ["checkout", "-q", "main"]
  writeFile (origin </> "README.md") "# demo\nmore\n"
  git origin ["add", "-A"] >> gitAt (dayAfter 2) origin ["commit", "-q", "-m", "Main moves on"]
