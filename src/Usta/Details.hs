{-# LANGUAGE OverloadedStrings #-}

{- | What a pull request changes and what is on a branch, read from the
repository's mirror when it has one and the forge otherwise. A mirror is
used only once it holds what is asked for (the pull request's refs are
fetched); a git failure falls back to the forge too.
-}
module Usta.Details
    ( Settings (..)
    , Details (..)
    , From (..)
    , new
    )
where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Sql (ConnectionPool)
import System.IO.Unsafe (unsafePerformIO)

import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Usta.Accounts (Accounts (..))
import Usta.DB (run)
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (Failure (..), Forge (..))
import Usta.Mirrors (Mirrors (..))
import Usta.Types.Branch (Branch, Tag)
import Usta.Types.Commit (Commit (..), Sha)
import Usta.Types.Diff (FileDiff)
import Usta.Types.Insights (Blamed, Change, Node)
import Usta.Types.Number (Number)
import Usta.Types.Parse (unparse)
-- For the pull requests' fields.

import Usta.Types.Ref (Ref (..))
import Usta.Types.RepoName (RepoName)
import Usta.Types.Tree (Entry)

import Usta.DB.PullRequest qualified as DB
import Usta.Git qualified as Git
import Usta.Types.PullRequest qualified as P

type Settings :: Type
data Settings = Settings
    { pool :: ConnectionPool
    , accounts :: Accounts
    , mirrors :: Mirrors
    }

-- | Where an answer came from, which the screens say.
type From :: Type
data From = FromMirror | FromForge
    deriving stock (Eq, Show)

{- | Each answer says where it came from, or is 'Left' with words for the
user when neither the mirror nor the forge could give it.
-}
type Details :: Type
data Details = Details
    { pullFiles :: AccountId -> RepoName -> Number -> IO (Either Text (From, [FileDiff]))
    , pullCommits :: AccountId -> RepoName -> Number -> IO (Either Text (From, [Commit]))
    , commitDiff :: AccountId -> RepoName -> Sha -> IO (Either Text (From, [FileDiff]))
    , recentCommits :: AccountId -> RepoName -> Text -> IO (Either Text (From, [Commit]))
    -- ^ The last commits on a branch, newest first.
    , readme :: AccountId -> RepoName -> Text -> IO (Either Text (From, Maybe (Text, Text)))
    -- ^ A branch's README, with its file name.
    , tree :: AccountId -> RepoName -> Text -> IO (Either Text (From, [Entry]))
    -- ^ Every folder and file on a branch.
    , file :: AccountId -> RepoName -> Text -> Text -> IO (Either Text (From, ByteString))
    -- ^ A file's contents on a branch, by path.
    , branchList :: AccountId -> RepoName -> Text -> IO (Either Text (From, [Branch]))
    {- ^ Every branch, most recently pushed first; from the mirror with how
    far each is from the default branch given.
    -}
    , tagList :: AccountId -> RepoName -> IO (Either Text (From, [Tag]))
    , history :: AccountId -> RepoName -> Text -> Int -> IO (Either Text (From, [Commit]))
    -- ^ A branch's history, newest first, 50 commits a page from 1.
    , changes :: AccountId -> RepoName -> Text -> Maybe UTCTime -> IO (Either Text [Change])
    -- ^ Each commit's size on a branch since a time; the mirror only.
    , checkOut :: AccountId -> RepoName -> Number -> FilePath -> Text -> Text -> IO (Either Text ())
    {- ^ A pull request in a worktree of the mirror, at a folder on a local
    branch tracking its branch; fetches the mirror first.
    -}
    , blame :: AccountId -> RepoName -> Text -> Text -> IO (Either Text [Blamed])
    -- ^ A file on a branch, line by line with the last commit; the mirror only.
    , fileHistory :: AccountId -> RepoName -> Text -> Text -> IO (Either Text (From, [Commit]))
    -- ^ The commits on a branch that changed a file.
    , graph :: AccountId -> RepoName -> [Text] -> Bool -> Int -> IO (Either Text [Node])
    {- ^ The graph's page, from 1, 100 commits each, of the given branches or
    all; the mirror only.
    -}
    }

new :: Settings -> Details
new settings =
    Details
        { pullFiles = \key repo n -> pull key repo n Git.diff (\f -> f.pullDiff repo n)
        , pullCommits = \key repo n -> pull key repo n Git.history (\f -> f.pullCommits repo n) >>= named key repo (\f -> f.pullCommits repo n)
        , commitDiff = \key repo sha -> either' key repo (\path -> Git.commitDiff path sha) (\f -> f.commitDiff repo sha)
        , recentCommits = \key repo branch ->
            either' key repo (\path -> Git.recent path branch 10) (\f -> f.recentCommits repo branch)
        , readme = \key repo branch -> either' key repo (\path -> Git.readme path branch) (\f -> f.readme repo branch)
        , tree = \key repo branch -> either' key repo (\path -> Git.tree path branch) (\f -> f.tree repo branch)
        , file = \key repo branch name ->
            either' key repo (\path -> BL.toStrict <$> Git.blob path branch name) (\f -> f.file repo branch name)
        , branchList = \key repo base -> either' key repo (\path -> Git.branches path base) (\f -> f.branchList repo)
        , tagList = \key repo -> either' key repo Git.tags (\f -> f.tags repo)
        , changes = \key repo branch since -> mirrorOnly key repo (\path -> Git.changes path branch since)
        , checkOut = \key repo n dir local upstream -> mirrorFor "A worktree needs the local mirror. Mirror this repository first, or check out a separate clone." key repo $ \path -> do
            Git.fetch path
            Git.worktree path dir local ("refs/pull/" <> T.pack (show (unparse n)) <> "/head") upstream
        , blame = \key repo branch file -> mirrorFor "Blame is read from a local mirror. Mirror this repository from its Overview to see it." key repo (\path -> Git.blame path branch file)
        , fileHistory = \key repo branch file -> either' key repo (\path -> Git.fileLog path branch file 50) (\f -> f.fileHistory repo branch file)
        , graph = \key repo scope firstParent n -> mirrorOnly key repo (\path -> Git.graph path scope firstParent ((n - 1) * 100) 100)
        , history = \key repo branch n ->
            either' key repo (\path -> Git.page path branch ((n - 1) * 50) 50) (\f -> f.history repo branch n) >>= named key repo (\f -> f.history repo branch 1)
        }
    where
        -- A pull request from the mirror needs both its base branch and its own
        -- head there.
        -- A mirror's commits know only names; the forge's own page of the
        -- branch says whose accounts they are, once per repository.
        named key repo asked = \case
            Right (FromMirror, commits) | any (\c -> c.login == Nothing) commits -> do
                logins <- loginsOf key repo asked
                pure (Right (FromMirror, [c {login = c.login <|> Map.lookup c.author logins} | c <- commits]))
            other -> pure other
        loginsOf key repo asked = do
            known <- Map.lookup (key, repo) <$> readIORef accountNames
            case known of
                Just names -> pure names
                Nothing -> do
                    forge <- settings.accounts.forgeOf key
                    found <- case forge of
                        Nothing -> pure []
                        Just f -> either (\(_ :: SomeException) -> []) id <$> try (asked f)
                    let
                        names = Map.fromList [(c.author, l) | c <- found, Just l <- [c.login]]
                    atomicModifyIORef' accountNames (\m -> (Map.insert (key, repo) names m, ()))
                    pure names

        pull :: AccountId -> RepoName -> Number -> (FilePath -> Sha -> Sha -> IO a) -> (Forge -> IO a) -> IO (Either Text (From, a))
        pull key repo n fromMirror fromForge = do
            base <- baseOf key repo n
            either'
                key
                repo
                ( \path -> do
                    ends <- (,) <$> maybe (pure Nothing) (Git.resolve path) base <*> Git.resolve path ("refs/pull/" <> T.pack (show (unparse n)) <> "/head")
                    case ends of
                        (Just from, Just to) -> fromMirror path from to
                        _ -> ioError (userError "the mirror has not fetched this pull request yet")
                )
                fromForge

        baseOf key repo n = do
            rows <- run settings.pool DB.pullRequests
            pure $ case [pr.baseBranch | (k, pr) <- rows, k == key, pr.ref.repo == repo, pr.ref.number == n] of
                branch : _ -> Just branch
                [] -> Nothing

        -- The mirror when it is there and works, the forge otherwise.
        mirrorOnly :: AccountId -> RepoName -> (FilePath -> IO a) -> IO (Either Text a)
        mirrorOnly = mirrorFor "Insights are drawn from a local mirror. Mirror this repository from its Overview to see them."
        mirrorFor :: Text -> AccountId -> RepoName -> (FilePath -> IO a) -> IO (Either Text a)
        mirrorFor why key repo fromMirror =
            settings.mirrors.ready key repo >>= \case
                Nothing -> pure (Left why)
                Just p -> either (\(e :: IOError) -> Left (T.pack (show e))) Right <$> try (fromMirrorSafe (fromMirror p))
        either' :: AccountId -> RepoName -> (FilePath -> IO a) -> (Forge -> IO a) -> IO (Either Text (From, a))
        either' key repo fromMirror fromForge = do
            path <- settings.mirrors.ready key repo
            local <- case path of
                Nothing -> pure Nothing
                Just p -> either (\(_ :: IOError) -> Nothing) Just <$> try (fromMirrorSafe (fromMirror p))
            case local of
                Just answer -> pure (Right (FromMirror, answer))
                Nothing -> do
                    forge <- settings.accounts.forgeOf key
                    case forge of
                        Nothing -> pure (Left "Sign in to this account again to load this.")
                        Just f -> do
                            asked <- try (fromForge f)
                            pure $ case asked of
                                Right answer -> Right (FromForge, answer)
                                Left Offline -> Left "The forge could not be reached."
                                Left SignedOut -> Left "The forge refused the account's token; sign in again."
                                Left (Refused message) -> Left ("The forge said: " <> message)

        -- Git's own failure counts as the mirror not having it.
        fromMirrorSafe action = do
            result <- try action
            case result of
                Left (Git.GitError message) -> ioError (userError (T.unpack message))
                Right answer -> pure answer

{- | Author names to the accounts they are, by account and repository,
for the run.
-}
accountNames :: IORef (Map.Map (AccountId, RepoName) (Map.Map Text Text))
accountNames = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE accountNames #-}
