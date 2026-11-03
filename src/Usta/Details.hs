{-# LANGUAGE OverloadedStrings #-}

-- | What a pull request changes and what is on a branch, read from the
-- repository's mirror when it has one and the forge otherwise. A mirror is
-- used only once it holds what is asked for (the pull request's refs are
-- fetched); a git failure falls back to the forge too.
module Usta.Details
  ( Settings (..)
  , Details (..)
  , From (..)
  , new
  ) where

import Control.Exception (try)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (ConnectionPool)

import Usta.Accounts (Accounts (..))
import Usta.DB (run)
import qualified Usta.DB.PullRequest as DB
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (Failure (..), Forge (..))
import qualified Usta.Git as Git
import Usta.Mirrors (Mirrors (..))
import Usta.Types.Commit (Commit, Sha)
import Usta.Types.Diff (FileDiff)
import Usta.Types.Number (Number)
import Usta.Types.Parse (unparse)
-- For the pull requests' fields.
import qualified Usta.Types.PullRequest as P
import Usta.Types.Ref (Ref (..))
import Usta.Types.RepoName (RepoName)

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

-- | Each answer says where it came from, or is 'Left' with words for the
-- user when neither the mirror nor the forge could give it.
type Details :: Type
data Details = Details
  { pullFiles :: AccountId -> RepoName -> Number -> IO (Either Text (From, [FileDiff]))
  , pullCommits :: AccountId -> RepoName -> Number -> IO (Either Text (From, [Commit]))
  , commitDiff :: AccountId -> RepoName -> Sha -> IO (Either Text (From, [FileDiff]))
  , recentCommits :: AccountId -> RepoName -> Text -> IO (Either Text (From, [Commit]))
    -- ^ The last commits on a branch, newest first.
  , readme :: AccountId -> RepoName -> Text -> IO (Either Text (From, Maybe (Text, Text)))
    -- ^ A branch's README, with its file name.
  }

new :: Settings -> Details
new settings = Details
  { pullFiles = \key repo n -> pull key repo n Git.diff (\f -> f.pullDiff repo n)
  , pullCommits = \key repo n -> pull key repo n Git.history (\f -> f.pullCommits repo n)
  , commitDiff = \key repo sha -> either' key repo (\path -> Git.commitDiff path sha) (\f -> f.commitDiff repo sha)
  , recentCommits = \key repo branch ->
      either' key repo (\path -> Git.recent path branch 10) (\f -> f.recentCommits repo branch)
  , readme = \key repo branch -> either' key repo (\path -> Git.readme path branch) (\f -> f.readme repo branch)
  }
  where
    -- A pull request from the mirror needs both its base branch and its own
    -- head there.
    pull :: AccountId -> RepoName -> Number -> (FilePath -> Sha -> Sha -> IO a) -> (Forge -> IO a) -> IO (Either Text (From, a))
    pull key repo n fromMirror fromForge = do
      base <- baseOf key repo n
      either' key repo
        (\path -> do
           ends <- (,) <$> maybe (pure Nothing) (Git.resolve path) base <*> Git.resolve path ("refs/pull/" <> T.pack (show (unparse n)) <> "/head")
           case ends of
             (Just from, Just to) -> fromMirror path from to
             _ -> ioError (userError "the mirror has not fetched this pull request yet"))
        fromForge

    baseOf key repo n = do
      rows <- run settings.pool DB.pullRequests
      pure $ case [pr.baseBranch | (k, pr) <- rows, k == key, pr.ref.repo == repo, pr.ref.number == n] of
        branch : _ -> Just branch
        [] -> Nothing

    -- The mirror when it is there and works, the forge otherwise.
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
