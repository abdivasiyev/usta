{-# LANGUAGE OverloadedStrings #-}

-- | Where the window gets what it shows: the lists, as the pages show
-- them, and word of what changed. 'fromDatabase' reads the cache; 'samples'
-- serves the mock lists, for the first-run previews. Neither signs in until
-- 'withAccounts' gives it the accounts.
module Usta.UI.Source
  ( Source (..)
  , Connected (..)
  , fromDatabase
  , samples
  , withAccounts
  , withMirrors
  , withDetails
  ) where

import Data.Kind (Type)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (ConnectionPool, toSqlKey)
import System.Directory (getHomeDirectory)

import Usta.Accounts (Accounts, Origins (..), describe)
import qualified Usta.Accounts as Accounts
import Usta.Bus (Bus, Change (..), subscribe)
import Usta.DB (run)
import Usta.DB.Account (listAccounts)
import Usta.DB.Mirror (choiceFor)
import Usta.Details (Details, From)
-- Qualified, since Source has fields of the same names.
import qualified Usta.Details as Details
import Usta.DB.Schema.Account (AccountId)
import qualified Usta.DB.Issue as DB
import qualified Usta.DB.Notification as DB
import qualified Usta.DB.PullRequest as DB
import qualified Usta.DB.Repository as DB
import Usta.Mirrors (Mirrors (..))
import qualified Usta.Mirrors as Mirrors
import Usta.Sync (Status, Sync (..))
import Usta.Types.Account (Account (..))
import Usta.Types.Forge (Kind)
import Usta.Types.Commit (Commit, Sha)
import Usta.Types.Diff (FileDiff)
import Usta.Types.Number (Number)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Host (Host)
import Usta.Types.Mirror (Choice (..))
-- For the repositories' fields.
import qualified Usta.Types.Repository as R
import Usta.Types.Parse (parse, unparse)
-- For the pull requests' fields.
import qualified Usta.Types.PullRequest as P
import Usta.Types.Ref (Ref (..))
import Usta.Types.Resource (Resource)
import Usta.Types.Token (Token)
import Usta.UI.Format (ago, clock)
import qualified Usta.UI.Pages.Inbox as Inbox
import qualified Usta.UI.Pages.Issues as Issues
import qualified Usta.UI.Pages.PullRequests as PullRequests
import qualified Usta.UI.Pages.Repositories as Repositories
import Usta.UI.Sidebar (Model (..))
import Usta.UI.Style (Tone (..))
import qualified Usta.UI.Sidebar as Sidebar

type Source :: Type
data Source = Source
  { inbox :: IO [Inbox.Notification]
  , pullRequests :: IO [PullRequests.PullRequest]
  , issues :: IO [Issues.Issue]
  , repositories :: IO [Repositories.Repository]
  , accounts :: IO [Connected]
  , onChange :: (Resource -> IO ()) -> IO ()
    -- ^ Runs the action on the GTK main thread whenever a list was saved
    -- anew.
  , onStatus :: (Status -> IO ()) -> IO ()
    -- ^ Likewise for each account's sync state, starting with the last
    -- one each account reported before.
  , syncNow :: IO ()
  , signIn :: Kind -> Host -> Token -> IO (Either Text ())
    -- ^ 'Left' says, for the user, why it did not work.
  , remove :: AccountId -> IO (Either Text ())
  , mirror :: Text -> Text -> Choice -> IO (Either Text ())
    -- ^ The user's mirror answer for the repository on a host, by name.
  , onMirrorStatus :: IO () -> IO ()
    -- ^ Runs the action on the GTK main thread whenever a mirror's state
    -- changes.
  , mirrorOrigins :: Text -> Text -> IO (Either Text Repositories.Origins)
    -- ^ Where a mirror of the repository on a host, by name, would clone
    -- from; for the mirror dialog.
  , pullFiles :: Text -> Text -> Int -> IO (Either Text (From, [FileDiff]))
    -- ^ A pull request's changes, by host, repository and number.
  , pullCommits :: Text -> Text -> Int -> IO (Either Text (From, [Commit]))
  , commitDiff :: Text -> Text -> Sha -> IO (Either Text (From, [FileDiff]))
  , recentCommits :: Text -> Text -> Text -> IO (Either Text (From, [Commit]))
    -- ^ A branch's last commits, by host, repository and branch.
  , readme :: Text -> Text -> Text -> IO (Either Text (From, Maybe (Text, Text)))
  }

-- | A signed-in account, as the window names it.
type Connected :: Type
data Connected = Connected
  { accountId :: AccountId
  , forge :: Kind
  , host :: Text
  , login :: Text
  }

-- | The lists as the cache has them, with the times formatted against the
-- clock at each read.
fromDatabase :: ConnectionPool -> Bus Change -> Bus Status -> Sync -> Source
fromDatabase pool changes statuses sync = Source
  { inbox = do
      c <- clock
      items <- run pool DB.inbox
      pure (mapMaybe (\i -> Inbox.fromNotification c i.forge i.notification i.pullRequest) items)
  , pullRequests = do
      c <- clock
      kindOf <- kinds
      rows <- run pool DB.pullRequests
      pure [view | (key, pr) <- rows, Just kind <- [kindOf key], Just view <- [PullRequests.fromPullRequest c kind pr]]
  , issues = do
      c <- clock
      kindOf <- kinds
      rows <- run pool DB.issues
      pure [Issues.fromIssue c kind issue | (key, issue) <- rows, Just kind <- [kindOf key]]
  , repositories = do
      kindOf <- kinds
      rows <- run pool DB.repositories
      pure [Repositories.fromRepository kind r | (key, r) <- rows, Just kind <- [kindOf key]]
  , accounts = do
      rows <- run pool listAccounts
      pure [Connected {accountId = key, forge = a.forge, host = unparse a.host, login = unparse a.login} | (key, a) <- rows]
  , onChange = \act -> subscribe changes (act . (.resource))
  , onStatus = \act -> subscribe statuses act >> (sync.latest >>= mapM_ act)
  , syncNow = sync.syncNow
  , signIn = \_ _ _ -> pure (Left unavailable)
  , remove = \_ -> pure (Left unavailable)
  , mirror = \_ _ _ -> pure (Left unavailable)
  , onMirrorStatus = const (pure ())
  , mirrorOrigins = \_ _ -> pure (Left unavailable)
  , pullFiles = \_ _ _ -> pure (Left unavailable)
  , pullCommits = \_ _ _ -> pure (Left unavailable)
  , commitDiff = \_ _ _ -> pure (Left unavailable)
  , recentCommits = \_ _ _ -> pure (Left unavailable)
  , readme = \_ _ _ -> pure (Left unavailable)
  }
  where
    kinds = kindsIn pool

-- | Each account's kind of forge.
kindsIn :: ConnectionPool -> IO (AccountId -> Maybe Kind)
kindsIn pool = do
  accounts <- run pool listAccounts
  pure (\key -> (.forge) <$> lookup key accounts)

-- | The mock lists, which never change.
samples :: Source
samples = Source
  { inbox = pure Inbox.sample
  , pullRequests = pure PullRequests.sample
  , issues = pure Issues.sample
  , repositories = pure Repositories.sample
  , accounts = pure [Connected {accountId = toSqlKey n, forge = f, host = h, login = "abdivasiyev"} | (n, (f, h)) <- zip [1 ..] Sidebar.sample.forges]
  , onChange = const (pure ())
  , onStatus = const (pure ())
  , syncNow = pure ()
  , signIn = \_ _ _ -> pure (Left unavailable)
  , remove = \_ -> pure (Left unavailable)
  , mirror = \_ _ _ -> pure (Left unavailable)
  , onMirrorStatus = const (pure ())
  , mirrorOrigins = \_ _ -> pure (Left unavailable)
  , pullFiles = \_ _ _ -> pure (Left unavailable)
  , pullCommits = \_ _ _ -> pure (Left unavailable)
  , commitDiff = \_ _ _ -> pure (Left unavailable)
  , recentCommits = \_ _ _ -> pure (Left unavailable)
  , readme = \_ _ _ -> pure (Left unavailable)
  }

-- | For the previews and @usta --sample@, which never touch the keyring.
unavailable :: Text
unavailable = "Signing in is not available here."

-- | Signs in and out through @accounts@.
withAccounts :: Accounts -> Source -> Source
withAccounts accounts source = source
  { signIn = \kind host token -> either (Left . describe) (const (Right ())) <$> accounts.signIn kind host token
  , remove = \key -> either (Left . describe) Right <$> accounts.remove key
  }

-- | The repositories with their mirrors' state, choosing a mirror through
-- @accounts@, and word of each mirror's progress from @statuses@.
withMirrors :: ConnectionPool -> Accounts -> Mirrors -> Bus Mirrors.Status -> Source -> Source
withMirrors pool accounts mirrors statuses source = source
  { repositories = do
      c <- clock
      kindOf <- kindsIn pool
      rows <- run pool DB.repositories
      latest <- mirrors.latest
      sequence
        [ do
            choice <- run pool (choiceFor key r.name)
            let state = [s.state | s <- latest, s.accountId == key, s.repo == r.name]
            pure (withState c choice (listToMaybe (reverse state)) (Repositories.fromRepository kind r))
        | (key, r) <- rows, Just kind <- [kindOf key]
        ]
  , mirror = \host name choice -> do
      found <- repoOf host name
      case found of
        Just (key, repo) -> either (Left . describe) Right <$> accounts.mirror key repo choice
        Nothing -> pure (Left gone)
  , mirrorOrigins = \host name -> do
      found <- repoOf host name
      case found of
        Nothing -> pure (Left gone)
        Just (key, repo) -> do
          home <- getHomeDirectory
          root <- Mirrors.defaultRoot
          let keptIn = maybe (T.pack root) ("~" <>) (T.stripPrefix (T.pack home) (T.pack root))
          asked <- accounts.origins key repo
          pure $ case asked of
            Left problem -> Left (describe problem)
            Right o -> Right Repositories.Origins
              { https = either (Left . describe) Right o.https
              , ssh = either (Left . describe) Right o.ssh
              , keptIn = keptIn
              }
  , onMirrorStatus = \act -> subscribe statuses (const act)
  }
  where
    repoOf host name = do
      rows <- run pool DB.repositories
      pure $ case [(key, r.name) | (key, r) <- rows, unparse r.host == host, unparse r.name == name] of
        found : _ -> Just found
        [] -> Nothing
    gone = "That repository is not in the cache any more."
    withState c choice state view = case (choice, state) of
      (Nothing, _) -> view {Repositories.canMirror = True}
      (Just Declined, _) -> view
        { Repositories.mirror = (Neutral, "API only")
        , Repositories.mirrorDetail = Just "Not mirrored, as you chose: diffs and history come from the forge's API."
        , Repositories.canMirror = True
        }
      (Just (Mirrored _ _), Just Mirrors.Cloning) -> view {Repositories.mirror = (Accent, "Cloning…")}
      (Just (Mirrored _ _), Just Mirrors.Fetching) -> view {Repositories.mirror = (Accent, "Fetching…")}
      (Just (Mirrored _ _), Just (Mirrors.Ready at)) ->
        view {Repositories.mirror = (Success, "Up to date"), Repositories.mirrorDetail = Just ("Fetched " <> ago c at)}
      -- A failed mirror can be chosen again: another transport, or the API.
      (Just (Mirrored _ _), Just (Mirrors.Failed message)) ->
        view {Repositories.mirror = (Danger, "Failed"), Repositories.mirrorDetail = Just message, Repositories.canMirror = True}
      (Just (Mirrored _ _), Nothing) -> view {Repositories.mirror = (Neutral, "Waiting to fetch")}

-- | Pull requests' and branches' contents through @details@, each found by
-- the account that has its repository in the cache.
withDetails :: ConnectionPool -> Details -> Source -> Source
withDetails pool details source = source
  { pullFiles = \host repo n -> pullOf host repo n details.pullFiles
  , pullCommits = \host repo n -> pullOf host repo n details.pullCommits
  , commitDiff = \host repo sha -> repoOf host repo (\key name -> details.commitDiff key name sha)
  , recentCommits = \host repo branch -> repoOf host repo (\key name -> details.recentCommits key name branch)
  , readme = \host repo branch -> repoOf host repo (\key name -> details.readme key name branch)
  }
  where
    pullOf :: Text -> Text -> Int -> (AccountId -> RepoName -> Number -> IO (Either Text a)) -> IO (Either Text a)
    pullOf host repo n ask = case parse n of
      Left _ -> pure (Left gone)
      Right number -> repoOf host repo (\key name -> ask key name number)
    -- The repository may be the user's own or one they only have pull
    -- requests in.
    repoOf :: Text -> Text -> (AccountId -> RepoName -> IO (Either Text a)) -> IO (Either Text a)
    repoOf host repo ask = do
      repos <- run pool DB.repositories
      prs <- run pool DB.pullRequests
      let keys = [key | (key, r) <- repos, unparse r.host == host, unparse r.name == repo]
            <> [key | (key, pr) <- prs, unparse pr.ref.host == host, unparse pr.ref.repo == repo]
      case (keys, parse repo) of
        (key : _, Right name) -> ask key name
        _ -> pure (Left gone)
    gone = "That repository is not in the cache any more."
