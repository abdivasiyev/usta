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
  ) where

import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool, toSqlKey)

import Usta.Accounts (Accounts, describe)
import qualified Usta.Accounts as Accounts
import Usta.Bus (Bus, Change (..), subscribe)
import Usta.DB (run)
import Usta.DB.Account (listAccounts)
import Usta.DB.Schema.Account (AccountId)
import qualified Usta.DB.Issue as DB
import qualified Usta.DB.Notification as DB
import qualified Usta.DB.PullRequest as DB
import qualified Usta.DB.Repository as DB
import Usta.Sync (Status, Sync (..))
import Usta.Types.Account (Account (..))
import Usta.Types.Forge (Kind)
import Usta.Types.Host (Host)
import Usta.Types.Parse (unparse)
import Usta.Types.Resource (Resource)
import Usta.Types.Token (Token)
import Usta.UI.Format (clock)
import qualified Usta.UI.Pages.Inbox as Inbox
import qualified Usta.UI.Pages.Issues as Issues
import qualified Usta.UI.Pages.PullRequests as PullRequests
import qualified Usta.UI.Pages.Repositories as Repositories
import Usta.UI.Sidebar (Model (..))
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
  }
  where
    kinds = do
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
