{-# LANGUAGE OverloadedStrings #-}

-- | The notifications each account has synced, and the inbox built from
-- them.
module Usta.DB.Notification
  ( InboxItem (..)
  , saveNotifications
  , markRead
  , inbox
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Database.Esqueleto.Experimental
  ( Entity (..)
  , InnerJoin (..)
  , LeftOuterJoin (..)
  , desc
  , from
  , just
  , on
  , orderBy
  , select
  , table
  , val
  , (&&.)
  , (:&) (..)
  , (==.)
  )
import Database.Persist (PersistException (PersistMarshalError))
import qualified Database.Persist as Persist

import Usta.DB (Result, requireHost)
import Usta.DB.PullRequest (fromEntities)
import Usta.DB.Schema.Account (AccountId)
import qualified Usta.DB.Schema.Account as Account
import Usta.DB.Schema.Notification (EntityField (..), SubjectKind (..), Unique (..))
import qualified Usta.DB.Schema.Notification as Schema
import qualified Usta.DB.Schema.PullRequest as PullRequest
import Usta.Types.Forge (Kind)
import Usta.Types.Host (Host)
import Usta.Types.Notification (Notification (..), Subject (..))
import Usta.Types.PullRequest (PullRequest)
import Usta.Types.ThreadId (ThreadId)

-- | One row of the inbox.
type InboxItem :: Type
data InboxItem = InboxItem
  { accountId :: AccountId
  , forge :: Kind
  , notification :: Notification
  , pullRequest :: Maybe PullRequest
    -- ^ The pull request it is about, when that has been synced too: the
    -- inbox shows its state and checks.
  }
  deriving stock (Eq, Show)

-- | Adds each notification, or updates the one with the same thread.
-- Throws 'Usta.DB.WrongHost' if any is not from the account's server.
saveNotifications :: AccountId -> [Notification] -> Result ()
saveNotifications account notifications = do
  requireHost account (map (\n -> n.host) notifications)
  for_ notifications $ \n -> do
    let row = toRow account n
    _ <-
      Persist.upsertBy
        (UniqueNotification account n.threadId)
        row
        [ NotificationRepo Persist.=. row.repo
        , NotificationSubjectKind Persist.=. row.subjectKind
        , NotificationSubjectNumber Persist.=. row.subjectNumber
        , NotificationTitle Persist.=. row.title
        , NotificationReason Persist.=. row.reason
        , NotificationUnread Persist.=. row.unread
        , NotificationUpdatedAt Persist.=. row.updatedAt
        ]
    pure ()

-- | Marks the thread read in the cache; telling the forge is sync's job.
markRead :: AccountId -> ThreadId -> Result ()
markRead account thread =
  Persist.updateWhere
    [NotificationAccountId Persist.==. account, NotificationThreadId Persist.==. thread]
    [NotificationUnread Persist.=. False]

-- | Every account's notifications, most recently updated first.
inbox :: Result [InboxItem]
inbox = do
  rows <- select $ do
    (n :& account :& pr) <-
      from $
        table @Schema.Notification
          `InnerJoin` table @Account.Account
          `on` (\(n :& account) -> n.accountId ==. account.id)
          `LeftOuterJoin` table @PullRequest.PullRequest
          `on` ( \(n :& _ :& pr) ->
                   pr.accountId ==. just n.accountId
                     &&. pr.repo ==. just n.repo
                     &&. pr.number ==. n.subjectNumber
                     &&. n.subjectKind ==. val PullRequestKind
               )
    orderBy [desc n.updatedAt]
    pure (n, account, pr)
  let linked = catMaybes [(\p -> (entityKey p, ((entityVal account).host, p))) <$> pr | (_, account, pr) <- rows]
  converted <- fromEntities (map snd linked)
  let byKey = Map.fromList (zip (map fst linked) converted)
  traverse (item byKey) rows
  where
    item byKey (Entity _ row, Entity key account, pr) = do
      n <- either (liftIO . throwIO . PersistMarshalError) pure (fromRow account.host row)
      pure
        InboxItem
          { accountId = key
          , forge = account.forge
          , notification = n
          , pullRequest = pr >>= \p -> Map.lookup (entityKey p) byKey
          }

toRow :: AccountId -> Notification -> Schema.Notification
toRow account n =
  Schema.Notification
    { accountId = account
    , threadId = n.threadId
    , repo = n.repo
    , subjectKind = kind
    , subjectNumber = number
    , title = n.title
    , reason = n.reason
    , unread = n.unread
    , updatedAt = n.updatedAt
    }
  where
    (kind, number) = case n.subject of
      PullRequestSubject x -> (PullRequestKind, Just x)
      IssueSubject x -> (IssueKind, Just x)
      OtherSubject -> (OtherKind, Nothing)

-- | Fails on a pull request or issue row without its number: the one
-- combination the columns allow that the application type doesn't.
fromRow :: Host -> Schema.Notification -> Either Text Notification
fromRow host row = do
  subject <- case (row.subjectKind, row.subjectNumber) of
    (PullRequestKind, Just x) -> Right (PullRequestSubject x)
    (IssueKind, Just x) -> Right (IssueSubject x)
    (OtherKind, _) -> Right OtherSubject
    _ -> Left "a notification about a pull request or issue has no subject_number"
  pure
    Notification
      { threadId = row.threadId
      , host
      , repo = row.repo
      , subject
      , title = row.title
      , reason = row.reason
      , unread = row.unread
      , updatedAt = row.updatedAt
      }
