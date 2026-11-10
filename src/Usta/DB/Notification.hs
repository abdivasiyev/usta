{-# LANGUAGE OverloadedStrings #-}

{- | The notifications each account has synced, and the inbox built from
them.
-}
module Usta.DB.Notification
    ( InboxItem (..)
    , saveNotifications
    , setUnread
    , setDone
    , inbox
    )
where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Kind (Type)
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

import Data.Map.Strict qualified as Map
import Database.Persist qualified as Persist

import Usta.DB (Result, requireHost)
import Usta.DB.PullRequest (fromEntities)
import Usta.DB.Schema.Account (AccountId)
import Usta.DB.Schema.Notification (EntityField (..), SubjectKind (..), Unique (..))
import Usta.Types.Forge (Kind)
import Usta.Types.Host (Host)
import Usta.Types.Notification (Notification (..), Subject (..))
import Usta.Types.PullRequest (PullRequest)
import Usta.Types.ThreadId (ThreadId)

import Usta.DB.Schema.Account qualified as Account
import Usta.DB.Schema.Notification qualified as Schema
import Usta.DB.Schema.PullRequest qualified as PullRequest

-- | One row of the inbox.
type InboxItem :: Type
data InboxItem = InboxItem
    { accountId :: AccountId
    , forge :: Kind
    , notification :: Notification
    , pullRequest :: Maybe PullRequest
    {- ^ The pull request it is about, when that has been synced too: the
    inbox shows its state and checks.
    -}
    , done :: Bool
    -- ^ Marked done in usta: shown under Done, not in the inbox.
    }
    deriving stock (Eq, Show)

{- | Adds each notification, or updates the one with the same thread.
Throws 'Usta.DB.WrongHost' if any is not from the account's server.
-}
saveNotifications :: AccountId -> [Notification] -> Result ()
saveNotifications account notifications = do
    requireHost account (map (\n -> n.host) notifications)
    for_ notifications $ \n -> do
        let
            row = toRow account n
        -- New activity on a thread marked done brings it back to the inbox.
        before <- Persist.getBy (UniqueNotification account n.threadId)
        let
            renewed = maybe False (\(Entity _ old) -> old.done && n.updatedAt > old.updatedAt) before
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
        when renewed
            $ Persist.updateWhere
                [NotificationAccountId Persist.==. account, NotificationThreadId Persist.==. n.threadId]
                [NotificationDone Persist.=. False]
        pure ()

{- | Marks the thread read or unread in the cache, as usta just told the
forge; the next sync would say the same.
-}
setUnread :: AccountId -> ThreadId -> Bool -> Result ()
setUnread account thread unread =
    Persist.updateWhere
        [NotificationAccountId Persist.==. account, NotificationThreadId Persist.==. thread]
        [NotificationUnread Persist.=. unread]

{- | Marks the thread done in usta, or brings it back to the inbox. Usta
keeps this itself: a forge only knows read and unread.
-}
setDone :: AccountId -> ThreadId -> Bool -> Result ()
setDone account thread done =
    Persist.updateWhere
        [NotificationAccountId Persist.==. account, NotificationThreadId Persist.==. thread]
        [NotificationDone Persist.=. done]

-- | Every account's notifications, most recently updated first.
inbox :: Result [InboxItem]
inbox = do
    rows <- select $ do
        (n :& account :& pr) <-
            from
                $ table @Schema.Notification
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
    let
        linked = catMaybes [(\p -> (entityKey p, ((entityVal account).host, p))) <$> pr | (_, account, pr) <- rows]
    converted <- fromEntities (map snd linked)
    let
        byKey = Map.fromList (zip (map fst linked) converted)
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
                    , done = row.done
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
        , done = False
        }
    where
        (kind, number) = case n.subject of
            PullRequestSubject x -> (PullRequestKind, Just x)
            IssueSubject x -> (IssueKind, Just x)
            OtherSubject -> (OtherKind, Nothing)

{- | Fails on a pull request or issue row without its number: the one
combination the columns allow that the application type doesn't.
-}
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
