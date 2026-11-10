{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The @notifications@ table: each account's notification threads, as last
synced.
-}
module Usta.DB.Schema.Notification
    ( Notification (..)
    , NotificationId
    , SubjectKind (..)
    , EntityField (..)
    , Unique (..)
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (EntityField, PersistField, Unique)
import Database.Persist.Sql (PersistFieldSql)
import Database.Persist.TH

import Usta.DB.Schema.Account (AccountId)
import Usta.Types.Notification (Reason)
import Usta.Types.Number (Number)
import Usta.Types.Parse (Parse (..), Parsed (..))
import Usta.Types.RepoName (RepoName)
import Usta.Types.ThreadId (ThreadId)

{- | How the @subject_kind@ column spells the application's 'Subject': the
kind here, the number in @subject_number@.
-}
type SubjectKind :: Type
data SubjectKind = PullRequestKind | IssueKind | OtherKind
    deriving stock (Bounded, Enum, Eq, Show)
    deriving (PersistField, PersistFieldSql) via Parsed SubjectKind

instance Parse SubjectKind where
    type Raw SubjectKind = Text
    parse raw = case raw of
        "pull_request" -> Right PullRequestKind
        "issue" -> Right IssueKind
        "other" -> Right OtherKind
        _ -> Left "a subject kind: pull_request, issue or other"
    unparse kind = case kind of
        PullRequestKind -> "pull_request"
        IssueKind -> "issue"
        OtherKind -> "other"

-- 'discoverEntities' passes the entities already in scope (the accounts
-- table), so the account reference becomes a foreign key; without it
-- persistent drops the reference silently.
share
    [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
    [persistLowerCase|
Notification sql=notifications
    accountId      AccountId OnDeleteCascade
    threadId       ThreadId
    repo           RepoName
    subjectKind    SubjectKind
    subjectNumber  Number Maybe
    title          Text
    reason         Reason Maybe
    unread         Bool
    updatedAt      UTCTime
    done           Bool default=0

    UniqueNotification accountId threadId
    deriving Eq Show
|]

type Notification :: Type

type NotificationId :: Type
