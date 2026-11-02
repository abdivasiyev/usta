{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A notification from a forge: what the inbox lists.
module Usta.Types.Notification
  ( Subject (..)
  , Reason (..)
  , Notification (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Types.Host (Host)
import Usta.Types.Number (Number)
import Usta.Types.Parse (Parse (..), Parsed (..))
import Usta.Types.RepoName (RepoName)
import Usta.Types.ThreadId (ThreadId)

-- | What the notification is about.
type Subject :: Type
data Subject
  = PullRequestSubject Number
  | IssueSubject Number
  | OtherSubject
    -- ^ A release, commit or discussion: listed, but not opened in usta.
  deriving stock (Eq, Show)

-- | Why you got the notification. The raw form is GitHub's name for the
-- reason; Forgejo reports none.
type Reason :: Type
data Reason
  = ReviewRequested
  | Mentioned
  | TeamMentioned
  | Assigned
  | Authored
  | Commented
  | CiActivity
    -- ^ A workflow run you triggered finished; whether it failed is the
    -- pull request's check state.
  | StateChanged
  | Subscribed
  deriving stock (Eq, Show, Enum, Bounded)
  deriving (PersistField, PersistFieldSql) via Parsed Reason

instance Parse Reason where
  type Raw Reason = Text
  parse raw = case raw of
    "review_requested" -> Right ReviewRequested
    "mention" -> Right Mentioned
    "team_mention" -> Right TeamMentioned
    "assign" -> Right Assigned
    "author" -> Right Authored
    "comment" -> Right Commented
    "ci_activity" -> Right CiActivity
    "state_change" -> Right StateChanged
    "subscribed" -> Right Subscribed
    _ -> Left "a notification reason such as review_requested, mention or assign"
  unparse reason = case reason of
    ReviewRequested -> "review_requested"
    Mentioned -> "mention"
    TeamMentioned -> "team_mention"
    Assigned -> "assign"
    Authored -> "author"
    Commented -> "comment"
    CiActivity -> "ci_activity"
    StateChanged -> "state_change"
    Subscribed -> "subscribed"

-- | One notification thread.
type Notification :: Type
data Notification = Notification
  { threadId :: ThreadId
  , host :: Host
  , repo :: RepoName
  , subject :: Subject
  , title :: Text
  , reason :: Maybe Reason
    -- ^ 'Nothing' on Forgejo, which does not say why.
  , unread :: Bool
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show)
