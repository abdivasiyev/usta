{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A pull request, as the lists and the review screen use it.
module Usta.Types.PullRequest
    ( State (..)
    , Checks (..)
    , Review (..)
    , PullRequest (..)
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Types.Count (Count)
import Usta.Types.Involvement (Involvement)
import Usta.Types.Login (Login)
import Usta.Types.Parse (Parse (..), Parsed (..))
import Usta.Types.Ref (Ref)

-- | Where the pull request stands.
type State :: Type
data State = Open | Draft | Merged | Closed
    deriving stock (Bounded, Enum, Eq, Show)
    deriving (PersistField, PersistFieldSql) via Parsed State

instance Parse State where
    type Raw State = Text
    parse raw = case raw of
        "open" -> Right Open
        "draft" -> Right Draft
        "merged" -> Right Merged
        "closed" -> Right Closed
        _ -> Left "a pull request state: open, draft, merged or closed"
    unparse state = case state of
        Open -> "open"
        Draft -> "draft"
        Merged -> "merged"
        Closed -> "closed"

-- | The combined result of the checks on the latest commit.
type Checks :: Type
data Checks = ChecksPassed | ChecksFailed | ChecksRunning
    deriving stock (Bounded, Enum, Eq, Show)
    deriving (PersistField, PersistFieldSql) via Parsed Checks

instance Parse Checks where
    type Raw Checks = Text
    parse raw = case raw of
        "passed" -> Right ChecksPassed
        "failed" -> Right ChecksFailed
        "running" -> Right ChecksRunning
        _ -> Left "a checks state: passed, failed or running"
    unparse checks = case checks of
        ChecksPassed -> "passed"
        ChecksFailed -> "failed"
        ChecksRunning -> "running"

-- | Where review stands overall.
type Review :: Type
data Review = AwaitingReview | Approved | ChangesRequested
    deriving stock (Bounded, Enum, Eq, Show)
    deriving (PersistField, PersistFieldSql) via Parsed Review

instance Parse Review where
    type Raw Review = Text
    parse raw = case raw of
        "awaiting" -> Right AwaitingReview
        "approved" -> Right Approved
        "changes_requested" -> Right ChangesRequested
        _ -> Left "a review state: awaiting, approved or changes_requested"
    unparse review = case review of
        AwaitingReview -> "awaiting"
        Approved -> "approved"
        ChangesRequested -> "changes_requested"

-- | One pull request.
type PullRequest :: Type
data PullRequest = PullRequest
    { ref :: Ref
    , title :: Text
    , author :: Login
    , headBranch :: Text
    -- ^ The branch with the changes.
    , headOwner :: Maybe Text
    -- ^ Who owns the fork the branch is in; 'Nothing' in the repository.
    , baseBranch :: Text
    -- ^ The branch it would merge into.
    , state :: State
    , checks :: Maybe Checks
    -- ^ 'Nothing' when the repository runs no checks.
    , review :: Maybe Review
    -- ^ 'Nothing' when nobody is asked to review.
    , comments :: Count
    , changedFiles :: Maybe Count
    -- ^ 'Nothing' until the details are fetched; lists don't carry it.
    , commits :: Maybe Count
    , involvement :: [Involvement]
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    }
    deriving stock (Eq, Show)
