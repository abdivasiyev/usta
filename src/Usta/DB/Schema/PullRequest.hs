{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The @pull_requests@ table: the pull requests each account has synced.
-- The host comes from the account.
module Usta.DB.Schema.PullRequest
  ( PullRequest (..)
  , PullRequestId
  , EntityField (..)
  , Unique (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH

import Usta.DB.Schema.Account (AccountId)
import Usta.Types.Login (Login)
import Usta.Types.Number (Number)
import Usta.Types.PullRequest (Checks, Review, State)
import Usta.Types.RepoName (RepoName)

-- Counts are Int: persistent has no Natural column. Reading a row turns
-- them back into Natural and rejects a negative one.
-- 'discoverEntities' passes the entities already in scope (the accounts
-- table), so the account reference becomes a foreign key.
share
  [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
  [persistLowerCase|
PullRequest sql=pull_requests
    accountId     AccountId OnDeleteCascade
    repo          RepoName
    number        Number
    title         Text
    author        Login
    headBranch    Text
    baseBranch    Text
    state         State
    checks        Checks Maybe
    review        Review Maybe
    comments      Int
    changedFiles  Int Maybe
    commits       Int Maybe
    createdAt     UTCTime
    updatedAt     UTCTime
    syncedAt      UTCTime

    UniquePullRequest accountId repo number
    deriving Eq Show
|]

type PullRequest :: Type

type PullRequestId :: Type
