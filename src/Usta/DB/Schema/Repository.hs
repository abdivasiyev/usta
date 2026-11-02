{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The @repositories@ table: the repositories an account can see, as last
-- synced. The host comes from the account.
module Usta.DB.Schema.Repository
  ( Repository (..)
  , RepositoryId
  , EntityField (..)
  , Unique (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH

import Usta.DB.Schema.Account (AccountId)
import Usta.Types.Count (Count)
import Usta.Types.RepoName (RepoName)

-- 'discoverEntities' passes the entities already in scope (the accounts
-- table), so the account reference becomes a foreign key; without it
-- persistent drops the reference silently.
share
  [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
  [persistLowerCase|
Repository sql=repositories
    accountId         AccountId OnDeleteCascade
    name              RepoName
    description       Text Maybe
    defaultBranch     Text
    private           Bool
    openPullRequests  Count
    openIssues        Count
    updatedAt         UTCTime
    syncedAt          UTCTime

    UniqueRepository accountId name
    deriving Eq Show
|]

type Repository :: Type

type RepositoryId :: Type
