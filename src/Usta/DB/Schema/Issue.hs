{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The @issues@ table: the issues each account has synced. An issue is
-- open until @closed_at@ is set. The host comes from the account.
module Usta.DB.Schema.Issue
  ( Issue (..)
  , IssueId
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
import Usta.Types.Login (Login)
import Usta.Types.Number (Number)
import Usta.Types.RepoName (RepoName)

-- 'discoverEntities' passes the entities already in scope (the accounts
-- table), so the account reference becomes a foreign key.
share
  [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
  [persistLowerCase|
Issue sql=issues
    accountId  AccountId OnDeleteCascade
    repo       RepoName
    number     Number
    title      Text
    author     Login
    comments   Count
    createdAt  UTCTime
    updatedAt  UTCTime
    closedAt   UTCTime Maybe
    syncedAt   UTCTime

    UniqueIssue accountId repo number
    deriving Eq Show
|]

type Issue :: Type

type IssueId :: Type
