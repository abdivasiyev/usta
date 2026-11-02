{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The @sync_cursors@ table: where each account's sync of each resource
-- stopped, so the next sync asks the forge only for what changed.
module Usta.DB.Schema.SyncCursor
  ( SyncCursor (..)
  , SyncCursorId
  , Resource (..)
  , EntityField (..)
  , Unique (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (EntityField, PersistField, Unique)
import Database.Persist.Sql (PersistFieldSql)
import Database.Persist.TH

import Usta.DB.Schema.Account (AccountId)
import Usta.Types.Parse (Parse (..), Parsed (..))

-- | What a cursor tracks: one per list the sync fetches.
type Resource :: Type
data Resource = Notifications | PullRequests | Issues | Repositories
  deriving stock (Eq, Show, Enum, Bounded)
  deriving (PersistField, PersistFieldSql) via Parsed Resource

instance Parse Resource where
  type Raw Resource = Text
  parse raw = case raw of
    "notifications" -> Right Notifications
    "pull_requests" -> Right PullRequests
    "issues" -> Right Issues
    "repositories" -> Right Repositories
    _ -> Left "a resource: notifications, pull_requests, issues or repositories"
  unparse resource = case resource of
    Notifications -> "notifications"
    PullRequests -> "pull_requests"
    Issues -> "issues"
    Repositories -> "repositories"

-- The ETag is opaque: whatever the forge sent, sent back unchanged.
-- 'discoverEntities' passes the entities already in scope (the accounts
-- table), so the account reference becomes a foreign key.
share
  [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
  [persistLowerCase|
SyncCursor sql=sync_cursors
    accountId  AccountId OnDeleteCascade
    resource   Resource
    etag       Text Maybe
    syncedAt   UTCTime

    UniqueSyncCursor accountId resource
    deriving Eq Show
|]

type SyncCursor :: Type

type SyncCursorId :: Type
