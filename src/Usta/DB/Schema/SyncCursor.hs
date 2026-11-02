{-# LANGUAGE DuplicateRecordFields #-}
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
  , EntityField (..)
  , Unique (..)
  ) where

import Data.Kind (Type)
import Data.Time (UTCTime)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH

import Usta.DB.Schema.Account (AccountId)
import Usta.Types.ETag (ETag)
import Usta.Types.Resource (Resource)

-- 'discoverEntities' passes the entities already in scope (the accounts
-- table), so the account reference becomes a foreign key.
share
  [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
  [persistLowerCase|
SyncCursor sql=sync_cursors
    accountId  AccountId OnDeleteCascade
    resource   Resource
    etag       ETag Maybe
    syncedAt   UTCTime

    UniqueSyncCursor accountId resource
    deriving Eq Show
|]

type SyncCursor :: Type

type SyncCursorId :: Type
