{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The @accounts@ table: one row per forge account usta is signed in to.
-- The token is not stored here; it lives in the system keyring.
module Usta.DB.Schema.Account
  ( Account (..)
  , AccountId
  , EntityField (..)
  , Unique (..)
  ) where

import Data.Kind (Type)
import Data.Time (UTCTime)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH

import Usta.Types.Account (AuthMethod)
import Usta.Types.Forge (Kind)
import Usta.Types.Host (Host)
import Usta.Types.Login (Login)

share
  [mkPersist sqlSettings {mpsFieldLabelModifier = \_ field -> field}]
  [persistLowerCase|
Account sql=accounts
    forge       Kind
    host        Host
    login       Login
    authMethod  AuthMethod
    createdAt   UTCTime

    UniqueAccount host login
    deriving Eq Show
|]

type Account :: Type

type AccountId :: Type
