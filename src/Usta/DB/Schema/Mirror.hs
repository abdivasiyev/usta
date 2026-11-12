{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The @mirrors@ table: for each account's repository, whether the user
chose a local mirror, and where it fetches from. Deleted with the
account; the mirror's files are not.
-}
module Usta.DB.Schema.Mirror
    ( Mirror (..)
    , MirrorId
    , EntityField (..)
    , Unique (..)
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH

import Usta.DB.Schema.Account (AccountId)
import Usta.Types.Mirror (Choice)
import Usta.Types.RepoName (RepoName)

-- 'discoverEntities' passes the entities already in scope (the accounts
-- table), so the account reference becomes a foreign key.
share
    [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
    [persistLowerCase|
Mirror sql=mirrors
    accountId  AccountId OnDeleteCascade
    repo       RepoName
    choice     Choice
    url        Text Maybe
    fetchedAt  UTCTime Maybe
    decidedAt  UTCTime

    UniqueMirror accountId repo
    deriving Eq Show
|]

type Mirror :: Type

type MirrorId :: Type
