{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The @issue_involvements@ table: how the account an issue was synced for
-- is involved in it, one row per way. Deleted with the issue.
module Usta.DB.Schema.IssueInvolvement
  ( IssueInvolvement (..)
  , IssueInvolvementId
  , EntityField (..)
  , Unique (..)
  ) where

import Data.Kind (Type)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH

import Usta.DB.Schema.Issue (IssueId)
import Usta.Types.Involvement (Involvement)

-- 'discoverEntities' passes the entities already in scope (the issues
-- table), so the reference becomes a foreign key.
share
  [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
  [persistLowerCase|
IssueInvolvement sql=issue_involvements
    issueId      IssueId OnDeleteCascade
    involvement  Involvement

    UniqueIssueInvolvement issueId involvement
    deriving Eq Show
|]

type IssueInvolvement :: Type

type IssueInvolvementId :: Type
