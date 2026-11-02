{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | The @issue_labels@ table: the labels on an issue, one row each.
-- Deleted with the issue.
module Usta.DB.Schema.IssueLabel
  ( IssueLabel (..)
  , IssueLabelId
  , EntityField (..)
  , Unique (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH

import Usta.DB.Schema.Issue (IssueId)
import Usta.Types.Label (Color)

-- 'discoverEntities' passes the entities already in scope (the issues
-- table), so the reference becomes a foreign key.
share
  [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
  [persistLowerCase|
IssueLabel sql=issue_labels
    issueId  IssueId OnDeleteCascade
    name     Text
    color    Color

    UniqueIssueLabel issueId name
    deriving Eq Show
|]

type IssueLabel :: Type

type IssueLabelId :: Type
