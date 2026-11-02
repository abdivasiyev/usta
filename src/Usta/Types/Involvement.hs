{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | How you are involved in a pull request or issue.
module Usta.Types.Involvement
  ( Involvement (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Types.Parse (Parse (..), Parsed (..))

-- | One way you are involved; a pull request or issue can involve you in
-- several. Decides which tab of the Pull requests and Issues pages shows it.
type Involvement :: Type
data Involvement
  = AsReviewer
    -- ^ Your review is requested.
  | AsAuthor
  | AsAssignee
  | AsMentioned
  deriving stock (Eq, Ord, Show, Enum, Bounded)
  deriving (PersistField, PersistFieldSql) via Parsed Involvement

instance Parse Involvement where
  type Raw Involvement = Text
  parse raw = case raw of
    "reviewer" -> Right AsReviewer
    "author" -> Right AsAuthor
    "assignee" -> Right AsAssignee
    "mentioned" -> Right AsMentioned
    _ -> Left "an involvement: reviewer, author, assignee or mentioned"
  unparse involvement = case involvement of
    AsReviewer -> "reviewer"
    AsAuthor -> "author"
    AsAssignee -> "assignee"
    AsMentioned -> "mentioned"
