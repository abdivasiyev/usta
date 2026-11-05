{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A user's name on a forge.
module Usta.Types.Login
  ( Login
  ) where

import Data.Aeson (FromJSON)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Web.HttpApiData (ToHttpApiData)

import Usta.Types.Parse (Parse (..), Parsed (..), isPathSegment)

-- | A user's name on a forge. GitHub's apps act as users named
-- @name[bot]@, such as @github-actions[bot]@.
type Login :: Type
newtype Login = Login Text
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, PersistField, PersistFieldSql, ToHttpApiData) via Parsed Login

instance Parse Login where
  type Raw Login = Text
  parse raw
    | isPathSegment (fromMaybe raw (Text.stripSuffix "[bot]" raw)) = Right (Login raw)
    | otherwise = Left "a user name of letters, digits, '.', '-' or '_'"
  unparse (Login raw) = raw
