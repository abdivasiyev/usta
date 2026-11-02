{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A label on an issue or pull request, as its repository defines it.
module Usta.Types.Label
  ( Color
  , Label (..)
  ) where

import Data.Aeson (FromJSON)
import Data.Char (isHexDigit)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Types.Parse (Parse (..), Parsed (..))

-- | A label's color: six hex digits, lowercase, without the @#@.
type Color :: Type
newtype Color = Color Text
  deriving stock (Eq, Show)
  deriving (FromJSON, PersistField, PersistFieldSql) via Parsed Color

-- | Takes it with or without a leading @#@; forges differ.
instance Parse Color where
  type Raw Color = Text
  parse raw
    | Text.length digits == 6 && Text.all isHexDigit digits = Right (Color (Text.toLower digits))
    | otherwise = Left "a color of six hex digits, such as d73a4a"
    where
      digits = fromMaybe raw (Text.stripPrefix "#" raw)
  unparse (Color digits) = digits

type Label :: Type
data Label = Label
  { name :: Text
  , color :: Color
  }
  deriving stock (Eq, Show)
