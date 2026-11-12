{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A label on an issue or pull request, as its repository defines it.
module Usta.Types.Label
    ( Color
    , Label (..)
    , Definition (..)
    )
where

import Data.Aeson (FromJSON)
import Data.Char (isHexDigit)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Data.Text qualified as Text

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

-- | A label as the repository defines it, to choose from or create.
type Definition :: Type
data Definition = Definition
    { name :: Text
    , color :: Color
    , description :: Text
    -- ^ May be empty.
    }
    deriving stock (Eq, Show)
