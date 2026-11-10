{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A pull request's or issue's number within its repository.
module Usta.Types.Number
    ( Number
    )
where

import Data.Aeson (FromJSON)
import Data.Kind (Type)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Web.HttpApiData (ToHttpApiData)

import Usta.Types.Parse (Parse (..), Parsed (..))

-- | A pull request's or issue's number within its repository.
type Number :: Type
newtype Number = Number Int
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, PersistField, PersistFieldSql, ToHttpApiData) via Parsed Number

instance Parse Number where
    type Raw Number = Int
    parse n
        | n > 0 = Right (Number n)
        | otherwise = Left "a positive number"
    unparse (Number n) = n
