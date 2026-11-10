{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | How many of something: comments, changed files, commits, open issues.
module Usta.Types.Count
    ( Count
    )
where

import Data.Aeson (FromJSON)
import Data.Kind (Type)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Types.Parse (Parse (..), Parsed (..))

{- | A count: zero or more. Parsed from the forge's JSON and from the
column alike, so a negative one is refused wherever it comes from.
-}
type Count :: Type
newtype Count = Count Int
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, PersistField, PersistFieldSql) via Parsed Count

instance Parse Count where
    type Raw Count = Int
    parse n
        | n >= 0 = Right (Count n)
        | otherwise = Left "a count of zero or more"
    unparse (Count n) = n
