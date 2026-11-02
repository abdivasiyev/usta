{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The forge's own identifier for a notification thread.
module Usta.Types.ThreadId
  ( ThreadId
  ) where

import Data.Aeson (FromJSON)
import Data.Char (isDigit)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Web.HttpApiData (ToHttpApiData)

import Usta.Types.Parse (Parse (..), Parsed (..))

-- | A notification thread's id on its forge: what sync matches on to update
-- a notification rather than add it twice, and what marking it read sends
-- back. GitHub sends it as a string of digits and Forgejo as a number; both
-- are kept as the digits.
type ThreadId :: Type
newtype ThreadId = ThreadId Text
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, PersistField, PersistFieldSql, ToHttpApiData) via Parsed ThreadId

instance Parse ThreadId where
  type Raw ThreadId = Text
  parse raw
    | not (Text.null raw) && Text.all isDigit raw = Right (ThreadId raw)
    | otherwise = Left "a notification id of digits"
  unparse (ThreadId raw) = raw
