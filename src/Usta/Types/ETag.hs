{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A forge's version tag for a list it sent.
module Usta.Types.ETag
  ( ETag
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import Usta.Types.Parse (Parse (..), Parsed (..))

-- | The @ETag@ header a forge sent with a list. Sent back as
-- @If-None-Match@, it lets the forge answer "not modified" instead of the
-- whole list. Opaque: kept exactly as sent, weak tags (@W/"…"@) included.
type ETag :: Type
newtype ETag = ETag Text
  deriving stock (Eq, Show)
  deriving (FromHttpApiData, PersistField, PersistFieldSql, ToHttpApiData) via Parsed ETag

instance Parse ETag where
  type Raw ETag = Text
  parse raw
    | Text.null raw = Left "an ETag header value"
    | otherwise = Right (ETag raw)
  unparse (ETag raw) = raw
