{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Whether a repository has a local mirror, as the user chose: usta asks
-- before mirroring, and remembers a no so it does not ask again.
module Usta.Types.Mirror
  ( Transport (..)
  , Choice (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Git (Kind (..))
import Usta.Types.Parse (Parse (..), Parsed (..))

-- | How the mirror reaches the forge.
type Transport :: Type
data Transport
  = Https
    -- ^ With the account's token, through usta's credential helper.
  | Ssh
    -- ^ With the user's own ssh keys.
  deriving stock (Eq, Show, Enum, Bounded)

type Choice :: Type
data Choice
  = Declined
    -- ^ No mirror: diffs and history come from the forge's API.
  | Mirrored {kind :: Kind, transport :: Transport}
  deriving stock (Eq, Show)
  deriving (PersistField, PersistFieldSql) via Parsed Choice

-- | "declined", or the kind and transport: "full/https", "partial/ssh".
instance Parse Choice where
  type Raw Choice = Text
  parse raw = case raw of
    "declined" -> Right Declined
    "full/https" -> Right (Mirrored Full Https)
    "full/ssh" -> Right (Mirrored Full Ssh)
    "partial/https" -> Right (Mirrored Partial Https)
    "partial/ssh" -> Right (Mirrored Partial Ssh)
    _ -> Left "a mirror choice: declined, or full or partial over https or ssh"
  unparse choice = case choice of
    Declined -> "declined"
    Mirrored k t -> depth k <> "/" <> over t
    where
      depth k = case k of
        Full -> "full"
        Partial -> "partial"
      over t = case t of
        Https -> "https"
        Ssh -> "ssh"
