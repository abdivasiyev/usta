-- | A repository's releases: a tag with notes and files to download.
module Usta.Types.Release
  ( Release (..)
  , Asset (..)
  ) where

import Data.Int (Int64)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

type Release :: Type
data Release = Release
  { name :: Text
  , tag :: Text
  , notes :: Text
    -- ^ Markdown.
  , author :: Text
  , published :: Maybe UTCTime
  , draft :: Bool
  , prerelease :: Bool
  , url :: Text
    -- ^ Its page on the forge.
  , assets :: [Asset]
  }
  deriving stock (Eq, Show)

type Asset :: Type
data Asset = Asset
  { name :: Text
  , size :: Int64
    -- ^ In bytes.
  , downloads :: Int
  , url :: Text
  }
  deriving stock (Eq, Show)
