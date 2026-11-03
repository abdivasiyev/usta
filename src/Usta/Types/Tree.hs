-- | What a branch holds: its folders and files, by path.
module Usta.Types.Tree
  ( Kind (..)
  , Entry (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)

type Kind :: Type
data Kind = Folder | File
  deriving stock (Eq, Show)

type Entry :: Type
data Entry = Entry
  { path :: Text
    -- ^ From the top, with "/" between folders, e.g. "src/Usta/Git.hs".
  , kind :: Kind
  , size :: Maybe Int
    -- ^ Bytes, for files.
  }
  deriving stock (Eq, Show)
