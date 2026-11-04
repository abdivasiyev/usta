-- | What a repository's Insights draw from: each commit's lines added and
-- removed, and its place in the history's graph.
module Usta.Types.Insights
  ( Change (..)
  , Node (..)
  , Blamed (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

-- | One commit's size.
type Change :: Type
data Change = Change
  { author :: Text
  , at :: UTCTime
  , added :: Int
  , removed :: Int
  }
  deriving stock (Eq, Show)

-- | A commit in the graph: its parents, first one first, and the branches
-- and tags that point at it.
type Node :: Type
data Node = Node
  { sha :: Text
  , parents :: [Text]
  , author :: Text
  , at :: UTCTime
  , subject :: Text
  , refs :: [Text]
    -- ^ e.g. "master", "origin/master", "tag: v0.3.0".
  }
  deriving stock (Eq, Show)

-- | A line of a file with the commit that last changed it.
type Blamed :: Type
data Blamed = Blamed
  { sha :: Text
  , author :: Text
  , at :: UTCTime
  , summary :: Text
  , line :: Text
  }
  deriving stock (Eq, Show)
