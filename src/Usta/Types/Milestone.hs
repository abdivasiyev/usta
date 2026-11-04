-- | A repository's milestones: a goal issues and pull requests count
-- toward, such as a release.
module Usta.Types.Milestone
  ( Milestone (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

type Milestone :: Type
data Milestone = Milestone
  { title :: Text
  , open :: Bool
  , due :: Maybe UTCTime
  , closedAt :: Maybe UTCTime
  , openItems :: Int
    -- ^ Issues and pull requests in it still open.
  , closedItems :: Int
  }
  deriving stock (Eq, Show)
