-- | What a search on a forge finds: pull requests, issues and
-- repositories, closed ones too.
module Usta.Types.Search
  ( Found (..)
  , Kind (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

type Kind :: Type
data Kind = PullRequest | Issue | Repository
  deriving stock (Eq, Show, Enum, Bounded)

type Found :: Type
data Found = Found
  { kind :: Kind
  , repo :: Text
    -- ^ e.g. "abdivasiyev/usta"; for a repository, itself.
  , number :: Maybe Int
  , title :: Text
    -- ^ For a repository, its description.
  , closed :: Bool
  , updated :: UTCTime
  }
  deriving stock (Eq, Show)
