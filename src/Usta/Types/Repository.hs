{-# LANGUAGE DerivingStrategies #-}

-- | A repository, as the Repositories page shows it.
module Usta.Types.Repository
  ( Repository (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Numeric.Natural (Natural)

import Usta.Types.Host (Host)
import Usta.Types.RepoName (RepoName)

-- | One repository you can see on a forge.
type Repository :: Type
data Repository = Repository
  { host :: Host
  , name :: RepoName
  , description :: Maybe Text
  , defaultBranch :: Text
  , private :: Bool
  , openPullRequests :: Natural
  , openIssues :: Natural
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show)
