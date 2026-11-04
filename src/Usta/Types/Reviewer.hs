-- | Who reviews a pull request, and where each of them stands.
module Usta.Types.Reviewer
  ( Reviewer (..)
  , Who (..)
  , Stance (..)
  , Candidate (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

import Usta.Types.Login (Login)

-- | A person, or a team of the repository's organization by name.
type Who :: Type
data Who = Person Login | Team Text
  deriving stock (Eq, Show)

type Stance :: Type
data Stance
  = Waiting
    -- ^ Asked, and not reviewed since.
  | Approved
  | ChangesRequested
  | Commented
  deriving stock (Eq, Show, Enum, Bounded)

type Reviewer :: Type
data Reviewer = Reviewer
  { who :: Who
  , stance :: Stance
  , stale :: Bool
    -- ^ The review was of an older commit than the latest.
  }
  deriving stock (Eq, Show)

-- | Someone who can be asked to review. A person who changed the pull
-- request's files before is suggested.
type Candidate :: Type
data Candidate = Candidate
  { who :: Who
  , about :: Text
    -- ^ A person's full name, a team's description; may be empty.
  , changed :: [Text]
    -- ^ The pull request's files they changed, by name, newest first.
  , lastChanged :: Maybe UTCTime
  , members :: Maybe Int
    -- ^ How many are in a team.
  }
  deriving stock (Eq, Show)
