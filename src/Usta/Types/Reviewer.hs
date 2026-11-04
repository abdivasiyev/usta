-- | Who reviews a pull request, and where each of them stands.
module Usta.Types.Reviewer
  ( Reviewer (..)
  , Who (..)
  , Stance (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)

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
