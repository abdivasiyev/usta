-- | What stands between a pull request and its merge, besides its checks:
-- the merge box shows only what applies.
module Usta.Types.Readiness
  ( Readiness (..)
  , UpdateStyle (..)
  ) where

import Data.Kind (Type)

type Readiness :: Type
data Readiness = Readiness
  { draft :: Bool
    -- ^ Reviewers are not asked until it is ready.
  , behind :: Int
    -- ^ Commits on the base branch the pull request does not have.
  , conflicts :: Bool
    -- ^ The forge cannot merge it as it is.
  , requiredApprovals :: Int
    -- ^ How many approvals the base branch's protection asks for; 0 when
    -- it asks none, or usta may not read it.
  }
  deriving stock (Eq, Show)

-- | How the base branch's newer commits come into the pull request.
type UpdateStyle :: Type
data UpdateStyle = ByMerge | ByRebase
  deriving stock (Eq, Show, Enum, Bounded)
