-- | The checks on a commit: CI jobs, and statuses other services post.
module Usta.Types.Check
  ( Check (..)
  , State (..)
  , Job (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)

type State :: Type
data State = Waiting | Running | Passed | Failed | Skipped
  deriving stock (Eq, Show, Enum, Bounded)

-- | A job of the forge's own CI, whose log the forge gives: its run, and
-- where it is in the run.
type Job :: Type
data Job = Job { run :: Int, index :: Int }
  deriving stock (Eq, Show)

type Check :: Type
data Check = Check
  { name :: Text
    -- ^ e.g. "test (pull_request)".
  , state :: State
  , description :: Text
    -- ^ What the check says of itself, e.g. "Failing after 48s".
  , url :: Maybe Text
    -- ^ Its page, absolute.
  , job :: Maybe Job
    -- ^ 'Nothing' for a status another service posted.
  }
  deriving stock (Eq, Show)
