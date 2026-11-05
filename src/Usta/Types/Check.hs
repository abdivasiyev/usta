-- | The checks on a commit: CI jobs, and statuses other services post.
module Usta.Types.Check
  ( Check (..)
  , State (..)
  , Job (..)
  , Run (..)
  ) where

import Data.Int (Int64)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

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

-- | A run of the forge's own CI: one workflow on one commit, with its
-- jobs as checks.
type Run :: Type
data Run = Run
  { id :: Int64
    -- ^ The forge's own id of the run, to re-run or cancel it.
  , number :: Int
    -- ^ As the forge's pages number it.
  , workflow :: Text
    -- ^ Its file, e.g. "ci.yml".
  , title :: Text
    -- ^ The commit's or pull request's title.
  , branch :: Text
  , event :: Text
    -- ^ What started it, e.g. "push" or "pull_request".
  , sha :: Text
  , state :: State
  , started :: UTCTime
  , updated :: UTCTime
    -- ^ When its last job changed; how long it took, once it is done.
  , url :: Text
  , jobs :: [Check]
  }
  deriving stock (Eq, Show)
