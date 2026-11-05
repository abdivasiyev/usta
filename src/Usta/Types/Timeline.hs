-- | What happened on an issue or pull request besides its comments: the
-- rows between them in the conversation.
module Usta.Types.Timeline
  ( Event (..)
  , Happening (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

import Usta.Types.Discussion (Verdict)
import Usta.Types.Label (Label)
import Usta.Types.Login (Login)
import Usta.Types.Number (Number)

type Event :: Type
data Event = Event
  { actor :: Login
  , at :: UTCTime
  , what :: Happening
  }
  deriving stock (Eq, Show)

type Happening :: Type
data Happening
  = Labeled Bool Label
    -- ^ Added ('True') or removed.
  | Assigned Bool Login
  | ReviewRequested Bool Login
  | Milestoned (Maybe Text) (Maybe Text)
    -- ^ From the old milestone to the new one, by title.
  | Renamed Text Text
    -- ^ From the old title to the new one.
  | Closed
  | Reopened
  | Merged
  | Referenced Number Text
    -- ^ Mentioned from another issue or pull request, with its title.
  | Pushed Int
    -- ^ How many commits.
  | MergeScheduled Bool
    -- ^ Set to merge when the checks pass, or no longer.
  | BranchDeleted
  | Reviewed Verdict
    -- ^ A review with no text of its own: an approval or a change request.
  deriving stock (Eq, Show)
