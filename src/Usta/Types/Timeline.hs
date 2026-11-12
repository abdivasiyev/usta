{- | What happened on an issue or pull request besides its comments: the
rows between them in the conversation.
-}
module Usta.Types.Timeline
    ( Event (..)
    , Happening (..)
    )
where

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
    = -- | Added ('True') or removed.
      Labeled Bool Label
    | Assigned Bool Login
    | ReviewRequested Bool Login
    | -- | From the old milestone to the new one, by title.
      Milestoned (Maybe Text) (Maybe Text)
    | -- | From the old title to the new one.
      Renamed Text Text
    | Closed
    | Reopened
    | Merged
    | -- | Mentioned from another issue or pull request, with its title.
      Referenced Number Text
    | -- | How many commits.
      Pushed Int
    | -- | Set to merge when the checks pass, or no longer.
      MergeScheduled Bool
    | BranchDeleted
    | -- | A review with no text of its own: an approval or a change request.
      Reviewed Verdict
    deriving stock (Eq, Show)
