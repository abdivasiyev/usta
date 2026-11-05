-- | What people say on an issue or pull request, and what they can do to
-- one: comment, close, merge, review.
module Usta.Types.Discussion
  ( Comment (..)
  , Reaction (..)
  , Feeling (..)
  , Watching (..)
  , Discussion (..)
  , Merge (..)
  , Verdict (..)
  , NewIssue (..)
  , NewPull (..)
  ) where

import Data.Int (Int64)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

import Usta.Types.Label (Label)
import Usta.Types.Login (Login)

type Comment :: Type
data Comment = Comment
  { author :: Login
  , body :: Text
    -- ^ Markdown, as written.
  , createdAt :: UTCTime
  , id :: Maybe Int64
    -- ^ The forge's id of a comment; 'Nothing' for the description.
  , edited :: Bool
  , url :: Text
    -- ^ Its page on the forge, to link to.
  , reactions :: [Reaction]
  , review :: Maybe Verdict
    -- ^ A pull request review's text, with how it voted; such a comment is
    -- edited on the forge's page, not here.
  }
  deriving stock (Eq, Show)

-- | How much of an issue or pull request reaches the user's inbox.
type Watching :: Type
data Watching
  = Subscribed
    -- ^ Every comment and change.
  | Participating
    -- ^ Only when mentioned or taking part.
  | Muted
    -- ^ Nothing, ever.
  deriving stock (Eq, Show, Enum, Bounded)

-- | How a reaction feels: the eight forges share.
type Feeling :: Type
data Feeling = ThumbsUp | ThumbsDown | Laugh | Hooray | Confused | Heart | Rocket | Eyes
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | How many reacted with one feeling, and whether the user is one of them.
type Reaction :: Type
data Reaction = Reaction {feeling :: Feeling, count :: Int, mine :: Bool}
  deriving stock (Eq, Show)

-- | An issue or pull request as the forge has it now. Its description is
-- the opening comment.
type Discussion :: Type
data Discussion = Discussion
  { opening :: Comment
  , comments :: [Comment]
    -- ^ Oldest first.
  , closed :: Bool
    -- ^ Merged pull requests are closed too.
  , merged :: Bool
  , assignees :: [Login]
  , labels :: [Label]
  , milestone :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | How a pull request goes into its base branch.
type Merge :: Type
data Merge
  = Squash
  | MergeCommit
  | Rebase
  deriving stock (Eq, Show, Enum, Bounded)

-- | An issue to file, as the form fills it in.
type NewIssue :: Type
data NewIssue = NewIssue
  { title :: Text
  , body :: Text
  , labels :: [Text]
    -- ^ By name; the repository must have them.
  , assignees :: [Login]
  }
  deriving stock (Eq, Show)

-- | A pull request to open.
type NewPull :: Type
data NewPull = NewPull
  { title :: Text
  , body :: Text
  , from :: Text
    -- ^ The branch with the changes.
  , into :: Text
  , labels :: [Text]
  , reviewers :: [Login]
  , draft :: Bool
  }
  deriving stock (Eq, Show)

-- | What a review says.
type Verdict :: Type
data Verdict
  = Approve
  | RequestChanges
  | CommentOnly
    -- ^ Neither approves nor asks for changes.
  deriving stock (Eq, Show, Enum, Bounded)
