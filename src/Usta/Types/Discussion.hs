-- | What people say on an issue or pull request, and what they can do to
-- one: comment, close, merge, review.
module Usta.Types.Discussion
  ( Comment (..)
  , Discussion (..)
  , Merge (..)
  , Verdict (..)
  ) where

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
  }
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

-- | What a review says.
type Verdict :: Type
data Verdict
  = Approve
  | RequestChanges
  | CommentOnly
    -- ^ Neither approves nor asks for changes.
  deriving stock (Eq, Show, Enum, Bounded)
