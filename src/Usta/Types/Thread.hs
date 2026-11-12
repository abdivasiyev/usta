{- | Review conversations on a pull request's diff, as the forge keeps
them: each on one line of a file, with its notes oldest first.
-}
module Usta.Types.Thread
    ( Note (..)
    , Thread (..)
    , Threads (..)
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

import Usta.Types.Login (Login)

-- | One comment in a thread.
type Note :: Type
data Note = Note
    { author :: Login
    , body :: Text
    -- ^ Markdown, as written.
    , createdAt :: UTCTime
    }
    deriving stock (Eq, Show)

type Thread :: Type
data Thread = Thread
    { path :: Text
    , line :: Int
    -- ^ The line it was left on, in the file as it was then.
    , code :: [Text]
    {- ^ The diff lines it was left on, the commented one last, each with
    its "+", "-" or " ".
    -}
    , commit :: Text
    -- ^ The commit it was left on.
    , resolvedBy :: Maybe Login
    , notes :: [Note]
    , url :: Text
    -- ^ Its first comment on the forge's website.
    }
    deriving stock (Eq, Show)

{- | A pull request's threads, and when the user last submitted a review
of it, to tell what was said since.
-}
type Threads :: Type
data Threads = Threads
    { threads :: [Thread]
    , lastReviewed :: Maybe UTCTime
    }
    deriving stock (Eq, Show)
