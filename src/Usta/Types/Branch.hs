-- | A repository's branches and tags, as its picker lists them.
module Usta.Types.Branch
    ( Branch (..)
    , Tag (..)
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

type Branch :: Type
data Branch = Branch
    { name :: Text
    , pushed :: Maybe UTCTime
    {- ^ When its last commit was made; 'Nothing' when the forge does not
    say.
    -}
    , ahead :: Maybe Int
    , behind :: Maybe Int
    {- ^ Commits it has that the default branch does not, and the other
    way round; known only from a local mirror.
    -}
    }
    deriving stock (Eq, Show)

type Tag :: Type
data Tag = Tag
    { name :: Text
    , at :: Maybe UTCTime
    }
    deriving stock (Eq, Show)
