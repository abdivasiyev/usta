-- | An issue, as the Issues page and the issue screen use it.
module Usta.Types.Issue
    ( Issue (..)
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

import Usta.Types.Count (Count)
import Usta.Types.Involvement (Involvement)
import Usta.Types.Label (Label)
import Usta.Types.Login (Login)
import Usta.Types.Ref (Ref)

-- | One issue. It is open until 'closedAt' says when it was closed.
type Issue :: Type
data Issue = Issue
    { ref :: Ref
    , title :: Text
    , author :: Login
    , labels :: [Label]
    , comments :: Count
    , involvement :: [Involvement]
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    , closedAt :: Maybe UTCTime
    }
    deriving stock (Eq, Show)
