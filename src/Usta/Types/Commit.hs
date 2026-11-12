{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Commits, as a repository's history gives them.
module Usta.Types.Commit
    ( Sha
    , Commit (..)
    )
where

import Data.Char (isHexDigit)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)
import Web.HttpApiData (ToHttpApiData)

import Data.Text qualified as Text

import Usta.Types.Parse (Parse (..), Parsed (..))

-- | A commit's full hash: 40 hex digits, or 64 in a SHA-256 repository.
type Sha :: Type
newtype Sha = Sha Text
    deriving stock (Eq, Ord, Show)
    deriving (ToHttpApiData) via Parsed Sha

instance Parse Sha where
    type Raw Sha = Text
    parse raw
        | Text.length hex `elem` [40, 64] && Text.all isHexDigit hex = Right (Sha hex)
        | otherwise = Left "a commit hash of 40 or 64 hex digits"
        where
            hex = Text.toLower (Text.strip raw)
    unparse (Sha hex) = hex

type Commit :: Type
data Commit = Commit
    { sha :: Sha
    , author :: Text
    , authoredAt :: UTCTime
    , subject :: Text
    , body :: Text
    -- ^ The message below the subject; empty when there is none.
    , login :: Maybe Text
    -- ^ The author's account on the forge, when the forge knows it.
    }
    deriving stock (Eq, Show)
