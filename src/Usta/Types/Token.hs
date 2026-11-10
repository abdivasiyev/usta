{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | An access token for a forge account.
module Usta.Types.Token
    ( Token
    )
where

import Data.Char (isSpace)
import Data.Kind (Type)
import Data.Text (Text)

import Data.Text qualified as Text

import Usta.Types.Parse (Parse (..))

{- | An access token. It has no 'Show' and no JSON or database instances,
so it cannot end up in logs or the cache; it lives in the system keyring
and only the HTTP client reads it, through 'unparse'.
-}
type Token :: Type
newtype Token = Token Text
    deriving stock (Eq)

{- | Surrounding whitespace from a paste is dropped; a token with spaces
inside is rejected.
-}
instance Parse Token where
    type Raw Token = Text
    parse raw
        | Text.null token = Left "a token"
        | Text.any isSpace token = Left "a token without spaces"
        | otherwise = Right (Token token)
        where
            token = Text.strip raw
    unparse (Token token) = token
