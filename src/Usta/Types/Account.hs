{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A forge account usta is signed in to.
module Usta.Types.Account
    ( AuthMethod (..)
    , Account (..)
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Types.Forge (Kind)
import Usta.Types.Host (Host)
import Usta.Types.Login (Login)
import Usta.Types.Parse (Parse (..), Parsed (..))

-- | How usta signed in to the account.
type AuthMethod :: Type
data AuthMethod = AccessToken | BrowserSignIn
    deriving stock (Bounded, Enum, Eq, Show)
    deriving (PersistField, PersistFieldSql) via Parsed AuthMethod

instance Parse AuthMethod where
    type Raw AuthMethod = Text
    parse raw = case raw of
        "token" -> Right AccessToken
        "browser" -> Right BrowserSignIn
        _ -> Left "a sign-in method: token or browser"
    unparse method = case method of
        AccessToken -> "token"
        BrowserSignIn -> "browser"

{- | One user on one server. The token is not here: it stays in the system
keyring, found by host and login.
-}
type Account :: Type
data Account = Account
    { forge :: Kind
    , host :: Host
    , login :: Login
    , authMethod :: AuthMethod
    }
    deriving stock (Eq, Show)
