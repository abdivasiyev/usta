{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | What names things on a forge: which forge, which server, which
-- repository, which user, which pull request or issue. Each is parsed from
-- its raw form once, through 'Parse', and exported abstractly.
module Usta.Core.Id
  ( Kind (..)
  , Host
  , RepoName
  , owner
  , shortName
  , Login
  , Number
  , Token
  , Ref (..)
  ) where

import Data.Aeson (FromJSON)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Text.Read (readMaybe)
import Web.HttpApiData (ToHttpApiData)

import Usta.Core.Parse (Parse (..), Parsed (..))

-- | Which forge software a server runs; decides how usta talks to it.
type Kind :: Type
data Kind = GitHub | GitLab | Forgejo
  deriving stock (Eq, Show, Enum, Bounded)
  deriving (FromJSON, PersistField, PersistFieldSql) via Parsed Kind

instance Parse Kind where
  type Raw Kind = Text
  parse raw = case raw of
    "github" -> Right GitHub
    "gitlab" -> Right GitLab
    "forgejo" -> Right Forgejo
    _ -> Left "a forge: github, gitlab or forgejo"
  unparse kind = case kind of
    GitHub -> "github"
    GitLab -> "gitlab"
    Forgejo -> "forgejo"

-- | A server's host name, lowercase, with a port when it has one:
-- @github.com@, @git.example.com:3000@. No scheme and no path.
type Host :: Type
newtype Host = Host Text
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, PersistField, PersistFieldSql) via Parsed Host

instance Parse Host where
  type Raw Host = Text
  parse raw
    | "://" `Text.isInfixOf` raw = Left "a host name without the scheme, such as github.com"
    | validHost lowered = Right (Host lowered)
    | otherwise = Left "a host name such as github.com or git.example.com:3000"
    where
      lowered = Text.toLower raw
  unparse (Host raw) = raw

validHost :: Text -> Bool
validHost text = case Text.splitOn ":" text of
  [name] -> validName name
  [name, port] -> validName name && validPort port
  _ -> False
  where
    validName name = Text.length name <= 253 && all validLabel (Text.splitOn "." name)
    validLabel label =
      not (Text.null label)
        && Text.length label <= 63
        && Text.all (\c -> isAsciiLower c || isDigit c || c == '-') label
        && not ("-" `Text.isPrefixOf` label)
        && not ("-" `Text.isSuffixOf` label)
    validPort port =
      Text.all isDigit port && maybe False (\p -> p >= 1 && p <= (65535 :: Int)) (readMaybe (Text.unpack port))

-- | A repository's full name, @owner/name@, as the forge spells it.
type RepoName :: Type
data RepoName = RepoName Text Text
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, PersistField, PersistFieldSql) via Parsed RepoName

instance Parse RepoName where
  type Raw RepoName = Text
  parse raw = case Text.splitOn "/" raw of
    [o, n] | validSegment o && validSegment n -> Right (RepoName o n)
    _ -> Left "a repository as owner/name, such as abdivasiyev/usta"
  unparse (RepoName o n) = o <> "/" <> n

-- | The user or organization that owns the repository.
owner :: RepoName -> Text
owner (RepoName o _) = o

-- | The name without the owner, as lists show it: @usta@.
shortName :: RepoName -> Text
shortName (RepoName _ n) = n

-- | A user's name on a forge.
type Login :: Type
newtype Login = Login Text
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, PersistField, PersistFieldSql, ToHttpApiData) via Parsed Login

instance Parse Login where
  type Raw Login = Text
  parse raw
    | validSegment raw = Right (Login raw)
    | otherwise = Left "a user name of letters, digits, '.', '-' or '_'"
  unparse (Login raw) = raw

-- | Owners, repository names and logins: what forges allow in a path
-- segment.
validSegment :: Text -> Bool
validSegment segment =
  not (Text.null segment)
    && segment /= "."
    && segment /= ".."
    && Text.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("._-" :: String)) segment

-- | A pull request's or issue's number within its repository.
type Number :: Type
newtype Number = Number Int
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, PersistField, PersistFieldSql, ToHttpApiData) via Parsed Number

instance Parse Number where
  type Raw Number = Int
  parse n
    | n > 0 = Right (Number n)
    | otherwise = Left "a positive number"
  unparse (Number n) = n

-- | An access token. It has no 'Show' and no JSON or database instances,
-- so it cannot end up in logs or the cache; it lives in the system keyring
-- and only the HTTP client reads it, through 'unparse'.
type Token :: Type
newtype Token = Token Text
  deriving stock (Eq)

-- | Surrounding whitespace from a paste is dropped; a token with spaces
-- inside is rejected.
instance Parse Token where
  type Raw Token = Text
  parse raw
    | Text.null token = Left "a token"
    | Text.any isSpace token = Left "a token without spaces"
    | otherwise = Right (Token token)
    where
      token = Text.strip raw
  unparse (Token token) = token

-- | One pull request or issue, wherever it lives.
type Ref :: Type
data Ref = Ref
  { host :: Host
  , repo :: RepoName
  , number :: Number
  }
  deriving stock (Eq, Ord, Show)
