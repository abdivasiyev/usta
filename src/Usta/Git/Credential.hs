{-# LANGUAGE OverloadedStrings #-}

-- | usta as git's credential helper, for mirrors over https: git runs
-- @usta git-credential get@ and describes the request on stdin, and usta
-- answers with the account's token from the keyring. The token goes from
-- the keyring to git through a pipe; it is never on a command line, in an
-- environment variable or in a file.
--
-- Only @get@ for https with a username is answered. @store@ and @erase@
-- do nothing, so git never keeps the token anywhere.
module Usta.Git.Credential
  ( Request (..)
  , parseRequest
  , respond
  , helper
  ) where

import qualified Data.ByteString.Char8 as B8
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.IO (stdin, stdout)

import Usta.Keyring (Keyring (..))
import Usta.Types.Host (Host)
import Usta.Types.Login (Login)
import Usta.Types.Parse (parse, unparse)
import Usta.Types.Token (Token)

-- | What git asks for: the account on a host.
type Request :: Type
data Request = Request
  { host :: Host
  , login :: Login
  }
  deriving stock (Eq, Show)

-- | git's "key=value" lines; 'Nothing' unless they ask over https and
-- name a user.
parseRequest :: Text -> Maybe Request
parseRequest input = do
  protocol <- field "protocol"
  if protocol /= "https" then Nothing else do
    host <- field "host" >>= either (const Nothing) Just . parse
    login <- field "username" >>= either (const Nothing) Just . parse
    pure Request {host = host, login = login}
  where
    pairs = [(k, T.drop 1 v) | line <- T.lines input, let (k, v) = T.breakOn "=" line, not (T.null v)]
    field key = lookup key pairs

-- | The answer to @get@: the user and the token, or nothing when usta has
-- no token for them. @current@ turns what the keyring keeps into the
-- token to send, as a browser sign-in keeps a refresh token.
respond :: Keyring -> (Host -> Token -> IO (Maybe Token)) -> Request -> IO Text
respond keyring current request = do
  found <- keyring.load request.host request.login
  sent <- maybe (pure Nothing) (current request.host) found
  pure $ case sent of
    Nothing -> ""
    Just token -> "username=" <> unparse request.login <> "\npassword=" <> unparse token <> "\n"

-- | Runs the helper for git's @action@ ("get", "store" or "erase").
helper :: Keyring -> (Host -> Token -> IO (Maybe Token)) -> String -> IO ()
helper keyring current action = do
  input <- decodeUtf8Lenient <$> B8.hGetContents stdin
  case (action, parseRequest input) of
    ("get", Just request) -> respond keyring current request >>= B8.hPutStr stdout . encodeUtf8
    _ -> pure ()
