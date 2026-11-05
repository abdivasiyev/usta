{-# LANGUAGE OverloadedStrings #-}

-- | What every forge adapter shares: one servant client environment per
-- account, and the forge's errors as a 'Failure'.
module Usta.Forge.Http
  ( Client
  , connect
  , apiUrl
  , run
  , runChanged
  , runDone
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Servant.Client
  ( BaseUrl (..)
  , ClientEnv (..)
  , ClientError (..)
  , ClientM
  , ResponseF (..)
  , Scheme (Https)
  , defaultMakeClientRequest
  , mkClientEnv
  , runClientM
  )
import Text.Read (readMaybe)

import Usta.Forge (Failure (..))
import Usta.Types.Host (Host)
import Usta.Types.Parse (unparse)

-- | Talks to one forge API as one account.
type Client :: Type
newtype Client = Client ClientEnv

-- | A client for the API at @base@ that sends @authorization@ (the whole
-- header value, e.g. @token …@) with every request, read for each one, as
-- a browser sign-in's token changes. The value is set on the request
-- only; it is not part of any error, and a redirect (GitHub's to a job
-- log's storage) does not carry it to another server.
connect :: HTTP.Manager -> BaseUrl -> IO ByteString -> Client
connect manager base current = Client (mkClientEnv manager base)
  { makeClientRequest = \url request -> do
      built <- defaultMakeClientRequest url request
      authorization <- current
      pure built
        { HTTP.requestHeaders =
            ("Authorization", authorization) : ("User-Agent", "usta") : HTTP.requestHeaders built
        , HTTP.shouldStripHeaderOnRedirect = (== "Authorization")
        }
  }

-- | @https://host/path@; a port in the host is kept.
apiUrl :: Host -> String -> BaseUrl
apiUrl host path = BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost = T.unpack name
  , baseUrlPort = fromMaybe 443 (readMaybe (T.unpack (T.drop 1 port)))
  , baseUrlPath = path
  }
  where
    (name, port) = T.breakOn ":" (unparse host)

-- | Runs a call, throwing 'Failure' when it fails.
run :: Client -> ClientM a -> IO a
run (Client env) call = runClientM call env >>= either (throwIO . failure) pure

-- | A conditional call: 'Nothing' when the forge answers 304, not
-- modified, to an @If-None-Match@ or @If-Modified-Since@.
runChanged :: Client -> ClientM a -> IO (Maybe a)
runChanged (Client env) call = runClientM call env >>= \case
  Right a -> pure (Just a)
  Left (FailureResponse _ response) | statusCode response.responseStatusCode == 304 -> pure Nothing
  Left e -> throwIO (failure e)

-- | A call whose answer usta does not read: any 2xx is done, though the
-- route names one status (GitHub answers a reaction that was already
-- there with 200, a new one with 201).
runDone :: Client -> ClientM a -> IO ()
runDone (Client env) call = runClientM call env >>= \case
  Right _ -> pure ()
  Left (FailureResponse _ response) | code < 300, code >= 200 -> pure ()
    where code = statusCode response.responseStatusCode
  Left e -> throwIO (failure e)

-- | Bodies are left out on purpose: they can echo what was sent.
failure :: ClientError -> Failure
failure e = case e of
  ConnectionError _ -> Offline
  FailureResponse _ response
    | code == 401 -> SignedOut
    | otherwise -> Refused ("the forge answered " <> T.pack (show code))
    where
      code = statusCode response.responseStatusCode
  DecodeFailure _ _ -> unreadable
  UnsupportedContentType _ _ -> unreadable
  InvalidContentTypeHeader _ -> unreadable
  where
    unreadable = Refused "the forge sent an answer usta cannot read"
