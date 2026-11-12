{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Signing in with the browser, as OAuth2 for native apps (RFC 8252):
usta opens the forge's authorize page and listens on 127.0.0.1 for the
answer, with PKCE in place of a client secret. The forge needs an
OAuth2 application, public, with the redirect URI
@http://127.0.0.1/callback@; its client ID is all usta asks for.

The keyring keeps the refresh token with the client ID, and the access
token, which lasts about an hour, lives only in memory.
-}
module Usta.OAuth
    ( Tokens (..)
    , Waiting (..)
    , begin
    , refresh
    , stored
    , storing
    )
where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:), (.:?))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Network.HTTP.Types (urlDecode, urlEncode)
import Numeric (readHex)

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.GObject qualified as GObject
import GI.Gio qualified as Gio
import Network.HTTP.Client qualified as HTTP

import Usta.Types.Parse (parse, unparse)
import Usta.Types.Token (Token)

type Tokens :: Type
data Tokens = Tokens
    { access :: Token
    , refreshToken :: Token
    , seconds :: Int
    -- ^ How long the access token lasts.
    }

instance FromJSON Tokens where
    parseJSON = withObject "Tokens" $ \o -> do
        a <- o .: "access_token"
        r <- o .: "refresh_token"
        s <- o .:? "expires_in"
        either fail pure $ Tokens <$> first' (parse a) <*> first' (parse r) <*> pure (fromMaybe 3600 s)
        where
            first' = either (Left . T.unpack) Right

{- | A sign-in on its way: the page to open, and the wait for the forge's
answer, to run off the main thread.
-}
type Waiting :: Type
data Waiting = Waiting
    { page :: Text
    , answer :: IO (Either Text Tokens)
    , stop :: IO ()
    }

{- | Starts listening and gives the authorize page of @site@ (such as
"https://git.azizovich.uz") for @client@. Call it on the main thread:
the answer arrives through the main loop.
-}
begin :: HTTP.Manager -> Text -> Text -> IO (Either Text Waiting)
begin manager site client = do
    verifier <- (\a b -> T.filter (/= '-') (a <> b)) <$> GLib.uuidStringRandom <*> GLib.uuidStringRandom
    state <- T.filter (/= '-') <$> GLib.uuidStringRandom
    challenge <- s256 verifier
    service <- Gio.socketServiceNew
    loopback <- Gio.inetAddressNewLoopback Gio.SocketFamilyIpv4
    wanted <- Gio.inetSocketAddressNew loopback 0
    bound <- try (Gio.socketListenerAddAddress service wanted Gio.SocketTypeStream Gio.SocketProtocolTcp (Nothing :: Maybe GObject.Object))
    case bound of
        Left (e :: SomeException) -> pure (Left ("usta could not listen for the forge's answer: " <> T.pack (show e)))
        Right effective -> do
            port <- GI.unsafeCastTo Gio.InetSocketAddress effective >>= Gio.inetSocketAddressGetPort
            let
                redirect = "http://127.0.0.1:" <> T.pack (show port) <> "/callback"
            result <- newEmptyMVar
            void $ GI.on service #incoming $ \connection _ -> do
                input <- Gio.iOStreamGetInputStream connection
                bytes <- Gio.inputStreamReadBytes input 8192 (Nothing :: Maybe Gio.Cancellable) >>= GLib.bytesGetData
                let
                    query = queryOf (fromMaybe "" bytes)
                    said = case (lookup "state" query, lookup "code" query, lookup "error" query) of
                        (Just s, Just code, _) | s == state -> Right code
                        (_, _, Just why) -> Left ("The forge said: " <> why)
                        _ -> Left "The forge's answer did not match this sign-in."
                output <- Gio.iOStreamGetOutputStream connection
                let
                    body = either (\why -> "usta was not signed in: " <> why) (const "Signed in. You can close this tab and go back to usta.") said
                    reply = "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\nConnection: close\r\n\r\n" <> encodeUtf8 body
                void (try (fromIntegral <$> Gio.outputStreamWriteAll output reply (Nothing :: Maybe Gio.Cancellable)) :: IO (Either SomeException Word))
                void (Gio.iOStreamClose connection (Nothing :: Maybe Gio.Cancellable))
                -- A browser may ask for /favicon.ico first; only the callback counts.
                if isCallback (fromMaybe "" bytes) then void (tryPutMVar result said) else pure ()
                pure True
            Gio.socketServiceStart service
            -- Stopping cancels the wait for a connection on the main loop's next
            -- turn; the socket closes after it, or GLib polls a closed one.
            let
                stopping = do
                    Gio.socketServiceStop service
                    void (GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (Gio.socketListenerClose service >> pure False))
                page =
                    site
                        <> "/login/oauth/authorize?"
                        <> form
                            [ ("client_id", client)
                            , ("redirect_uri", redirect)
                            , ("response_type", "code")
                            , ("state", state)
                            , ("code_challenge", challenge)
                            , ("code_challenge_method", "S256")
                            ]
                answer = do
                    code <- takeMVar result
                    case code of
                        Left why -> pure (Left why)
                        Right c ->
                            post
                                manager
                                site
                                [ ("grant_type", "authorization_code")
                                , ("client_id", client)
                                , ("code", c)
                                , ("redirect_uri", redirect)
                                , ("code_verifier", verifier)
                                ]
            pure (Right Waiting {page = page, answer = answer, stop = stopping})

-- | A new access token for the refresh token kept with @client@.
refresh :: HTTP.Manager -> Text -> Text -> Token -> IO (Either Text Tokens)
refresh manager site client token =
    post manager site [("grant_type", "refresh_token"), ("client_id", client), ("refresh_token", unparse token)]

post :: HTTP.Manager -> Text -> [(Text, Text)] -> IO (Either Text Tokens)
post manager site fields = do
    asked <- try $ do
        request <- HTTP.parseRequest (T.unpack (site <> "/login/oauth/access_token"))
        let
            body =
                HTTP.urlEncodedBody
                    [(encodeUtf8 k, encodeUtf8 v) | (k, v) <- fields]
                    request {HTTP.requestHeaders = [("Accept", "application/json")]}
        HTTP.httpLbs body manager
    pure $ case asked of
        Left (e :: HTTP.HttpException) -> Left ("The forge could not be reached: " <> T.pack (takeWhile (/= '\n') (show e)))
        Right response -> case eitherDecode (HTTP.responseBody response) of
            Right tokens -> Right tokens
            Left _ -> Left "The forge did not give a token."

{- | The keyring's form, "oauth:<client>:<refresh token>"; a token pasted
by hand is never in it.
-}
stored :: Token -> Maybe (Text, Token)
stored token = case T.splitOn ":" (unparse token) of
    ["oauth", client, r] -> either (const Nothing) (Just . (,) client) (parse r)
    _ -> Nothing

storing :: Text -> Token -> Token
storing client r = either (const r) id (parse ("oauth:" <> client <> ":" <> unparse r))

-- | base64url of the SHA-256, without padding.
s256 :: Text -> IO Text
s256 verifier = do
    hex <- GLib.computeChecksumForString GLib.ChecksumTypeSha256 verifier (-1)
    let
        raw = B.pack [fromIntegral n | pair <- chunks (maybe "" T.unpack hex), (n, _) <- take 1 (readHex pair :: [(Int, String)])]
    encoded <- GLib.base64Encode (Just raw)
    pure (T.dropWhileEnd (== '=') (T.map (\c -> case c of '+' -> '-'; '/' -> '_'; _ -> c) encoded))
    where
        chunks (a : b : rest) = [a, b] : chunks rest
        chunks _ = []

-- | "GET /callback?code=…&state=… HTTP/1.1" to its query's pairs.
queryOf :: B.ByteString -> [(Text, Text)]
queryOf request =
    [ (decode k, decode (B.drop 1 v))
    | pair <- B8.split '&' (B8.drop 1 (B8.dropWhile (/= '?') target))
    , let
        (k, v) = B8.break (== '=') pair
    ]
    where
        target = B8.takeWhile (/= ' ') (B8.drop 1 (B8.dropWhile (/= ' ') request))
        decode = decodeUtf8Lenient . urlDecode True

isCallback :: B.ByteString -> Bool
isCallback = B8.isPrefixOf "GET /callback"

form :: [(Text, Text)] -> Text
form pairs = T.intercalate "&" [k <> "=" <> decodeUtf8Lenient (urlEncode True (encodeUtf8 v)) | (k, v) <- pairs]
