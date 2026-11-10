{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A forge's answers, recorded once from the real forge and served back
to usta's own forge code: a contract test runs each operation against
them, so a change in what a forge sends, or in what usta asks, shows.

Recording (USTA_RECORD=forgejo or github) runs the same tests through
a proxy to the forge and writes test/contract/<forge>.json. Forgejo is
the local test server (CONTRIBUTING.md); GitHub is asked through the
gh command, so no token passes through the tests, and only for public
data: searches ask about abdivasiyev/usta instead, private repositories
are cut out, and notifications and organisations are made up.
-}
module Usta.Contract.Replay
    ( Exchange (..)
    , Recorded
    , Server (..)
    , start
    , Written (..)
    , standIn
    , recording
    )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, eitherDecodeFileStrict, encode, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status (..), status404, statusCode)
import Network.Socket (close)
import Servant.Client (BaseUrl (..), Scheme (Http))
import System.Environment (lookupEnv)
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)

import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Text qualified as T
import Data.Vector qualified as V
import Network.HTTP.Client qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

-- | One request and the forge's answer to it.
data Exchange = Exchange
    { request :: Text
    -- ^ The method, path and query, the query sorted and without times.
    , body :: Maybe Value
    -- ^ What was sent, with search words left out.
    , status :: Int
    , headers :: [(Text, Text)]
    , answer :: Text
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

type Recorded :: Type
type Recorded = MVar [Exchange]

-- | Which forge is being recorded, if one is.
recording :: IO (Maybe String)
recording = lookupEnv "USTA_RECORD"

{- | A server for one forge's tests: the recorded answers, or a proxy
that records them. 'finish' writes what was recorded.
-}
data Server = Server
    { base :: BaseUrl
    , missed :: IO [Text]
    -- ^ What usta asked for that was not recorded.
    , finish :: IO ()
    }

-- | A server for @forge@, "forgejo" or "github".
start :: String -> IO Server
start forge = do
    wanted <- recording
    let
        fixture = "test/contract/" <> forge <> ".json"
    if wanted == Just forge
        then do
            store <- newMVar []
            manager <- HTTP.newManager HTTP.defaultManagerSettings
            (url, stop) <- serve (proxy forge manager store)
            pure
                Server
                    { base = url
                    , missed = pure []
                    , finish = stop >> readMVar store >>= BL.writeFile fixture . encodePretty . reverse
                    }
        else do
            known <- either (\e -> fail (fixture <> ": " <> e)) pure =<< eitherDecodeFileStrict fixture
            misses <- newMVar []
            (url, stop) <- serve (replay known misses)
            pure Server {base = url, missed = reverse <$> readMVar misses, finish = stop}

-- | A request that would change something on the forge.
data Written = Written
    { method :: Text
    , path :: Text
    -- ^ Without the query.
    , query :: [(Text, Text)]
    , contentType :: Text
    , sent :: BL.ByteString
    }
    deriving stock (Show)

{- | A forge that answers reads as @forge@ did when recorded and keeps
every write it is sent, answering it with @answer@; for checking what
usta sends without changing a real forge.
-}
standIn :: String -> (Written -> (Int, BL.ByteString)) -> IO (Server, IO [Written])
standIn forge answer = do
    let
        fixture = "test/contract/" <> forge <> ".json"
    known <- either (\e -> fail (fixture <> ": " <> e)) pure =<< eitherDecodeFileStrict @[Exchange] fixture
    misses <- newMVar []
    writes <- newMVar []
    let
        app req respond = do
            body <- Wai.strictRequestBody req
            -- What was recorded is a read (GitHub's GraphQL reads are POSTs).
            case [e | e <- known, e.request == keyOf req, e.body == normal (decode body)] of
                e : _ -> respond (Wai.responseLBS (Status e.status "") [(CI.mk (encodeUtf8 k), encodeUtf8 v) | (k, v) <- e.headers] (BL.fromStrict (encodeUtf8 e.answer)))
                []
                    | Wai.requestMethod req == "GET" -> do
                        modifyMVar_ misses (pure . (keyOf req :))
                        respond (Wai.responseLBS status404 [] "{\"message\":\"not recorded\"}")
                    | otherwise -> written req body respond
        written req body respond = do
            let
                w =
                    Written
                        { method = decodeUtf8Lenient (Wai.requestMethod req)
                        , path = decodeUtf8Lenient (Wai.rawPathInfo req)
                        , query = [(decodeUtf8Lenient k, maybe "" decodeUtf8Lenient v) | (k, v) <- Wai.queryString req]
                        , contentType = maybe "" decodeUtf8Lenient (lookup "Content-Type" (Wai.requestHeaders req))
                        , sent = body
                        }
                (code, reply) = answer w
            modifyMVar_ writes (pure . (w :))
            respond (Wai.responseLBS (Status code "") [("Content-Type", "application/json")] reply)
    (url, stop) <- serve app
    pure (Server {base = url, missed = reverse <$> readMVar misses, finish = stop}, reverse <$> readMVar writes)

serve :: Wai.Application -> IO (BaseUrl, IO ())
serve app = do
    (port, socket) <- Warp.openFreePort
    thread <- forkIO (Warp.runSettingsSocket Warp.defaultSettings socket app)
    pure (BaseUrl Http "127.0.0.1" port "", killThread thread >> close socket)

replay :: [Exchange] -> MVar [Text] -> Wai.Application
replay known misses req respond = do
    sent <- Wai.strictRequestBody req
    let
        key = keyOf req
        said = normal (decode sent)
    case [e | e <- known, e.request == key, e.body == said] of
        e : _ -> respond (Wai.responseLBS (Status e.status "") [(CI.mk (encodeUtf8 k), encodeUtf8 v) | (k, v) <- e.headers] (BL.fromStrict (encodeUtf8 e.answer)))
        [] -> do
            modifyMVar_ misses (pure . (key <> maybe "" (\b -> " " <> decodeUtf8Lenient (BL.toStrict (encode b))) said :))
            respond (Wai.responseLBS status404 [] "{\"message\":\"not recorded\"}")

{- | "GET /repos/a/b/pulls?page=1&state=open": the query sorted, and its
times left out, as they differ from run to run.
-}
keyOf :: Wai.Request -> Text
keyOf req = decodeUtf8Lenient (Wai.requestMethod req) <> " " <> path <> query
    where
        path = decodeUtf8Lenient (Wai.rawPathInfo req)
        pairs = sortOn fst [(decodeUtf8Lenient k, maybe "" decodeUtf8Lenient v) | (k, v) <- Wai.queryString req, k `notElem` ["since", "before"]]
        query = if null pairs then "" else "?" <> T.intercalate "&" [k <> "=" <> v | (k, v) <- pairs]

-- | A GraphQL body with its search words left out: they hold dates.
normal :: Maybe Value -> Maybe Value
normal = fmap $ \case
    Object o
        | Just (Object vs) <- KeyMap.lookup "variables" o
        , KeyMap.member "q" vs ->
            Object (KeyMap.insert "variables" (Object (KeyMap.insert "q" (String "*") vs)) o)
    v -> v

proxy :: String -> HTTP.Manager -> Recorded -> Wai.Application
proxy forge manager store req respond = do
    sent <- Wai.strictRequestBody req
    (code, hs, got) <- case forge of
        "github" -> fromGitHub req sent
        _ -> fromForgejo manager req sent
    let
        kept = [(decodeUtf8Lenient (CI.original k), decodeUtf8Lenient v) | (k, v) <- hs, CI.foldedCase k `elem` ["content-type", "etag", "link", "x-total-count"]]
        cleaned = if forge == "github" then publicOnly got else got
    modifyMVar_ store (pure . (Exchange (keyOf req) (normal (decode sent)) code kept (decodeUtf8Lenient (BL.toStrict cleaned)) :))
    respond (Wai.responseLBS (Status code "") hs cleaned)

-- | The local Forgejo, as it is.
fromForgejo :: HTTP.Manager -> Wai.Request -> BL.ByteString -> IO (Int, [(CI.CI B.ByteString, B.ByteString)], BL.ByteString)
fromForgejo manager req sent = do
    target <- fromMaybe "http://127.0.0.1:3300" <$> lookupEnv "USTA_FORGEJO"
    base <- HTTP.parseRequest (target <> B8.unpack (Wai.rawPathInfo req) <> B8.unpack (Wai.rawQueryString req))
    r <-
        HTTP.httpLbs
            base
                { HTTP.method = Wai.requestMethod req
                , HTTP.requestHeaders = [h | h@(k, _) <- Wai.requestHeaders req, CI.foldedCase k `elem` ["authorization", "accept", "content-type"]]
                , HTTP.requestBody = HTTP.RequestBodyLBS sent
                , HTTP.redirectCount = 0
                }
            manager
    pure (statusCode (HTTP.responseStatus r), [h | h@(k, _) <- HTTP.responseHeaders r, CI.foldedCase k /= "content-length", CI.foldedCase k /= "transfer-encoding"], HTTP.responseBody r)

-- | GitHub through gh, public data only.
fromGitHub :: Wai.Request -> BL.ByteString -> IO (Int, [(CI.CI B.ByteString, B.ByteString)], BL.ByteString)
fromGitHub req sent
    | path == "/notifications" = pure (200, json, madeUpNotifications)
    | path == "/user/orgs" = pure (200, json, "[]")
    -- The user's own answer has private counts and settings.
    | path == "/user" = pure (200, json, "{\"login\":\"abdivasiyev\",\"id\":1,\"type\":\"User\"}")
    | path == "/graphql" = gh ["api", "graphql", "--include", "--input", "-"] (publicSearch sent)
    | otherwise =
        gh
            ( ["api", "--include", "-X", B8.unpack (Wai.requestMethod req), B8.unpack (B.drop 1 (Wai.rawPathInfo req <> Wai.rawQueryString req))]
                <> concat [["-H", B8.unpack (CI.original k) <> ": " <> B8.unpack v] | (k, v) <- Wai.requestHeaders req, CI.foldedCase k == "accept"]
            )
            ""
    where
        path = Wai.rawPathInfo req
        json = [("Content-Type", "application/json")]

-- | gh, as the user's shell has it; its status line, headers and body.
gh :: [String] -> BL.ByteString -> IO (Int, [(CI.CI B.ByteString, B.ByteString)], BL.ByteString)
gh args input = do
    home <- fromMaybe "" <$> lookupEnv "USTA_REAL_HOME"
    let
        cleaned = ["-u", "GH_TOKEN", "-u", "GITHUB_TOKEN", "-u", "XDG_CONFIG_HOME", "-u", "XDG_DATA_HOME", "-u", "XDG_CONFIG_DIRS", "HOME=" <> home]
    (_, out, _) <- readProcess (setStdin (byteStringInput input) (proc "env" (cleaned <> ["zsh", "-lic", "gh \"$@\"", "zsh"] <> args)))
    let
        (head', rest) = breakOnBlank (BL.toStrict out)
        ls = map (B8.filter (/= '\r')) (B8.lines head')
        code = case map B8.words ls of
            (_ : c : _) : _ -> maybe 500 fst (B8.readInt c)
            _ -> 500
        hs = [(CI.mk k, B8.dropWhile (== ' ') (B.drop 1 v)) | l <- drop 1 ls, let
                                                                                (k, v) = B8.break (== ':') l, not (B.null v)]
    pure (code, [h | h@(k, _) <- hs, CI.foldedCase k `elem` ["content-type", "etag", "link"]], BL.fromStrict rest)
    where
        breakOnBlank s = case B.breakSubstring "\r\n\r\n" s of
            (a, b) | not (B.null b) -> (a, B.drop 4 b)
            _ -> let (a, b) = B.breakSubstring "\n\n" s in (a, B.drop 2 b)

-- | A search about abdivasiyev/usta instead of the user's own.
publicSearch :: BL.ByteString -> BL.ByteString
publicSearch sent = case decode sent of
    Just (Object o)
        | Just (Object vs) <- KeyMap.lookup "variables" o
        , Just (String q) <- KeyMap.lookup "q" vs ->
            let
                kind = T.unwords [w | w <- T.words q, w `elem` ["is:pr", "is:issue"]]
            in
                encode (Object (KeyMap.insert "variables" (Object (KeyMap.insert "q" (String ("repo:abdivasiyev/usta " <> kind)) vs)) o))
    _ -> sent

{- | The answer without anything private in it, and of the user's
repositories only usta: the others are theirs to show or not.
-}
publicOnly :: BL.ByteString -> BL.ByteString
publicOnly got = maybe got (encode . strip) (decode got)
    where
        strip v = case v of
            Array xs -> Array (V.map strip (V.filter kept xs))
            Object o -> Object (counted (KeyMap.map strip o))
            _ -> v
        kept = \case
            Object o ->
                not (any (\k -> KeyMap.lookup k o == Just (Bool True)) ["private", "isPrivate"])
                    && all (\k -> maybe True (== String "abdivasiyev/usta") (KeyMap.lookup k o)) ["nameWithOwner", "full_name"]
            _ -> True
        -- A list's total says no more than what is left of it.
        counted o = case (KeyMap.lookup "nodes" o, KeyMap.lookup "totalCount" o) of
            (Just (Array ns), Just _) -> KeyMap.insert "totalCount" (Number (fromIntegral (V.length ns))) o
            _ -> o

madeUpNotifications :: BL.ByteString
madeUpNotifications =
    encode
        [ object
            [ "id" .= ("1" :: Text)
            , "unread" .= True
            , "reason" .= ("review_requested" :: Text)
            , "updated_at" .= ("2026-11-05T10:00:00Z" :: Text)
            , "subject" .= object ["title" .= ("Test the forges' answers" :: Text), "url" .= ("https://api.github.com/repos/abdivasiyev/usta/pulls/260" :: Text), "type" .= ("PullRequest" :: Text)]
            , "repository" .= object ["full_name" .= ("abdivasiyev/usta" :: Text)]
            ]
        , object
            [ "id" .= ("2" :: Text)
            , "unread" .= False
            , "reason" .= ("mention" :: Text)
            , "updated_at" .= ("2026-11-04T09:00:00Z" :: Text)
            , "subject" .= object ["title" .= ("usta 0.1.0 (beta)" :: Text), "url" .= Null, "type" .= ("Release" :: Text)]
            , "repository" .= object ["full_name" .= ("abdivasiyev/usta" :: Text)]
            ]
        ]
