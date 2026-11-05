{-# LANGUAGE OverloadedStrings #-}

-- | Signing in in the browser: the answer counts only for this sign-in,
-- the token is asked with the PKCE verifier, and a refusal is said.
module Usta.OAuthTests (tests) where

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryPutMVar)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (void)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified GI.GLib as GLib
import Network.HTTP.Client (Manager, defaultManagerSettings, httpLbs, newManager, parseRequest)
import Network.HTTP.Types (parseQuery, status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import qualified Usta.OAuth as OAuth
import Usta.Types.Parse (parse, unparse)

tests :: TestTree
tests =
  withResource startLoop GLib.mainLoopQuit $ \_ ->
    testGroup
      "OAuth"
      [ testCase "an answer for another sign-in is refused" $ withForge $ \manager site _ -> do
          waiting <- begin manager site
          callback manager waiting [("state", "someone-else"), ("code", "c")]
          fmap (const ()) <$> waiting.answer >>= (@?= Left "The forge's answer did not match this sign-in.")
      , testCase "the forge's refusal is said" $ withForge $ \manager site _ -> do
          waiting <- begin manager site
          callback manager waiting [("state", stateOf waiting), ("error", "access_denied")]
          fmap (const ()) <$> waiting.answer >>= (@?= Left "The forge said: access_denied")
      , testCase "the code is traded with the verifier the challenge was made from" $ withForge $ \manager site asked -> do
          waiting <- begin manager site
          -- A browser may ask for the page's icon first.
          _ <- httpLbs' manager (T.unpack (T.replace "/callback" "/favicon.ico" (redirectOf waiting)))
          callback manager waiting [("state", stateOf waiting), ("code", "the-code")]
          tokens <- waiting.answer
          fmap (\t -> (unparse t.access, unparse t.refreshToken)) tokens @?= Right ("fresh", "again")
          form <- readMVar asked
          lookup "code" form @?= Just (Just "the-code")
          lookup "grant_type" form @?= Just (Just "authorization_code")
          case lookup "code_verifier" form of
            Just (Just verifier) -> challengeOf waiting @?= decodeUtf8 (B8.filter (/= '=') (Base64.encode (SHA256.hash verifier)))
            _ -> assertFailure "no verifier"
      , testCase "the authorize page asks for this client, with PKCE" $ withForge $ \manager site _ -> do
          waiting <- begin manager site
          (param "client_id" waiting, param "response_type" waiting, param "code_challenge_method" waiting)
            @?= ("usta-test", "code", "S256")
          waiting.stop
      , testCase "the refresh token is kept with the client" $ do
          r <- either (fail . T.unpack) pure (parse "refresh-me")
          fmap (fmap unparse) (OAuth.stored (OAuth.storing "client" r)) @?= Just ("client", "refresh-me")
      , testCase "a token pasted by hand is not taken for one" $ do
          t <- either (fail . T.unpack) pure (parse "ghp_pasted")
          fmap (fmap unparse) (OAuth.stored t) @?= Nothing
      ]

-- | GLib's main loop, which the sign-in's listener answers through.
startLoop :: IO GLib.MainLoop
startLoop = do
  loop <- GLib.mainLoopNew Nothing False
  void (forkOS (GLib.mainLoopRun loop))
  pure loop

-- | A forge whose token endpoint keeps the form it was sent.
withForge :: (Manager -> Text -> MVarForm -> IO ()) -> IO ()
withForge act = do
  asked <- newEmptyMVar
  let app req respond = do
        body <- Wai.strictRequestBody req
        _ <- tryPutMVar asked (parseQuery (BL.toStrict body))
        respond (Wai.responseLBS status200 [("Content-Type", "application/json")] "{\"access_token\":\"fresh\",\"refresh_token\":\"again\",\"expires_in\":3600}")
  manager <- newManager defaultManagerSettings
  Warp.testWithApplication (pure app) $ \port -> act manager ("http://127.0.0.1:" <> T.pack (show port)) asked

type MVarForm = MVar [(B8.ByteString, Maybe B8.ByteString)]

begin :: Manager -> Text -> IO OAuth.Waiting
begin manager site = do
  started <- newEmptyMVar
  -- On the main loop's thread, as the sign-in wants.
  _ <- GLib.idleAdd GLib.PRIORITY_DEFAULT (OAuth.begin manager site "usta-test" >>= putMVar started >> pure False)
  readMVar started >>= either (fail . T.unpack) pure

-- | The browser coming back to usta.
callback :: Manager -> OAuth.Waiting -> [(Text, Text)] -> IO ()
callback manager waiting pairs =
  void (httpLbs' manager (T.unpack (redirectOf waiting <> "?" <> T.intercalate "&" [k <> "=" <> v | (k, v) <- pairs])))

httpLbs' :: Manager -> String -> IO ()
httpLbs' manager u = parseRequest u >>= \r -> void (httpLbs r manager)

param :: B8.ByteString -> OAuth.Waiting -> Text
param name waiting = maybe "" decodeUtf8 (lookup name (parseQuery (encodeUtf8 (snd (T.breakOn "?" waiting.page)))) >>= id)

stateOf, redirectOf, challengeOf :: OAuth.Waiting -> Text
stateOf = param "state"
redirectOf = param "redirect_uri"
challengeOf = param "code_challenge"
