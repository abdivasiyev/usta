{-# LANGUAGE OverloadedStrings #-}

{- | What must never leak: a token to another server, a forge's words in
an error, a token in anything shown.
-}
module Usta.SecurityTests (tests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (bracket, displayException, try)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status, hLocation, status200, status302, status401, status500)
import Network.Socket (close)
import Servant.Client (BaseUrl (..), Scheme (Http))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

import Usta.Forge (Failure (..))
import Usta.Types.Parse (parse, unparse)
import Usta.Types.Token (Token)

import Usta.Forge.Forgejo qualified as Forgejo
import Usta.Forge.GitHub qualified as GitHub

tests :: TestTree
tests =
    testGroup
        "Security"
        [ testCase "a redirect to another server does not carry the token" $ do
            heard <- newMVar []
            withServer
                ( \req respond -> do
                    modifyMVar_ heard (pure . (lookup "Authorization" (Wai.requestHeaders req) :))
                    respond (Wai.responseLBS status200 [("Content-Type", "application/json")] "{\"login\":\"tester\"}")
                )
                $ \storage ->
                    withServer (\_ respond -> respond (Wai.responseLBS status302 [(hLocation, B8.pack (url storage <> "/user"))] "")) $ \forge -> do
                        manager <- newManager defaultManagerSettings
                        login <- Forgejo.whoAmI manager forge secret
                        unparse login @?= "tester"
            readMVar heard >>= (@?= [Nothing])
        , testCase "the token goes to the forge itself" $ do
            heard <- newMVar []
            withServer
                ( \req respond -> do
                    modifyMVar_ heard (pure . (lookup "Authorization" (Wai.requestHeaders req) :))
                    respond (Wai.responseLBS status200 [("Content-Type", "application/json")] "{\"login\":\"tester\"}")
                )
                $ \forge -> do
                    manager <- newManager defaultManagerSettings
                    _ <- GitHub.whoAmI manager GitHub.Urls {rest = forge, graph = forge} secret
                    readMVar heard >>= (@?= [Just "Bearer ghp_neverShown"])
        , testCase "a refused token asks to sign in again"
            $ failing status401 "{\"message\":\"Bad credentials\"}" >>= (@?= Left SignedOut)
        , testCase "an error leaves out what the forge said" $ do
            found <- failing status500 "{\"message\":\"you sent ghp_neverShown\"}"
            found @?= Left (Refused "the forge answered 500")
        , testCase "a forge out of reach is offline, and says no more" $ do
            manager <- newManager defaultManagerSettings
            (port, socket) <- Warp.openFreePort
            close socket
            found <- try (Forgejo.whoAmI manager (BaseUrl Http "127.0.0.1" port "") secret)
            fmap unparse found @?= Left Offline
        , testCase "no failure shows the token" $ do
            found <- failing status500 "ghp_neverShown"
            assertBool "shown" (either (not . T.isInfixOf "neverShown" . T.pack . displayException) (const True) found)
        ]
    where
        secret = either (error . T.unpack) id (parse "ghp_neverShown") :: Token

-- | What the forge's code makes of an answer to "who am I".
failing :: Status -> BL.ByteString -> IO (Either Failure T.Text)
failing status body =
    withServer (\_ respond -> respond (Wai.responseLBS status [("Content-Type", "application/json")] body)) $ \forge -> do
        manager <- newManager defaultManagerSettings
        fmap unparse <$> try (Forgejo.whoAmI manager forge (either (error . T.unpack) id (parse "ghp_neverShown")))

withServer :: Wai.Application -> (BaseUrl -> IO a) -> IO a
withServer app act =
    bracket
        (Warp.openFreePort >>= \(port, socket) -> (,,) port socket <$> forkIO (Warp.runSettingsSocket Warp.defaultSettings socket app))
        (\(_, socket, thread) -> killThread thread >> close socket)
        (\(port, _, _) -> act (BaseUrl Http "127.0.0.1" port ""))

url :: BaseUrl -> String
url b = "http://" <> b.baseUrlHost <> ":" <> show b.baseUrlPort
