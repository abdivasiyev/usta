{-# LANGUAGE OverloadedStrings #-}

{- | One usta at a time: a later one hands its links to the one running.
And usta as git's credential helper: the token goes to git for the
account it asks about, and only over https.
-}
module Usta.HandoffTests (tests) where

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad (void)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import GI.GLib qualified as GLib

import Usta.Git.Credential (Request (..), parseRequest, respond)
import Usta.Types.Parse (parseM, unparse)
import Usta.Types.Token (Token)

import Usta.Handoff qualified as Handoff
import Usta.Keyring qualified as Keyring

tests :: TestTree
tests =
    testGroup
        "Handoff and credentials"
        [ withResource startLoop GLib.mainLoopQuit $ \_ ->
            -- One after another: they share the one socket.
            dependentTestGroup
                "handoff"
                AllFinish
                [ testCase "with no usta running, nothing is handed"
                    $ Handoff.send ["https://github.com/abdivasiyev/usta"] >>= (@?= False)
                , testCase "the running usta gets each link, and is shown" $ do
                    got <- newMVar []
                    shown <- newEmptyMVar
                    onLoop (Handoff.listen (\l -> modifyMVar_ got (pure . (<> [l]))) (putMVar shown ()))
                    Handoff.send ["https://github.com/abdivasiyev/usta/pull/1", "  ", "usta://git.azizovich.uz/a/b"] >>= (@?= True)
                    timeout 5000000 (takeMVar shown) >>= assertBool "not shown" . maybe False (const True)
                    -- The links come right after, in the same turn of the loop.
                    onLoop (pure ())
                    readMVar got >>= (@?= ["https://github.com/abdivasiyev/usta/pull/1", "usta://git.azizovich.uz/a/b"])
                , testCase "a later usta with no links only brings it up" $ do
                    shown <- newEmptyMVar
                    onLoop (Handoff.listen (const (pure ())) (putMVar shown ()))
                    Handoff.send [] >>= (@?= True)
                    timeout 5000000 (takeMVar shown) >>= assertBool "not shown" . maybe False (const True)
                ]
        , testGroup
            "credentials"
            [ testCase "git's request over https, for a user"
                $ fmap (\r -> (unparse r.host, unparse r.login)) (parseRequest "protocol=https\nhost=git.azizovich.uz\nusername=tester\npath=a/b.git\n")
                    @?= Just ("git.azizovich.uz", "tester")
            , testCase "not over plain http" $ parseRequest "protocol=http\nhost=example.com\nusername=a\n" @?= Nothing
            , testCase "not without a user" $ parseRequest "protocol=https\nhost=example.com\n" @?= Nothing
            , testCase "not for a host that is no host" $ parseRequest "protocol=https\nhost=https://x\nusername=a\n" @?= Nothing
            , testCase "the account's token, as git wants it" $ do
                keyring <- Keyring.memory
                Just request <- pure (parseRequest "protocol=https\nhost=github.com\nusername=abdivasiyev\n")
                token <- parseM "ghp_forGit" :: IO Token
                keyring.save request.host request.login token
                respond keyring (\_ t -> pure (Just t)) request >>= (@?= "username=abdivasiyev\npassword=ghp_forGit\n")
            , testCase "nothing for an account usta does not have" $ do
                keyring <- Keyring.memory
                Just request <- pure (parseRequest "protocol=https\nhost=github.com\nusername=someone\n")
                respond keyring (\_ t -> pure (Just t)) request >>= (@?= "")
            , testCase "nothing when the token cannot be renewed" $ do
                keyring <- Keyring.memory
                Just request <- pure (parseRequest "protocol=https\nhost=github.com\nusername=abdivasiyev\n")
                parseM "oauth:client:refresh" >>= keyring.save request.host request.login
                respond keyring (\_ _ -> pure Nothing) request >>= (@?= ("" :: Text))
            ]
        ]

startLoop :: IO GLib.MainLoop
startLoop = do
    loop <- GLib.mainLoopNew Nothing False
    void (forkOS (GLib.mainLoopRun loop))
    pure loop

-- | Runs @act@ on the main loop's thread and waits for it.
onLoop :: IO () -> IO ()
onLoop act = do
    done <- newEmptyMVar
    _ <- GLib.idleAdd GLib.PRIORITY_DEFAULT (act >> putMVar done () >> pure False)
    takeMVar done
