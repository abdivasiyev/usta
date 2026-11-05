{-# LANGUAGE OverloadedStrings #-}

-- | What usta keeps beside its cache: drafts, viewed files, actions
-- waiting for the forge, pins, the log, and tokens in a keyring. The
-- tests' own home holds them (see Main).
module Usta.LocalFilesTests (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (getHomeDirectory)
import Test.Tasty (DependencyType (..), TestTree, sequentialTestGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Usta.Drafts as Drafts
import qualified Usta.Keyring as Keyring
import qualified Usta.Log as Log
import qualified Usta.Pins as Pins
import Usta.Types.Parse (parseM, unparse)
import Usta.Types.Token (Token)
import qualified Usta.Viewed as Viewed
import Usta.Waiting (Action (..))
import qualified Usta.Waiting as Waiting

tests :: TestTree
tests =
  -- One after another: they share the files and the log.
  sequentialTestGroup
    "Local files"
    AllFinish
    [ testCase "a draft is kept, and an empty one taken away" $ do
        Drafts.keep "github.com/a/b#1" "half a comment"
        Drafts.load "github.com/a/b#1" >>= (@?= "half a comment")
        Drafts.keep "github.com/a/b#1" "  \n"
        Drafts.load "github.com/a/b#1" >>= (@?= "")
    , testCase "drafts are kept by where they were written" $ do
        Drafts.keep "x#1" "one"
        Drafts.keep "x#2" "two"
        (,) <$> Drafts.load "x#1" <*> Drafts.load "x#2" >>= (@?= ("one", "two"))
    , testCase "viewed files" $ do
        Viewed.keep "pr" (Map.fromList [("src/A.hs", "abc")])
        Viewed.load "pr" >>= (@?= Map.fromList [("src/A.hs", "abc")])
        Viewed.keep "pr" Map.empty
        Viewed.load "pr" >>= (@?= Map.empty)
    , testCase "actions wait in order" $ do
        Waiting.update (const [])
        Waiting.add Comment {host = "github.com", repo = "a/usta", number = 3, text = "hi"}
        Waiting.add Done {host = "github.com", repo = "a/usta", number = 4}
        map Waiting.describe <$> Waiting.load >>= (@?= ["Comment on usta #3", "Mark usta #4 done"])
        Waiting.update (const [])
        Waiting.load >>= (@?= [])
    , testCase "actions added at once are all kept" $ do
        Waiting.update (const [])
        done <- newEmptyMVar
        forM_ [1 .. 40] $ \n -> forkIO (Waiting.add Done {host = "github.com", repo = "a/usta", number = n} >> putMVar done ())
        forM_ [1 .. 40 :: Int] $ \_ -> takeMVar done
        length <$> Waiting.load >>= (@?= 40)
        Waiting.update (const [])
    , testCase "one send of what waits at a time" $ do
        Waiting.update (const [Done {host = "github.com", repo = "a/usta", number = 1}])
        started <- newEmptyMVar
        finish <- newEmptyMVar
        first <- newEmptyMVar
        _ <- forkIO (Waiting.sending (putMVar started () >> takeMVar finish >> pure 0) >>= putMVar first)
        takeMVar started
        second <- Waiting.sending (pure 99)
        putMVar finish ()
        (,) second <$> takeMVar first >>= (@?= (1, 0))
        Waiting.update (const [])
    , testCase "pins, once each, in the order pinned" $ do
        Pins.setPinned ("github.com", "a/one") True
        Pins.setPinned ("github.com", "a/two") True
        Pins.setPinned ("github.com", "a/one") True
        Pins.pins >>= (@?= [("github.com", "a/two"), ("github.com", "a/one")])
        Pins.setPinned ("github.com", "a/two") False
        Pins.pins >>= (@?= [("github.com", "a/one")])
    , testCase "the log writes the home folder as ~" $ do
        home <- T.pack <$> getHomeDirectory
        Log.say "test" ("opened " <> home <> "/x.txt")
        found <- Log.lastLines 1
        assertBool (show found) (any ("opened ~/x.txt" `T.isInfixOf`) found && not (any (home `T.isInfixOf`) found))
    , testCase "a message of many lines stays one entry" $ do
        Log.say "test" "first\nsecond"
        found <- Log.lastLines 2
        map (T.isSuffixOf "test  first") (take 1 found) <> map (== "         second") (drop 1 found) @?= [True, True]
    , testCase "a crash is told at the next start" $ do
        _ <- Log.start
        crashed <- Log.start
        Log.stop
        clean <- Log.start
        Log.stop
        (crashed, clean) @?= (True, False)
    , testCase "the memory keyring" $ do
        keyring <- Keyring.memory
        host <- parseM "github.com"
        login <- parseM "a"
        other <- parseM "b"
        token <- parseM "ghp_secret" :: IO Token
        keyring.save host login token
        fmap unparse <$> keyring.load host login >>= (@?= Just "ghp_secret")
        fmap unparse <$> keyring.load host other >>= (@?= Nothing)
        keyring.forget host login
        fmap unparse <$> keyring.load host login >>= (@?= Nothing)
    ]
