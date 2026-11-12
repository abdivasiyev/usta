{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | What is done offline waits, and goes to the forge once it answers:
in order, each once, and a refused one dropped. Through the window's
own source, on a forge that can be taken offline.
-}
module Usta.WaitingTests (tests) where

import Control.Exception (throwIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Text qualified

import Usta.Accounts (Accounts (..), Problem (..))
import Usta.DB (run)
import Usta.DB.Account (saveAccount)
import Usta.DB.Notification (saveNotifications)
import Usta.DB.Repository (saveRepositories)
import Usta.Forge (Failure (..), Fetched (..), Forge (..), Known (..))
import Usta.SyncTests (githubAccount, withCache)
import Usta.Types.Parse (parseM, unparse)
import Usta.UI.Source (Source (..))
import Usta.Waiting (Action (..))

import Usta.Forge qualified as F
import Usta.Forge.Sample qualified as Sample
import Usta.Types.Discussion qualified as D
import Usta.UI.Source qualified as Source
import Usta.Waiting qualified as Waiting

tests :: TestTree
tests =
    -- One after another: what waits is one file.
    dependentTestGroup
        "Waiting"
        AllFinish
        [ testCase "a comment made offline waits, and is sent when the forge answers" $ withSource $ \t -> do
            writeIORef t.offline True
            t.source.comment "github.com" "abdivasiyev/usta" 3 "Written on a train" >>= (@?= Right ())
            t.source.waiting >>= (@?= [Comment "github.com" "abdivasiyev/usta" 3 "Written on a train"])
            readIORef t.sent >>= (@?= [])
            writeIORef t.offline False
            t.source.sendWaiting >>= (@?= 0)
            readIORef t.sent >>= (@?= ["comment #3: Written on a train"])
            t.source.waiting >>= (@?= [])
        , testCase "still offline, everything still waits" $ withSource $ \t -> do
            writeIORef t.offline True
            mapM_ (\n -> t.source.comment "github.com" "abdivasiyev/usta" n "later") [1, 2]
            t.source.sendWaiting >>= (@?= 2)
            length <$> t.source.waiting >>= (@?= 2)
        , testCase "sent in the order they were made" $ withSource $ \t -> do
            writeIORef t.offline True
            mapM_ (\n -> t.source.comment "github.com" "abdivasiyev/usta" n "in order") [5, 1, 3]
            writeIORef t.offline False
            _ <- t.source.sendWaiting
            readIORef t.sent >>= (@?= ["comment #5: in order", "comment #1: in order", "comment #3: in order"])
        , testCase "one the forge refuses is dropped, and the rest go" $ withSource $ \t -> do
            writeIORef t.offline True
            mapM_ (\n -> t.source.comment "github.com" "abdivasiyev/usta" n "refuse me?") [99, 3]
            writeIORef t.offline False
            t.source.sendWaiting >>= (@?= 0)
            readIORef t.sent >>= (@?= ["comment #3: refuse me?"])
        , testCase "a done made offline marks the thread read when sent" $ withSource $ \t -> do
            Waiting.add Done {host = "github.com", repo = "abdivasiyev/usta", number = 2}
            t.source.sendWaiting >>= (@?= 0)
            readIORef t.sent >>= (@?= ["read 1"])
        , testCase "one dropped by hand is not sent" $ withSource $ \t -> do
            writeIORef t.offline True
            mapM_ (\n -> t.source.comment "github.com" "abdivasiyev/usta" n "maybe") [1, 2]
            t.source.dropWaiting 0
            writeIORef t.offline False
            _ <- t.source.sendWaiting
            readIORef t.sent >>= (@?= ["comment #2: maybe"])
        ]

data Running = Running
    { source :: Source
    , offline :: IORef Bool
    , sent :: IORef [Text]
    }

{- | The sample's github.com account with a forge that keeps what it is
sent, refuses #99, and is out of reach while @offline@ is set.
-}
withSource :: (Running -> IO ()) -> IO ()
withSource act = withCache $ \pool -> do
    Waiting.update (const [])
    now <- getCurrentTime
    account <- githubAccount
    key <- run pool (saveAccount now account)
    sample <- Sample.new now account
    let
        none = Known {etag = Nothing, since = Nothing}
    (sample.forge :: Forge).notifications none >>= \case
        Fetched {items} -> run pool (saveNotifications key items)
        NotModified -> pure ()
    (sample.forge :: Forge).repositories none >>= \case
        Fetched {items} -> run pool (saveRepositories key now items)
        NotModified -> pure ()
    offline <- newIORef False
    sent <- newIORef []
    me <- parseM "abdivasiyev"
    let
        keep line = atomicModifyIORef' sent (\xs -> (xs <> [line], ()))
        reachable = readIORef offline >>= \o -> if o then throwIO Offline else pure ()
        forge :: Forge
        forge =
            sample.forge
                { F.comment = \_ n text -> do
                    reachable
                    if unparse n == 99 then throwIO (Refused "locked") else keep ("comment #" <> showT (unparse n) <> ": " <> text)
                    pure D.Comment {author = me, body = text, createdAt = now, id = Nothing, edited = False, url = "", reactions = [], review = Nothing}
                , F.markRead = \thread -> reachable >> keep ("read " <> unparse thread)
                }
        accounts =
            Accounts
                { signIn = \_ _ _ -> pure (Left NotSignedIn)
                , remove = \_ -> pure (Left NotSignedIn)
                , mirror = \_ _ _ -> pure (Left NotSignedIn)
                , origins = \_ _ -> pure (Left NotSignedIn)
                , forgeOf = \k -> pure (if k == key then Just forge else Nothing)
                , beginBrowser = \_ _ -> pure (Left "no browser here")
                , signInBrowser = \_ _ _ _ -> pure (Left NotSignedIn)
                }
    act Running {source = Source.withAccounts pool accounts Source.samples, offline = offline, sent = sent}
    Waiting.update (const [])
    where
        showT = Data.Text.pack . show
