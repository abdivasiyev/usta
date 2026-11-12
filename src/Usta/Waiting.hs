{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{- | What the user did while the forge could not be reached: it waits
here, in order, until the forge answers again. A small JSON file beside
the cache, so nothing is lost when usta closes.
-}
module Usta.Waiting
    ( Action (..)
    , load
    , update
    , add
    , sending
    , describe
    )
where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar)
import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

import Data.Text qualified as T

import Usta.JsonFile qualified as JsonFile

type Action :: Type
data Action
    = -- | A comment on an issue or pull request.
      Comment {host :: Text, repo :: Text, number :: Int, text :: Text}
    | -- | A notification marked done; the cache has it done already.
      Done {host :: Text, repo :: Text, number :: Int}
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

-- | Everything waiting, oldest first.
load :: IO [Action]
load = file >>= JsonFile.read []

-- | Changes what waits, as one step: an action added meanwhile is kept.
update :: ([Action] -> [Action]) -> IO ()
update change = file >>= \path -> JsonFile.modify [] path change

add :: Action -> IO ()
add action = update (<> [action])

{- | Runs @send@, which sends what waits, unless a send is running
already: two would post the same comment twice. Then it is how many
wait.
-}
sending :: IO Int -> IO Int
sending send = bracket (tryTakeMVar busy) (mapM_ (putMVar busy)) $ \case
    Just () -> send
    Nothing -> length <$> load

busy :: MVar ()
busy = unsafePerformIO (newMVar ())
{-# NOINLINE busy #-}

-- | "Comment on usta #3".
describe :: Action -> Text
describe = \case
    Comment {repo, number} -> "Comment on " <> short repo number
    Done {repo, number} -> "Mark " <> short repo number <> " done"
    where
        short r n = T.takeWhileEnd (/= '/') r <> " #" <> T.pack (show n)

file :: IO FilePath
file = JsonFile.beside "waiting.json"
