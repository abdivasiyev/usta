{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | What the user did while the forge could not be reached: it waits
-- here, in order, until the forge answers again. A small JSON file beside
-- the cache, so nothing is lost when usta closes.
module Usta.Waiting
  ( Action (..)
  , load
  , save
  , add
  , describe
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict, encodeFile)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath (takeDirectory, (</>))

import Usta.DB (databaseFile)

type Action :: Type
data Action
  = Comment {host :: Text, repo :: Text, number :: Int, text :: Text}
    -- ^ A comment on an issue or pull request.
  | Done {host :: Text, repo :: Text, number :: Int}
    -- ^ A notification marked done; the cache has it done already.
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Everything waiting, oldest first.
load :: IO [Action]
load = do
  path <- file
  found <- try (decodeFileStrict path) :: IO (Either SomeException (Maybe [Action]))
  pure (either (const []) (maybe [] id) found)

save :: [Action] -> IO ()
save actions = do
  path <- file
  _ <- try (encodeFile path actions) :: IO (Either SomeException ())
  pure ()

add :: Action -> IO ()
add action = load >>= save . (<> [action])

-- | "Comment on usta #3".
describe :: Action -> Text
describe = \case
  Comment {repo, number} -> "Comment on " <> short repo number
  Done {repo, number} -> "Mark " <> short repo number <> " done"
  where
    short r n = T.takeWhileEnd (/= '/') r <> " #" <> T.pack (show n)

file :: IO FilePath
file = (</> "waiting.json") . takeDirectory <$> databaseFile
