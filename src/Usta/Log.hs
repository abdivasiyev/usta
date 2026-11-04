{-# LANGUAGE OverloadedStrings #-}

-- | usta's log: what it did and what went wrong, one line each, in
-- @usta.log@ beside the cache. It never holds tokens or what was sent
-- to a forge, and the home folder is written as ~, so a report can carry
-- it as it is. A marker file says usta is running; found at the next
-- start, it means the last run did not end on its own.
module Usta.Log
  ( say
  , lastLines
  , start
  , stop
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.Directory (doesFileExist, getFileSize, getHomeDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory, (</>))

import Usta.DB (databaseFile)

-- | "00:31:49 sync  git.azizovich.uz synced"; @area@ is short, as
-- "sync", "ui" or "error".
say :: Text -> Text -> IO ()
say area message = quietly $ do
  path <- file "usta.log"
  home <- T.pack <$> getHomeDirectory
  now <- getZonedTime
  -- Past 1 MB the log starts again, keeping the one before.
  big <- doesFileExist path >>= \e -> if e then (> 1000000) <$> getFileSize path else pure False
  when big $ renameFile path (path <> ".1")
  let clean = T.replace home "~" (T.replace "\n" "\n         " message)
  T.appendFile path (T.pack (formatTime defaultTimeLocale "%F %T" now) <> " " <> T.justifyLeft 5 ' ' area <> " " <> clean <> "\n")

-- | The last @n@ lines, oldest first.
lastLines :: Int -> IO [Text]
lastLines n = do
  path <- file "usta.log"
  found <- try (T.readFile path)
  pure (either (\(_ :: SomeException) -> []) (reverse . take n . reverse . T.lines) found)

-- | Marks usta as running; True when the last run did not stop itself.
start :: IO Bool
start = do
  marker <- file "running"
  crashed <- doesFileExist marker
  quietly (T.writeFile marker "")
  say "start" "usta started"
  pure crashed

-- | Called when usta quits on its own.
stop :: IO ()
stop = do
  say "start" "usta quit"
  file "running" >>= quietly . removeFile

file :: FilePath -> IO FilePath
file name = (</> name) . takeDirectory <$> databaseFile

-- | A log that cannot be written is only lost, never an error.
quietly :: IO () -> IO ()
quietly act = void (try act :: IO (Either SomeException ()))
