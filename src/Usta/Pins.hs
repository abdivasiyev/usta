{-# LANGUAGE OverloadedStrings #-}

-- | The repositories the user pinned to the sidebar, by host and name, in
-- the order they were pinned. usta's own, kept in a small JSON file
-- beside the cache.
module Usta.Pins
  ( pins
  , setPinned
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (decodeFileStrict, encodeFile)
import Data.Text (Text)
import System.FilePath (takeDirectory, (</>))

import Usta.DB (databaseFile)

-- | Each pin as (host, name).
pins :: IO [(Text, Text)]
pins = do
  path <- file
  found <- try (decodeFileStrict path) :: IO (Either SomeException (Maybe [(Text, Text)]))
  pure (either (const []) (maybe [] id) found)

setPinned :: (Text, Text) -> Bool -> IO ()
setPinned repo on = do
  current <- pins
  let changed = filter (/= repo) current <> [repo | on]
  path <- file
  _ <- try (encodeFile path changed) :: IO (Either SomeException ())
  pure ()

file :: IO FilePath
file = (</> "pins.json") . takeDirectory <$> databaseFile
