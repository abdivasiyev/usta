{-# LANGUAGE OverloadedStrings #-}

-- | The files of each pull request marked viewed, each with a mark of
-- what its diff was then, so a file that changed since opens again. A
-- small JSON file beside the cache.
module Usta.Viewed
  ( load
  , keep
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (decodeFileStrict, encodeFile)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.FilePath (takeDirectory, (</>))

import Usta.DB (databaseFile)

-- | The files viewed at @key@ (a pull request), by path.
load :: Text -> IO (Map.Map Text Text)
load key = Map.findWithDefault Map.empty key <$> everything

-- | Keeps the files viewed at @key@; none takes the entry away.
keep :: Text -> Map.Map Text Text -> IO ()
keep key files = do
  known <- everything
  let changed = if Map.null files then Map.delete key known else Map.insert key files known
  path <- file
  -- Not written is only forgotten, never an error.
  _ <- try (encodeFile path changed) :: IO (Either SomeException ())
  pure ()

everything :: IO (Map.Map Text (Map.Map Text Text))
everything = do
  path <- file
  found <- try (decodeFileStrict path) :: IO (Either SomeException (Maybe (Map.Map Text (Map.Map Text Text))))
  pure (either (const Map.empty) (maybe Map.empty id) found)

file :: IO FilePath
file = (</> "viewed.json") . takeDirectory <$> databaseFile
