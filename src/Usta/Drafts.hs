{-# LANGUAGE OverloadedStrings #-}

-- | What the user was writing and has not sent, kept by where it was
-- written, so a comment box opens with it again, after leaving the page
-- or closing usta. A small JSON file beside the cache.
module Usta.Drafts
  ( load
  , keep
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (decodeFileStrict, encodeFile)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeDirectory, (</>))

import Usta.DB (databaseFile)

-- | The draft written at @key@; empty when there is none.
load :: Text -> IO Text
load key = Map.findWithDefault "" key <$> everything

-- | Keeps @text@ at @key@; an empty one takes the draft away, as after it
-- was sent.
keep :: Text -> Text -> IO ()
keep key text = do
  drafts <- everything
  let changed = if T.null (T.strip text) then Map.delete key drafts else Map.insert key text drafts
  path <- file
  -- A draft that cannot be written is only lost, never an error.
  _ <- try (encodeFile path changed) :: IO (Either SomeException ())
  pure ()

everything :: IO (Map.Map Text Text)
everything = do
  path <- file
  found <- try (decodeFileStrict path) :: IO (Either SomeException (Maybe (Map.Map Text Text)))
  pure (either (const Map.empty) (maybe Map.empty id) found)

file :: IO FilePath
file = (</> "drafts.json") . takeDirectory <$> databaseFile
