{-# LANGUAGE OverloadedStrings #-}

-- | What the user was writing and has not sent, kept by where it was
-- written, so a comment box opens with it again, after leaving the page
-- or closing usta. A small JSON file beside the cache.
module Usta.Drafts
  ( load
  , keep
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import qualified Usta.JsonFile as JsonFile

-- | The draft written at @key@; empty when there is none.
load :: Text -> IO Text
load key = Map.findWithDefault "" key <$> (file >>= JsonFile.read Map.empty)

-- | Keeps @text@ at @key@; an empty one takes the draft away, as after it
-- was sent.
keep :: Text -> Text -> IO ()
keep key text = do
  path <- file
  JsonFile.modify Map.empty path (if T.null (T.strip text) then Map.delete key else Map.insert key text)

file :: IO FilePath
file = JsonFile.beside "drafts.json"
