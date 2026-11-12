{-# LANGUAGE OverloadedStrings #-}

{- | The repositories the user pinned to the sidebar, by host and name, in
the order they were pinned. usta's own, kept in a small JSON file
beside the cache.
-}
module Usta.Pins
    ( pins
    , setPinned
    )
where

import Data.Text (Text)

import Usta.JsonFile qualified as JsonFile

-- | Each pin as (host, name).
pins :: IO [(Text, Text)]
pins = file >>= JsonFile.read []

setPinned :: (Text, Text) -> Bool -> IO ()
setPinned repo on = do
    path <- file
    JsonFile.modify [] path (\current -> filter (/= repo) current <> [repo | on])

file :: IO FilePath
file = JsonFile.beside "pins.json"
