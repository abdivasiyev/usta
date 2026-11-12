{-# LANGUAGE OverloadedStrings #-}

{- | The files of each pull request marked viewed, each with a mark of
what its diff was then, so a file that changed since opens again. A
small JSON file beside the cache.
-}
module Usta.Viewed
    ( load
    , keep
    )
where

import Data.Text (Text)

import Data.Map.Strict qualified as Map

import Usta.JsonFile qualified as JsonFile

-- | The files viewed at @key@ (a pull request), by path.
load :: Text -> IO (Map.Map Text Text)
load key = Map.findWithDefault Map.empty key <$> (file >>= JsonFile.read Map.empty)

-- | Keeps the files viewed at @key@; none takes the entry away.
keep :: Text -> Map.Map Text Text -> IO ()
keep key files = do
    path <- file
    JsonFile.modify Map.empty path (if Map.null files then Map.delete key else Map.insert key files)

file :: IO FilePath
file = JsonFile.beside "viewed.json"
