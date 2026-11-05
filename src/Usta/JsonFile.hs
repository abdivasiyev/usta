-- | The small JSON files usta keeps beside its cache: drafts, viewed
-- files, pins and what waits for the forge. Each change reads, changes
-- and writes its file under one lock, so two at once (an offline comment
-- while the waiting ones are sent) keep both; and it writes a new file
-- and renames it over the old one, so usta stopping half way loses
-- nothing. A file that is missing or broken reads as @empty@.
module Usta.JsonFile
  ( read
  , modify
  , beside
  ) where

import Prelude hiding (read)

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON, ToJSON, decodeFileStrict, encodeFile)
import System.Directory (renameFile)
import System.FilePath (takeDirectory, (</>))
import System.IO.Unsafe (unsafePerformIO)

import Usta.DB (databaseFile)

-- | What the file holds, or @empty@.
read :: forall a. FromJSON a => a -> FilePath -> IO a
read empty path = do
  found <- try (decodeFileStrict path) :: IO (Either SomeException (Maybe a))
  pure (either (const empty) (maybe empty id) found)

-- | Changes what the file holds. A file that cannot be written is only
-- not changed, never an error.
modify :: (FromJSON a, ToJSON a) => a -> FilePath -> (a -> a) -> IO ()
modify empty path change = withMVar lock $ \() -> do
  changed <- change <$> read empty path
  _ <- try (encodeFile (path <> ".new") changed >> renameFile (path <> ".new") path) :: IO (Either SomeException ())
  pure ()

-- | @name@ in the folder that has the cache.
beside :: FilePath -> IO FilePath
beside name = (</> name) . takeDirectory <$> databaseFile

-- | One for every file: they change rarely, and never for long.
lock :: MVar ()
lock = unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}
