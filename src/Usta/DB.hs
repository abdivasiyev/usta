{-# LANGUAGE OverloadedStrings #-}

-- | The local cache: a SQLite file in the platform's data folder.
--
-- Everything the app shows is read from here, and sync writes here; the
-- forges are never asked while a screen is drawn.
module Usta.DB
  ( Result
  , databaseFile
  , withDatabase
  , run
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Kind (Type)
import qualified Data.Text as Text
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runMigrationQuiet, runSqlPool)
import Database.Persist.Sqlite (mkSqliteConnectionInfo, withSqlitePoolInfo)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getHomeDirectory, getXdgDirectory)
import System.FilePath ((</>))

import Usta.DB.Schema (migrateAll)
import qualified Usta.Types.Platform as Platform

-- | A query, run against the cache by 'run'.
type Result :: Type -> Type
type Result a = SqlPersistT IO a

-- | @usta.db@ in the platform's data folder, which is created if missing:
-- @~/Library/Application Support/usta@ on macOS, @$XDG_DATA_HOME/usta@
-- (normally @~/.local/share/usta@) elsewhere.
databaseFile :: IO FilePath
databaseFile = do
  folder <- case Platform.current of
    Platform.MacOS -> (</> "Library" </> "Application Support" </> "usta") <$> getHomeDirectory
    _ -> getXdgDirectory XdgData "usta"
  createDirectoryIfMissing True folder
  pure (folder </> "usta.db")

-- | Opens the cache at @file@, brings its schema up to date, and closes it
-- when @action@ returns. Foreign keys are on and the file is in WAL mode
-- (persistent-sqlite's defaults), so sync can write while screens read.
withDatabase :: FilePath -> (ConnectionPool -> IO a) -> IO a
withDatabase file action =
  runNoLoggingT $ withSqlitePoolInfo (mkSqliteConnectionInfo (Text.pack file)) 4 $ \pool -> liftIO $ do
    _ <- runSqlPool (runMigrationQuiet migrateAll) pool
    action pool

-- | Runs a query in one transaction.
run :: ConnectionPool -> Result a -> IO a
run pool query = runSqlPool query pool
