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
  , WrongHost (..)
  , requireHost
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Control.Monad.Logger (runNoLoggingT)
import Data.Kind (Type)
import qualified Data.Text as Text
import Database.Persist (get)
import Database.Persist.Sql (ConnectionPool, SqlPersistT, runMigrationQuiet, runSqlPool)
import Database.Persist.Sqlite (mkSqliteConnectionInfo, withSqlitePoolInfo)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getHomeDirectory, getXdgDirectory)
import System.FilePath ((</>))

import Usta.DB.Schema (migrateAll)
import Usta.DB.Schema.Account (AccountId)
import qualified Usta.DB.Schema.Account as Account
import Usta.Types.Host (Host)
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
-- when @action@ returns. Foreign keys are on (persistent-sqlite's default).
--
-- One connection, so queries take turns: a save reads before it writes,
-- and two such transactions on separate connections make SQLite answer
-- "database is locked" at once rather than wait.
withDatabase :: FilePath -> (ConnectionPool -> IO a) -> IO a
withDatabase file action =
  runNoLoggingT $ withSqlitePoolInfo (mkSqliteConnectionInfo (Text.pack file)) 1 $ \pool -> liftIO $ do
    _ <- runSqlPool (runMigrationQuiet migrateAll) pool
    action pool

-- | Runs a query in one transaction.
run :: ConnectionPool -> Result a -> IO a
run pool query = runSqlPool query pool

-- | A save was handed something from another server than the account it
-- is saved under. Thrown by 'requireHost'; 'run' then rolls the whole
-- transaction back.
type WrongHost :: Type
data WrongHost = WrongHost
  { accountHost :: Host
  , itemHost :: Host
  }
  deriving stock (Show)

instance Exception WrongHost

-- | Throws 'WrongHost' unless every host is the account's. Rows don't
-- store the host; they take it from their account, so a pull request
-- from github.com saved under a Forgejo account would silently move.
requireHost :: AccountId -> [Host] -> Result ()
requireHost key hosts = do
  account <- get key
  for_ account $ \row ->
    for_ hosts $ \host ->
      when (host /= row.host) $ liftIO (throwIO WrongHost {accountHost = row.host, itemHost = host})
