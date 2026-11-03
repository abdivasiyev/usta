{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ImplicitParams #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Data.GI.Base
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import GI.Gtk.Objects.Application (Application (..))
import Network.HTTP.Client.TLS (newTlsManager)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Environment (getArgs)
import System.FilePath ((</>))

import qualified Usta.Accounts as Accounts
import Usta.Bus (newBus)
import Usta.DB (databaseFile, run, withDatabase)
import Usta.DB.Account (listAccounts, saveAccount)
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (Forge)
import Usta.Forge.Http (apiUrl)
import qualified Usta.Forge.Sample as Sample
import qualified Usta.Git.Credential as Credential
import qualified Usta.Keyring as Keyring
import qualified Usta.Mirrors as Mirrors
import qualified Usta.Sync as Sync
import Usta.Types.Account (Account (..), AuthMethod (..))
import Usta.Types.Forge (Kind (..))
import Usta.Types.Parse (parseM)
import qualified Usta.UI.Gallery as Gallery
import qualified Usta.UI.Source as Source
import Usta.UI.Source (Source)
import qualified Usta.UI.Window as Window

-- | @usta --gallery@ opens the component gallery instead of the app;
-- @usta --welcome@ and @usta --first-sync@ preview the first-run states;
-- @usta --sample@ runs the whole app against sample forges;
-- @usta git-credential get@ is git's credential helper for mirrors.
main :: IO ()
main = getArgs >>= choose
  where
    choose args
      | ["git-credential", action] <- args = Credential.helper Keyring.system action
      | has "--gallery" = runApp (void . Gallery.build)
      | has "--welcome" = runApp (void . Window.buildAt Window.FirstRun Source.samples)
      | has "--first-sync" = runApp (void . Window.buildAt Window.FirstSync Source.samples)
      | has "--sample" = sample
      | otherwise = cache
      where
        has = (`elem` args)

runApp :: (Application -> IO ()) -> IO ()
runApp open = do
  app <- new Application [ #applicationId := "io.github.abdivasiyev.Usta"
                         , On #activate (open ?self)
                         ]
  void $ #run app Nothing

-- | The real cache, with every saved account syncing on its keyring
-- token. Only Forgejo accounts sync until the other forges get clients.
cache :: IO ()
cache = do
  file <- databaseFile
  manager <- newTlsManager
  withDatabase file $ \pool -> do
    saved <- run pool listAccounts
    changes <- newBus
    statuses <- newBus
    sync <- Sync.start Sync.Settings {pool = pool, changes = changes, statuses = statuses, every = 60} []
    root <- Mirrors.defaultRoot
    mirrorStatuses <- newBus
    mirrors <- Mirrors.start Mirrors.Settings {pool = pool, changes = changes, statuses = mirrorStatuses, root = root}
    accounts <- Accounts.connect Accounts.Settings
      { manager = manager
      , keyring = Keyring.system
      , pool = pool
      , changes = changes
      , sync = sync
      , mirrors = mirrors
      , forgejoApi = \host -> apiUrl host "/api/v1"
      }
    let source = Source.withAccounts accounts (Source.fromDatabase pool changes statuses sync)
        start = if null saved then Window.FirstRun else Window.Ready
    runApp (void . Window.buildAt start source)

-- | Two sample accounts in a fresh cache file, synced each minute. Every
-- 90 seconds the sample forges change and sync runs at once, so the pages
-- can be seen updating. The cache is a file, not @:memory:@, since the pool
-- may close an idle connection and an in-memory database goes with it.
sample :: IO ()
sample = do
  file <- (</> "usta-sample.db") <$> getTemporaryDirectory
  removePathForcibly file
  withDatabase file $ \pool -> do
    now <- getCurrentTime
    github <- Account GitHub <$> parseM "github.com" <*> parseM "abdivasiyev" <*> pure AccessToken
    forgejo <- Account Forgejo <$> parseM "git.azizovich.uz" <*> parseM "abdivasiyev" <*> pure AccessToken
    samples <- traverse (\account -> do
                           key <- run pool (saveAccount now account)
                           s <- Sample.new now account
                           pure (key, s)) [github, forgejo]
    source <- fromPool pool [(key, s.forge) | (key, s) <- samples]
    void $ forkIO $ forever $ do
      threadDelay 90000000
      time <- getCurrentTime
      changed <- traverse (\(_, s) -> s.advance time) samples
      when (or changed) source.syncNow
    runApp (void . Window.build source)

-- | A source reading @pool@, with sync running for @forges@ every minute.
fromPool :: ConnectionPool -> [(AccountId, Forge)] -> IO Source
fromPool pool forges = do
  changes <- newBus
  statuses <- newBus
  sync <- Sync.start Sync.Settings {pool = pool, changes = changes, statuses = statuses, every = 60} forges
  pure (Source.fromDatabase pool changes statuses sync)
