{-# LANGUAGE OverloadedStrings #-}

-- | Keeps the chosen mirrors up to date: one background thread clones or
-- fetches them one at a time, so git never runs many times at once. A
-- mirror is updated when usta starts, when sync saves its account's pull
-- requests, and as soon as the user chooses it.
module Usta.Mirrors
  ( State (..)
  , Status (..)
  , Settings (..)
  , Mirrors (..)
  , defaultRoot
  , start
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, modifyTVar', newTQueueIO, newTVarIO, readTQueue, readTVarIO, writeTQueue)
import Control.Exception (try)
import Control.Monad (forM_, forever, void, when)
import qualified Data.Map.Strict as Map
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import System.Directory (XdgDirectory (XdgCache), doesFileExist, getHomeDirectory, getXdgDirectory)
import System.FilePath ((</>))

import Usta.Bus (Bus, Change (..), listen, publish)
import Usta.DB (run)
import Usta.DB.Mirror (Wanted (..), markFetched, mirrored)
import Usta.DB.Schema.Account (AccountId)
import qualified Usta.Git as Git
import Usta.Types.Host (Host)
import Usta.Types.Parse (unparse)
import qualified Usta.Types.Platform as Platform
import Usta.Types.RepoName (RepoName)
import Usta.Types.Resource (Resource (..))

type State :: Type
data State
  = Cloning
  | Fetching
  | Ready UTCTime
    -- ^ Up to date as of then.
  | Failed Text
    -- ^ git's message; tried again at the next update.
  deriving stock (Eq, Show)

type Status :: Type
data Status = Status
  { accountId :: AccountId
  , repo :: RepoName
  , state :: State
  }
  deriving stock (Eq, Show)

type Settings :: Type
data Settings = Settings
  { pool :: ConnectionPool
  , changes :: Bus Change
    -- ^ Heard for each account's pull requests being saved.
  , statuses :: Bus Status
  , root :: FilePath
    -- ^ Mirrors are at @root/host/owner/repo.git@.
  }

type Mirrors :: Type
data Mirrors = Mirrors
  { ensure :: AccountId -> RepoName -> IO ()
    -- ^ Clones or fetches one repository soon, if it is chosen for a
    -- mirror; for right after the user chose it.
  , ready :: AccountId -> RepoName -> IO (Maybe FilePath)
    -- ^ The mirror's path once it has been cloned; 'Nothing' before, or
    -- when the repository is not mirrored, so the API is used instead.
  , latest :: IO [Status]
  }

-- | The platform's cache folder: macOS may empty it when the disk is full,
-- which only means mirrors are cloned again.
defaultRoot :: IO FilePath
defaultRoot = case Platform.current of
  Platform.MacOS -> (</> "Library" </> "Caches" </> "usta" </> "mirrors") <$> getHomeDirectory
  _ -> (</> "mirrors") <$> getXdgDirectory XdgCache "usta"

-- | Which mirrors a job updates.
type Job :: Type
data Job = Everything | OfAccount AccountId | One AccountId RepoName

start :: Settings -> IO Mirrors
start settings = do
  jobs <- newTQueueIO
  known <- newTVarIO Map.empty
  let report status = do
        atomically (modifyTVar' known (Map.insert (status.accountId, status.repo) status))
        publish settings.statuses status
  void $ forkIO $ forever $ atomically (readTQueue jobs) >>= work settings report
  next <- listen settings.changes
  void $ forkIO $ forever $ do
    change <- next
    when (change.resource == PullRequests) $ atomically (writeTQueue jobs (OfAccount change.accountId))
  atomically (writeTQueue jobs Everything)
  pure Mirrors
    { ensure = \key repo -> atomically (writeTQueue jobs (One key repo))
    , ready = \key repo -> do
        wanted <- run settings.pool mirrored
        case [w | w <- wanted, w.accountId == key, w.repo == repo] of
          w : _ -> do
            let path = pathOf settings.root w.host w.repo
            cloned <- doesFileExist (path </> "HEAD")
            pure (if cloned then Just path else Nothing)
          [] -> pure Nothing
    , latest = Map.elems <$> readTVarIO known
    }

-- | Brings the job's mirrors up to date, one after another. A failure is
-- reported and the rest go on.
work :: Settings -> (Status -> IO ()) -> Job -> IO ()
work settings report job = do
  wanted <- run settings.pool mirrored
  forM_ (filter chosen wanted) $ \w -> do
    let path = pathOf settings.root w.host w.repo
        tell state = report Status {accountId = w.accountId, repo = w.repo, state = state}
    cloned <- doesFileExist (path </> "HEAD")
    tell (if cloned then Fetching else Cloning)
    outcome <- try (if cloned then Git.fetch path else Git.clone w.kind w.url path)
    case outcome of
      Left (Git.GitError message) -> tell (Failed message)
      Right () -> do
        now <- getCurrentTime
        run settings.pool (markFetched now w.accountId w.repo)
        tell (Ready now)
  where
    chosen w = case job of
      Everything -> True
      OfAccount key -> w.accountId == key
      One key repo -> w.accountId == key && w.repo == repo

pathOf :: FilePath -> Host -> RepoName -> FilePath
pathOf root host repo = root </> T.unpack (unparse host) </> T.unpack (unparse repo) <> ".git"
