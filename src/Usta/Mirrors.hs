{-# LANGUAGE LambdaCase #-}
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
  , prune
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically, modifyTVar', newTQueueIO, newTVarIO, readTQueue, readTVarIO, writeTQueue)
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (filterM, forM_, forever, when)
import qualified Data.Map.Strict as Map
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import System.Directory (XdgDirectory (XdgCache), doesDirectoryExist, doesFileExist, getHomeDirectory, getXdgDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath (takeExtension, (</>))

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
  , stop :: IO ()
    -- ^ Stops updating mirrors; one being cloned is left as it is.
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
  worker <- forkIO $ forever $ atomically (readTQueue jobs) >>= work settings report
  next <- listen settings.changes
  watcher <- forkIO $ forever $ do
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
    , stop = killThread watcher >> killThread worker
    }

-- | Brings the job's mirrors up to date, one after another. A failure is
-- reported and the rest go on.
work :: Settings -> (Status -> IO ()) -> Job -> IO ()
work settings report job = do
  wanted <- run settings.pool mirrored
  forM_ (filter chosen wanted) $ \w -> do
    let path = pathOf settings.root w.host w.repo
        tell state = report Status {accountId = w.accountId, repo = w.repo, state = state}
    -- Whatever fails (git, the disk, the cache) fails this mirror only:
    -- the worker goes on with the next one and the next job.
    outcome <- tryAny $ do
      cloned <- doesFileExist (path </> "HEAD")
      tell (if cloned then Fetching else Cloning)
      if cloned then Git.fetch path else Git.clone w.kind w.url path
      now <- getCurrentTime
      run settings.pool (markFetched now w.accountId w.repo)
      pure now
    tell (either Failed Ready outcome)
  where
    chosen w = case job of
      Everything -> True
      OfAccount key -> w.accountId == key
      One key repo -> w.accountId == key && w.repo == repo

-- | Its words for whatever failed; git's own for git. Stopping passes
-- through.
tryAny :: IO a -> IO (Either Text a)
tryAny act = try act >>= \case
  Right a -> pure (Right a)
  Left (e :: SomeException)
    | Just (async :: SomeAsyncException) <- fromException e -> throwIO async
    | Just (Git.GitError message) <- fromException e -> pure (Left message)
    | otherwise -> pure (Left (T.pack (displayException e)))

-- | Deletes the clones under @root@ that no chosen mirror uses any more,
-- such as those of a removed account or a repository set to the API, and
-- gives how many went.
prune :: ConnectionPool -> FilePath -> IO Int
prune pool root = do
  wanted <- map (\w -> pathOf root w.host w.repo) <$> run pool mirrored
  hosts <- folders root
  owners <- concat <$> traverse folders hosts
  clones <- concat <$> traverse folders owners
  let unused = [c | c <- clones, takeExtension c == ".git", c `notElem` wanted]
  mapM_ removeDirectoryRecursive unused
  pure (length unused)
  where
    folders dir = do
      exists <- doesDirectoryExist dir
      if not exists then pure [] else do
        names <- listDirectory dir
        filterM doesDirectoryExist (map (dir </>) names)

pathOf :: FilePath -> Host -> RepoName -> FilePath
pathOf root host repo = root </> T.unpack (unparse host) </> T.unpack (unparse repo) <> ".git"
