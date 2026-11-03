{-# LANGUAGE OverloadedStrings #-}

-- | Keeps the database a copy of every account's forge: one thread per
-- account syncs its lists, then waits for the interval or 'syncNow'.
-- Accounts can be added and removed while it runs.
module Usta.Sync
  ( State (..)
  , Status (..)
  , Settings (..)
  , Sync (..)
  , start
  ) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, orElse, readTVar, readTVarIO, registerDelay, retry, writeTVar)
import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Control.Exception (Handler (..), SomeAsyncException, SomeException, catches, displayException, fromException, throwIO)
import Data.Kind (Type)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Database.Persist.Sql (ConnectionPool)

import Usta.Bus (Bus, Change (..), publish)
import Usta.DB (Result, run)
import Usta.DB.Issue (saveIssues)
import Usta.DB.Notification (saveNotifications)
import Usta.DB.PullRequest (savePullRequests)
import Usta.DB.Repository (saveRepositories)
import Usta.DB.Schema.Account (AccountId)
import Usta.DB.SyncCursor (Cursor (..), cursor, saveCursor)
import Usta.Forge (Failure (..), Fetched (..), Forge (..))
import Usta.Types.ETag (ETag)
import Usta.Types.Resource (Resource (..))

type State :: Type
data State
  = Syncing
  | Synced UTCTime
  | Failed Failure
    -- ^ Tried again at the next interval or 'syncNow'.
  deriving stock (Eq, Show)

type Status :: Type
data Status = Status
  { accountId :: AccountId
  , state :: State
  }
  deriving stock (Eq, Show)

type Settings :: Type
data Settings = Settings
  { pool :: ConnectionPool
  , changes :: Bus Change
    -- ^ Gets a change for every list saved anew.
  , statuses :: Bus Status
  , every :: NominalDiffTime
    -- ^ How long an account waits between syncs.
  }

type Sync :: Type
data Sync = Sync
  { syncNow :: IO ()
    -- ^ Every account syncs now instead of at its next interval.
  , add :: AccountId -> Forge -> IO ()
    -- ^ Starts syncing an account at once; replaces its forge if it was
    -- already syncing, as after signing in again.
  , remove :: AccountId -> IO ()
    -- ^ Stops syncing an account. When it returns, nothing more is
    -- reported for it.
  , report :: Status -> IO ()
    -- ^ Tells of an account that cannot sync at all, such as one whose
    -- token is gone.
  , stop :: IO ()
  , latest :: IO [Status]
  , setEvery :: Maybe NominalDiffTime -> IO ()
    -- ^ A new interval, 'Nothing' for syncing only when asked. Every
    -- account syncs at once and then keeps the new pace.
    -- ^ Each account's last status, for whoever starts listening late.
  }

start :: Settings -> [(AccountId, Forge)] -> IO Sync
start settings accounts = do
  wake <- newTVarIO (0 :: Int)
  every <- newTVarIO (Just settings.every)
  known <- newTVarIO Map.empty
  threads <- newTVarIO Map.empty
  let report status = do
        atomically (modifyTVar' known (Map.insert status.accountId status))
        publish settings.statuses status
      remove key = do
        old <- atomically $ do
          running <- readTVar threads
          modifyTVar' threads (Map.delete key)
          pure (Map.lookup key running)
        -- killThread returns once the thread has stopped.
        mapM_ killThread old
        atomically (modifyTVar' known (Map.delete key))
      add key forge = do
        remove key
        thread <- forkIO (worker settings report wake every (key, forge))
        atomically (modifyTVar' threads (Map.insert key thread))
  mapM_ (uncurry add) accounts
  pure Sync
    { syncNow = atomically (modifyTVar' wake (+ 1))
    , add = add
    , remove = remove
    , report = report
    , stop = readTVarIO threads >>= mapM_ killThread
    , latest = toList <$> readTVarIO known
    , setEvery = \new -> atomically $ do
        writeTVar every new
        modifyTVar' wake (+ 1)
    }

-- | Syncs, then waits for the interval (if any) or a bump of @wake@,
-- forever.
worker :: Settings -> (Status -> IO ()) -> TVar Int -> TVar (Maybe NominalDiffTime) -> (AccountId, Forge) -> IO ()
worker settings report wake every (key, forge) = readTVarIO wake >>= loop
  where
    loop seen = do
      syncAccount settings report key forge
      pace <- readTVarIO every
      timeout <- traverse (\s -> registerDelay (floor (s * 1000000))) pace
      next <- atomically $
        (readTVar wake >>= \n -> check (n /= seen) >> pure n)
          `orElse` (maybe retry (\t -> readTVar t >>= check) timeout >> readTVar wake)
      loop next

-- | One sync of every list, stopping at the first failure.
syncAccount :: Settings -> (Status -> IO ()) -> AccountId -> Forge -> IO ()
syncAccount settings report key forge = do
  report Status {accountId = key, state = Syncing}
  outcome <- attempt $ do
    list Notifications forge.notifications (const (saveNotifications key))
    list PullRequests forge.pullRequests (savePullRequests key)
    list Issues forge.issues (saveIssues key)
    list Repositories forge.repositories (saveRepositories key)
  time <- getCurrentTime
  report Status {accountId = key, state = either Failed (const (Synced time)) outcome}
  where
    list :: Resource -> (Maybe ETag -> IO (Fetched [a])) -> (UTCTime -> [a] -> Result ()) -> IO ()
    list resource fetch save = do
      known <- run settings.pool (cursor key resource)
      let knownTag = known >>= (.etag)
      fetched <- fetch knownTag
      time <- getCurrentTime
      case fetched of
        NotModified ->
          run settings.pool (saveCursor key resource Cursor {etag = knownTag, syncedAt = time})
        Fetched {etag = tag, items = items} -> do
          run settings.pool $ do
            save time items
            saveCursor key resource Cursor {etag = tag, syncedAt = time}
          publish settings.changes Change {accountId = key, resource = resource}

-- | Runs a sync, turning what can go wrong into a 'Failure'. Async
-- exceptions, such as 'stop' killing the thread, pass through.
attempt :: IO () -> IO (Either Failure ())
attempt action =
  (Right <$> action)
    `catches`
      [ Handler (pure . Left)
      , Handler $ \(e :: SomeException) -> case fromException e of
          Just (async :: SomeAsyncException) -> throwIO async
          Nothing -> pure (Left (Refused (T.pack (displayException e))))
      ]
