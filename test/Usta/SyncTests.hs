{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Syncing a forge into the cache, on the sample forge: what lands in
-- the cache, what is said to have changed, and what a failure leaves.
module Usta.SyncTests (tests, withCache, githubAccount) where

import Control.Exception (bracket)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Usta.Bus (Change (..), listen, newBus)
import Usta.DB (run, withDatabase)
import Usta.DB.Account (saveAccount)
import Usta.DB.Issue (issues)
import Usta.DB.Notification (InboxItem (..), inbox, setDone)
import Usta.DB.PullRequest (pullRequests)
import Usta.DB.Repository (repositories, saveRepositories)
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (Failure (..), Fetched (..), Forge (..), Known (..))
import qualified Usta.Forge.Sample as Sample
import Usta.Sync (State (..), Status (..), Sync (..))
import qualified Usta.Sync as Sync
import Usta.Types.Account (Account (..), AuthMethod (..))
import qualified Usta.Types.Forge as Kind
import Usta.Types.Notification (Notification (..))
import Usta.Types.Parse (parseM)
import Usta.Types.Resource (Resource (..))

tests :: TestTree
tests =
  testGroup
    "Sync"
    [ testCase "the first sync fills the cache with the account's lists" $ synced $ \t -> do
        counts t.pool >>= (@?= (4, 3, 4, 1))
        sortOn fromEnum <$> drain t.changes >>= (@?= [Notifications, PullRequests, Issues, Repositories])
    , testCase "a sync when nothing changed says nothing changed" $ synced $ \t -> do
        _ <- drain t.changes
        again t
        drain t.changes >>= (@?= [])
    , testCase "a change on another host changes nothing here" $ synced $ \t -> do
        _ <- drain t.changes
        _ <- getCurrentTime >>= t.sample.advance
        again t
        drain t.changes >>= (@?= [])
    , testCase "a change here says which list changed" $ synced $ \t -> do
        _ <- drain t.changes
        now <- getCurrentTime
        _ <- t.sample.advance now
        _ <- t.sample.advance now
        again t
        sortOn fromEnum <$> drain t.changes >>= (@?= [Notifications, PullRequests])
        counts t.pool >>= (@?= (5, 4, 4, 1))
    , testCase "an offline forge fails the sync and keeps the cache" $ synced $ \t -> do
        t.sample.failWith (Just Offline)
        t.sync.syncNow
        waitFor t (== Failed Offline)
        counts t.pool >>= (@?= (4, 3, 4, 1))
        t.sample.failWith Nothing
        again t
    , testCase "a forge that refuses the token says to sign in again" $ synced $ \t -> do
        t.sample.failWith (Just SignedOut)
        t.sync.syncNow
        waitFor t (== Failed SignedOut)
    , testCase "done stays done until the thread has news" $ synced $ \t -> do
        thread <- parseM "1"
        run t.pool (setDone t.account thread True)
        writeIORef t.refetch (Just Nothing)
        again t
        doneOf t.pool "1" >>= (@?= Just True)
    , testCase "news on a thread marked done brings it back" $ withForge (\_ s -> s) $ \t -> do
        thread <- parseM "1"
        run t.pool (setDone t.account thread True)
        now <- getCurrentTime
        writeIORef t.refetch (Just (Just now))
        again t
        doneOf t.pool "1" >>= (@?= Just False)
    , testCase "a whole list forgets what the forge no longer has" $
        withForgeAfter sampleRepositories (\_ f -> f {repositories = \_ -> pure Fetched {etag = Nothing, items = [], whole = True}}) $ \t ->
          counts t.pool >>= \(_, _, _, n) -> n @?= 0
    , testCase "a list that is not whole forgets nothing" $
        withForgeAfter sampleRepositories (\_ f -> f {repositories = \_ -> pure Fetched {etag = Nothing, items = [], whole = False}}) $ \t ->
          counts t.pool >>= \(_, _, _, n) -> n @?= 1
    , testCase "an account removed reports nothing more" $ synced $ \t -> do
        t.sync.remove t.account
        statuses <- t.sync.latest
        assertBool "still reported" (all ((/= t.account) . (.accountId)) statuses)
    ]

-- | What a test has: the cache, the sample forge, the running sync.
data Running = Running
  { pool :: ConnectionPool
  , account :: AccountId
  , sample :: Sample.Sample
  , sync :: Sync
  , changes :: IO (Maybe Resource)
  , statuses :: IO Status
  , refetch :: IORef (Maybe (Maybe UTCTime))
    -- ^ While set, notifications are fetched whole each time, updated at
    -- the time when there is one.
  }

-- | A cache in a file of its own.
withCache :: (ConnectionPool -> IO a) -> IO a
withCache act = withSystemTempDirectory "usta-cache" $ \dir -> withDatabase (dir </> "usta.db") act

githubAccount :: IO Account
githubAccount = Account Kind.GitHub <$> parseM "github.com" <*> parseM "abdivasiyev" <*> pure AccessToken

synced :: (Running -> IO ()) -> IO ()
synced = withForge (\_ s -> s)

-- | Starts syncing the sample forge, changed by @adapt@, and waits for
-- its first sync.
withForge :: (Sample.Sample -> Forge -> Forge) -> (Running -> IO ()) -> IO ()
withForge = withForgeAfter (\_ _ -> pure ())

-- | The sample's repositories, saved as an earlier sync would have.
sampleRepositories :: ConnectionPool -> AccountId -> IO ()
sampleRepositories pool key = do
  now <- getCurrentTime
  account <- githubAccount
  sample <- Sample.new now account
  sample.forge.repositories Known {etag = Nothing, since = Nothing} >>= \case
    Fetched {items} -> run pool (saveRepositories key now items)
    NotModified -> pure ()

-- | 'withForge', with the cache given what @before@ saves first.
withForgeAfter :: (ConnectionPool -> AccountId -> IO ()) -> (Sample.Sample -> Forge -> Forge) -> (Running -> IO ()) -> IO ()
withForgeAfter before adapt test = withCache $ \pool -> do
  now <- getCurrentTime
  account <- githubAccount
  key <- run pool (saveAccount now account)
  before pool key
  sample <- Sample.new now account
  refetch <- newIORef Nothing
  changeBus <- newBus
  statusBus <- newBus
  nextChange <- listen changeBus
  nextStatus <- listen statusBus
  let forge = (adapt sample sample.forge) {notifications = \known -> readIORef refetch >>= \case
        Nothing -> sample.forge.notifications known
        Just at -> sample.forge.notifications Known {etag = Nothing, since = known.since} >>= \case
          Fetched {etag, items, whole} -> pure Fetched {etag = etag, items = [maybe n (\time -> n {updatedAt = time}) at | n <- items], whole = whole}
          NotModified -> pure NotModified}
      settings = Sync.Settings {pool = pool, changes = changeBus, statuses = statusBus, every = 3600}
  bracket (Sync.start settings [(key, forge)]) (.stop) $ \s -> do
    let t = Running
          { pool = pool, account = key, sample = sample, sync = s
          , changes = timeout 300000 (fmap (.resource) nextChange)
          , statuses = nextStatus, refetch = refetch }
    waitFor t isSynced
    test t

isSynced :: State -> Bool
isSynced = \case
  Synced _ -> True
  _ -> False

-- | Syncs now and waits until it went through.
again :: Running -> IO ()
again t = t.sync.syncNow >> waitFor t (== Syncing) >> waitFor t isSynced

waitFor :: Running -> (State -> Bool) -> IO ()
waitFor t wanted = timeout 10000000 go >>= maybe (assertFailure "the sync never got there") pure
  where
    go = t.statuses >>= \s -> if wanted s.state then pure () else go

-- | Every change said since the last time, by its list.
drain :: IO (Maybe Resource) -> IO [Resource]
drain next = next >>= maybe (pure []) (\r -> (r :) <$> drain next)

-- | Notifications in the inbox, pull requests, issues, repositories.
counts :: ConnectionPool -> IO (Int, Int, Int, Int)
counts pool = run pool $ (,,,) <$> (length <$> inbox) <*> (length <$> pullRequests) <*> (length <$> issues) <*> (length <$> repositories)

doneOf :: ConnectionPool -> String -> IO (Maybe Bool)
doneOf pool thread = do
  t <- parseM (T.pack thread)
  items <- run pool inbox
  pure (lookup t [(i.notification.threadId, i.done) | i <- items])
