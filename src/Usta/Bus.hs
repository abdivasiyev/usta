-- | The change bus: sync threads say what changed, and the window reloads
-- that from the database. Only what changed travels, never the data, so the
-- database stays the one copy of it.
module Usta.Bus
  ( Change (..)
  , Bus
  , newBus
  , publish
  , subscribe
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TChan
  , atomically
  , dupTChan
  , newBroadcastTChanIO
  , readTChan
  , writeTChan
  )
import Control.Monad (forever, void)
import Data.Kind (Type)
import qualified GI.GLib as GLib

import Usta.DB.Schema.Account (AccountId)
import Usta.Types.Resource (Resource)

-- | An account's copy of one resource was saved anew.
type Change :: Type
data Change = Change
  { accountId :: AccountId
  , resource :: Resource
  }
  deriving stock (Eq, Show)

type Bus :: Type
newtype Bus = Bus (TChan Change)

newBus :: IO Bus
newBus = Bus <$> newBroadcastTChanIO

-- | Safe from any thread; never blocks.
publish :: Bus -> Change -> IO ()
publish (Bus chan) change = atomically (writeTChan chan change)

-- | Runs @onChange@ for every change published from now on, in order, on
-- the GTK main thread, so it may touch widgets.
subscribe :: Bus -> (Change -> IO ()) -> IO ()
subscribe (Bus chan) onChange = do
  mine <- atomically (dupTChan chan)
  void $ forkIO $ forever $ do
    change <- atomically (readTChan mine)
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (onChange change >> pure False)
