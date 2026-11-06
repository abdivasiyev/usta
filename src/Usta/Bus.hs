-- | Buses from sync threads to the window: a 'Bus' 'Change' says what
-- changed, and the window reloads that from the database. Only what changed
-- travels, never the data, so the database stays the one copy of it.
module Usta.Bus
  ( Change (..)
  , Bus
  , newBus
  , publish
  , subscribe
  , listen
  ) where

import Control.Concurrent (forkIO, killThread)
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

type Bus :: Type -> Type
newtype Bus a = Bus (TChan a)

newBus :: IO (Bus a)
newBus = Bus <$> newBroadcastTChanIO

-- | Safe from any thread; never blocks.
publish :: Bus a -> a -> IO ()
publish (Bus chan) message = atomically (writeTChan chan message)

-- | Runs @onMessage@ for every message published from now on, in order, on
-- the GTK main thread, so it may touch widgets; until the action it gives
-- is run, as when the window it updates closes.
subscribe :: Bus a -> (a -> IO ()) -> IO (IO ())
subscribe bus onMessage = do
  next <- listen bus
  thread <- forkIO $ forever $ do
    message <- next
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (onMessage message >> pure False)
  pure (killThread thread)

-- | The messages published from now on, one per call, blocking until the
-- next arrives; for threads other than the window's, such as tests.
listen :: Bus a -> IO (IO a)
listen (Bus chan) = do
  mine <- atomically (dupTChan chan)
  pure (atomically (readTChan mine))
