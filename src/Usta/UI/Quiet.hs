-- | Forge calls that show their own progress, as a button's spinner or a
-- conversation drawn again in place: while one runs, the window's thin
-- activity bar stays away, so the page does not flicker for it.
module Usta.UI.Quiet
  ( quietly
  , hushed
  ) where

import Control.Exception (bracket_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

hush :: IORef Int
hush = unsafePerformIO (newIORef 0)
{-# NOINLINE hush #-}

quietly :: IO a -> IO a
quietly = bracket_ (atomicModifyIORef' hush (\n -> (n + 1, ()))) (atomicModifyIORef' hush (\n -> (n - 1, ())))

-- | Whether a quiet call is running.
hushed :: IO Bool
hushed = (> 0) <$> readIORef hush
