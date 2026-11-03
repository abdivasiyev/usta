-- | Which operating system usta runs on. Decides window controls and how
-- shortcuts are written in the UI, and where the cache lives.
module Usta.Types.Platform
  ( Platform (..)
  , current
  , canNotify
  ) where

import Data.Kind (Type)
import qualified Data.List as List
import System.Environment (getExecutablePath)
import qualified System.Info

type Platform :: Type
data Platform
  = MacOS
  | Linux
  | Windows
  | OtherOS
  deriving stock (Eq, Show)

current :: Platform
current = case System.Info.os of
  "darwin" -> MacOS
  "linux" -> Linux
  "mingw32" -> Windows
  _ -> OtherOS

-- | Whether the desktop takes notifications from usta. On macOS GLib shows
-- them only for an app bundle; outside one it falls back to D-Bus, which
-- macOS does not have, so nothing shows and GLib prints warnings.
canNotify :: IO Bool
canNotify = case current of
  MacOS -> List.isInfixOf ".app/Contents/MacOS/" <$> getExecutablePath
  _ -> pure True
