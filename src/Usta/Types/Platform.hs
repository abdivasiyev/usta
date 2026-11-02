-- | Which operating system usta runs on. Decides window controls and how
-- shortcuts are written in the UI, and where the cache lives.
module Usta.Types.Platform
  ( Platform (..)
  , current
  ) where

import Data.Kind (Type)
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
