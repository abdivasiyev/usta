{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | What differs per operating system: window controls and how keyboard
-- shortcuts are written.
module Usta.UI.Platform
  ( Platform (..)
  , current
  , shortcut
  , configureHeaderBar
  ) where

import Data.GI.Base (AttrOp ((:=)), set)
import Data.Kind (Type)
import Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified System.Info

type Platform :: Type
data Platform
  = MacOS
  | Linux
  | Windows
  | OtherOS
  deriving (Eq, Show)

current :: Platform
current = case System.Info.os of
  "darwin" -> MacOS
  "linux" -> Linux
  "mingw32" -> Windows
  _ -> OtherOS

-- | A shortcut on the primary modifier: "⌘K" on macOS, "Ctrl K" elsewhere.
-- Keybindings themselves use GTK's @<Primary>@, which maps the same way.
shortcut :: Platform -> Text -> Text
shortcut MacOS key = "⌘" <> key
shortcut _ key = "Ctrl " <> key

-- | Window controls the way each system draws them.
configureHeaderBar :: Platform -> Gtk.HeaderBar -> IO ()
configureHeaderBar platform header = case platform of
  -- The real traffic lights, on the left.
  MacOS -> set header [ #useNativeControls := True
                      , #decorationLayout := "close,minimize,maximize:"
                      ]
  Windows -> set header [#decorationLayout := "icon:minimize,maximize,close"]
  -- On Linux the desktop's gtk-decoration-layout setting decides.
  _ -> pure ()
