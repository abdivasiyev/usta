{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | What differs per operating system in the UI: window controls and how
-- keyboard shortcuts are written. 'Platform' itself lives in
-- "Usta.Types.Platform" and is re-exported here.
module Usta.UI.Platform
  ( Platform (..)
  , current
  , canNotify
  , shortcut
  , configureHeaderBar
  , dialogOver
  , keys
  ) where

import Control.Monad (forM_, void)
import Data.GI.Base (AttrOp ((:=)), set)
import qualified Data.GI.Base as GI
import Data.GI.Base.ManagedPtr (withManagedPtr)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.Ptr (Ptr)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

import Usta.Types.Platform (Platform (..), canNotify, current)

-- | A shortcut on the primary modifier: "⌘K" on macOS, "Ctrl K" elsewhere.
-- Keybindings write @<Primary>@ and go through 'keys'.
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

-- | Opens @dialog@ over @parent@: on top of it and modal to it, as GTK
-- does; on macOS also as a child window, so a full-screen usta shows it
-- over itself rather than on a full-screen space of its own.
dialogOver :: Gtk.Window -> Maybe Gtk.Window -> IO ()
dialogOver dialog parent = forM_ parent $ \p -> do
  Gtk.windowSetTransientFor dialog (Just p)
  case current of
    MacOS -> do
      let both act = do
            mine <- Gtk.nativeGetSurface dialog
            theirs <- Gtk.nativeGetSurface p
            case (mine, theirs) of
              (Just a, Just b) -> withManagedPtr a $ \pa -> withManagedPtr b $ \pb -> act pa pb
              _ -> pure ()
      -- Ready before macOS first shows it, then a child once shown.
      void $ GI.after dialog #realize $
        Gtk.nativeGetSurface dialog >>= mapM_ (\a -> withManagedPtr a c_prepare)
      void $ GI.on dialog #map (both c_attach)
      void $ GI.on dialog #unmap (both c_detach)
    _ -> pure ()
  -- Closed, the dialog hands the focus back to the window it was over, so
  -- usta never ends up with no window in front.
  void $ GI.on dialog #destroy (Gtk.windowPresent p)

foreign import ccall unsafe "usta_prepare_dialog" c_prepare :: Ptr Gdk.Surface -> IO ()
foreign import ccall unsafe "usta_attach_dialog" c_attach :: Ptr Gdk.Surface -> Ptr Gdk.Surface -> IO ()
foreign import ccall unsafe "usta_detach_dialog" c_detach :: Ptr Gdk.Surface -> Ptr Gdk.Surface -> IO ()

-- | A shortcut as GTK should read it. GTK reads @<Primary>@ as Control
-- even on macOS, where the primary modifier is Command (@<Meta>@ to GTK).
keys :: Text -> Text
keys trigger = case current of
  MacOS -> T.replace "<Primary>" "<Meta>" trigger
  _ -> trigger
