{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Design tokens from docs/DESIGN.md and the stylesheet loader.
--
-- Components ship their own CSS and refer to the tokens below through
-- @var(--name)@, so a new component never hard-codes a color.
module Usta.UI.Style
  ( Tone (..)
  , toneClass
  , install
  , setTheme
  , themes
  , tokens
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.GI.Base (AttrOp ((:=)), SignalProxy (PropertyNotify), get, new, on, set)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (intPtrToPtr, nullPtr)
import qualified GI.GObject as GObject
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Usta.Preferences (Theme (..))
import qualified Usta.Types.Platform as Platform

foreign import ccall unsafe "usta_system_dark" systemDark :: IO CInt

-- | The meaning a colored element carries. Add a constructor here and a
-- matching @.tone-*@ rule in the component CSS to introduce a new one.
type Tone :: Type
data Tone
  = Neutral
  | Accent
  | Success
  | Warning
  | Danger
  | Merged
  deriving (Eq, Show, Enum, Bounded)

toneClass :: Tone -> Text
toneClass tone = case tone of
  Neutral -> "tone-neutral"
  Accent -> "tone-accent"
  Success -> "tone-success"
  Warning -> "tone-warning"
  Danger -> "tone-danger"
  Merged -> "tone-merged"

-- | Loads the tokens followed by every component's CSS for the display
-- the widget is on, once: each window used to add them again, and a
-- closed window's stayed.
install :: Gtk.Widget -> [Text] -> IO ()
install widget components = do
  display <- Gtk.widgetGetDisplay widget
  already <- GObject.objectGetData display "usta-css"
  when (already == nullPtr) $ installOn display components

installOn :: Gdk.Display -> [Text] -> IO ()
installOn display components = do
  GObject.objectSetData display "usta-css" (intPtrToPtr 1)
  provider <- new Gtk.CssProvider []
  Gtk.cssProviderLoadFromString provider (mconcat (tokens : components))
  Gtk.styleContextAddProviderForDisplay display provider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION)

  -- An app's provider does not follow the system color scheme on its own;
  -- without this the dark tokens below never apply.
  settings <- Gtk.settingsGetForDisplay display
  let sync = do
        scheme <- get settings #gtkInterfaceColorScheme
        set provider [#prefersColorScheme := scheme]
  sync
  void $ on settings (PropertyNotify #gtkInterfaceColorScheme) (const sync)

-- | Light, dark, or whatever the system uses; 'install' makes the tokens
-- follow. On Linux GTK reads the system's choice from the desktop, so
-- 'System' gives the setting back to it; on macOS GTK cannot, so usta
-- reads the appearance itself.
setTheme :: Theme -> IO ()
setTheme theme = do
  settings <- Gtk.settingsGetDefault
  forM_ settings $ \s -> case theme of
    Light -> choose s Gtk.InterfaceColorSchemeLight
    Dark -> choose s Gtk.InterfaceColorSchemeDark
    System
      | Platform.current == Platform.MacOS -> do
          dark <- (/= 0) <$> systemDark
          choose s (if dark then Gtk.InterfaceColorSchemeDark else Gtk.InterfaceColorSchemeLight)
      | otherwise -> Gtk.settingsResetProperty s "gtk-interface-color-scheme"
  where
    -- Only a change is set, so pages repaint only when the theme changes.
    choose s scheme = do
      now <- get s #gtkInterfaceColorScheme
      unless (now == scheme) $ set s [#gtkInterfaceColorScheme := scheme]

-- | 'setTheme' for a window, and what stops it when the window goes:
-- while the choice is 'System', macOS's appearance is read again every
-- two seconds, so switching it applies without restarting usta.
themes :: IO (Theme -> IO (), IO ())
themes = do
  chosen <- newIORef System
  watch <- if Platform.current /= Platform.MacOS then pure Nothing else
    fmap Just $ GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT 2 $ do
      theme <- readIORef chosen
      when (theme == System) (setTheme System)
      pure True
  pure (\theme -> writeIORef chosen theme >> setTheme theme, mapM_ (void . GLib.sourceRemove) watch)

tokens :: Text
tokens = mconcat
  [ "window {"
  , "  --window: #FAFAFA; --surface: #FFFFFF; --sidebar: #F1F2F4;"
  , "  --border: #E2E4E8; --border-subtle: #EEF0F3; --input-border: #D5D8DE;"
  , "  --input-bg: #F4F5F7;"
  , "  --text: #1B1D21; --text-muted: #5B616B; --text-soft: #3D434C;"
  , "  --accent: #2F5FD0; --on-accent: #FFFFFF;"
  , "  --selected: #EEF3FD; --nav-active: #E3E8F5;"
  , "  --success: #1F7A3A; --success-bg: #E3F1E7;"
  , "  --warning: #8A5A00; --warning-bg: #FFF4E0;"
  , "  --danger: #B42318; --danger-bg: #FDECEA; --danger-border: #F3C1BB;"
  , "  --merged: #5B21B6; --merged-bg: #EFE9FB;"
  , "  --accent-soft: #1F47A8; --accent-bg: #E3E8F5;"
  , "  --neutral-bg: #EEF0F3;"
  , "}"
  , "@media (prefers-color-scheme: dark) {"
  , "  window {"
  , "    --window: #16181C; --surface: #1E2126; --sidebar: #191B1F;"
  , "    --border: #2E3238; --border-subtle: #262A30; --input-border: #3A3F47;"
  , "    --input-bg: #24272D;"
  , "    --text: #E6E8EC; --text-muted: #9AA0A9; --text-soft: #C3C7CE;"
  , "    --accent: #7AA5FF; --on-accent: #0E1420;"
  , "    --selected: #22314F; --nav-active: #2A3550;"
  , "    --success: #5BD38A; --success-bg: #1C3326;"
  , "    --warning: #F2C25C; --warning-bg: #3A2F14;"
  , "    --danger: #F28B82; --danger-bg: #3E1F1E; --danger-border: #5A2A27;"
  , "    --merged: #C4A7FF; --merged-bg: #2F2545;"
  , "    --accent-soft: #A9C2FF; --accent-bg: #23304D;"
  , "    --neutral-bg: #2A2E35;"
  , "  }"
  , "}"
  , "window.usta {"
  , "  background-color: var(--window); color: var(--text);"
  , "  font-family: Inter, sans-serif; font-size: 14px;"
  , "}"
  , ".muted { color: var(--text-muted); }"
  , ".soft { color: var(--text-soft); }"
  , ".mono { font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  -- Hover, press and selection ease in instead of snapping.
  , "button, row, entry, checkbutton check, radio, switch {"
  , "  transition: background-color 120ms ease-out, box-shadow 120ms ease-out, color 120ms ease-out, border-color 120ms ease-out;"
  , "}"
  ]
