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
  , tokens
  ) where

import Control.Monad (forM_, void)
import Data.GI.Base (AttrOp ((:=)), SignalProxy (PropertyNotify), get, new, on, set)
import Data.Kind (Type)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import Usta.Preferences (Theme (..))

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
-- the widget is on.
install :: Gtk.Widget -> [Text] -> IO ()
install widget components = do
  provider <- new Gtk.CssProvider []
  Gtk.cssProviderLoadFromString provider (mconcat (tokens : components))
  display <- Gtk.widgetGetDisplay widget
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
-- follow.
setTheme :: Theme -> IO ()
setTheme theme = do
  settings <- Gtk.settingsGetDefault
  forM_ settings $ \s -> set s [#gtkInterfaceColorScheme := scheme]
  where
    scheme = case theme of
      System -> Gtk.InterfaceColorSchemeDefault
      Light -> Gtk.InterfaceColorSchemeLight
      Dark -> Gtk.InterfaceColorSchemeDark

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
  ]
