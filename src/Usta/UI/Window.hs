{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The main window: header bar on top, pages in the middle, the shortcut
-- strip at the bottom.
module Usta.UI.Window
  ( build
  ) where

import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.NavItem as NavItem
import qualified Usta.UI.Components.SearchField as SearchField
import qualified Usta.UI.Components.Shortcut as Shortcut
import qualified Usta.UI.Platform as Platform
import qualified Usta.UI.Sidebar as Sidebar
import Usta.UI.Style (Tone (..))
import qualified Usta.UI.Style as Style

build :: Gtk.Application -> IO Gtk.ApplicationWindow
build app = do
  header <- headerBar Platform.current

  pages <- GI.new Gtk.Stack [#hexpand := True, #vexpand := True]
  forM_ [minBound .. maxBound] $ \page -> do
    -- Each page gets its own screen next; until then a placeholder.
    placeholder <- GI.new Gtk.Label [#label := Sidebar.pageTitle page, #cssClasses := ["muted"]]
    void $ Gtk.stackAddNamed pages placeholder (Just (Sidebar.pageName page))

  sidebar <- Sidebar.new Sidebar.sample Sidebar.Inbox $ \page ->
    Gtk.stackSetVisibleChildName pages (Sidebar.pageName page)

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
  #append body sidebar
  #append body pages

  footer <- Shortcut.bar
    [("j k", "move"), ("Enter", "open"), ("e", "done"), ("s", "star"), ("o", "open in browser")]
    (Just "2 forges · 3 accounts")

  root <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append root header
  #append root body
  #append root footer

  -- GTK hides the titlebar widget in fullscreen, and ours holds search and
  -- sync. So the header lives in the content, and an empty hidden titlebar
  -- stops GTK from adding its own.
  noTitlebar <- GI.new Gtk.Box [#visible := False]

  window <- GI.new Gtk.ApplicationWindow [ #application := app
                                         , #title := "usta"
                                         , #defaultWidth := 1280
                                         , #defaultHeight := 800
                                         , #titlebar := noTitlebar
                                         , #child := root
                                         , #cssClasses := ["usta"]
                                         ]
  Gtk.toWidget window >>= \w -> Style.install w styles
  #present window
  -- GTK focuses the first entry; start with nothing focused so single-key
  -- shortcuts (j, k, e) work instead of typing into the search field.
  Gtk.windowSetFocus window (Nothing :: Maybe Gtk.Widget)
  pure window

headerBar :: Platform.Platform -> IO Gtk.HeaderBar
headerBar platform = do
  header <- GI.new Gtk.HeaderBar [#cssClasses := ["u-header"]]
  Platform.configureHeaderBar platform header

  -- No centered title: the brand and search sit on the left.
  noTitle <- GI.new Gtk.Box []
  GI.set header [#titleWidget := noTitle]

  brand <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Icon.new 24 Icon.Logo >>= #append brand
  GI.new Gtk.Label [#label := "usta", #cssClasses := ["brand"]] >>= #append brand
  #packStart header brand

  (search, _) <- SearchField.new SearchField.defaults
    { SearchField.shortcut = Just (Platform.shortcut platform "K") }
  #packStart header search

  sync <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Badge.dot Success >>= #append sync
  GI.new Gtk.Label [#label := "Synced 1 min ago", #cssClasses := ["muted", "sync"]] >>= #append sync

  refresh <- Button.new Button.defaults
    { Button.icon = Just "view-refresh-symbolic"
    , Button.variant = Button.Flat
    , Button.tooltip = Just "Sync now"
    }
  settings <- Button.new Button.defaults
    { Button.icon = Just "emblem-system-symbolic"
    , Button.variant = Button.Flat
    , Button.tooltip = Just "Settings"
    }
  avatar <- Avatar.new 32 "AA"

  -- packEnd fills from the right edge inwards.
  #packEnd header avatar
  #packEnd header settings
  #packEnd header refresh
  #packEnd header sync
  pure header

styles :: [Text]
styles =
  [ Avatar.css, Badge.css, Button.css, Card.css, Forge.css, Icon.css, NavItem.css
  , SearchField.css, Shortcut.css, Sidebar.css
  , windowCss
  ]

windowCss :: Text
windowCss = mconcat
  [ "headerbar.u-header {"
  , "  min-height: 52px; padding: 0 8px; background: var(--surface); color: var(--text);"
  , "  border-bottom: 1px solid var(--border); box-shadow: none;"
  , "}"
  , "headerbar.u-header label.brand { font-weight: 700; font-size: 15px; margin-right: 12px; }"
  , "headerbar.u-header label.sync { font-size: 13px; }"
  ]
