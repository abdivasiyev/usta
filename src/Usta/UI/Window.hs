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
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Components.Shortcut as Shortcut
import qualified Usta.UI.Pages.Inbox as Inbox
import qualified Usta.UI.Pages.PullRequest as PullRequest
import qualified Usta.UI.Pages.PullRequest.Conversation as Conversation
import qualified Usta.UI.Pages.PullRequests as PullRequests
import qualified Usta.UI.Platform as Platform
import qualified Usta.UI.Sidebar as Sidebar
import Usta.UI.Style (Tone (..))
import qualified Usta.UI.Style as Style

build :: Gtk.Application -> IO Gtk.ApplicationWindow
build app = do
  header <- headerBar Platform.current

  -- Two screens: "main" (sidebar and pages) and "detail" (one pull
  -- request, full width, with a way back).
  screens <- GI.new Gtk.Stack [#hexpand := True, #vexpand := True]
  let openPullRequest backLabel summary = do
        old <- Gtk.stackGetChildByName screens "detail"
        forM_ old $ Gtk.stackRemove screens
        detail <- PullRequest.new backLabel (Gtk.stackSetVisibleChildName screens "main")
                    tabContent summary
        void $ Gtk.stackAddNamed screens detail (Just "detail")
        Gtk.stackSetVisibleChildName screens "detail"

  pages <- GI.new Gtk.Stack [#hexpand := True, #vexpand := True]
  forM_ [minBound .. maxBound] $ \page -> do
    content <- case page of
      Sidebar.Inbox ->
        Inbox.new (openPullRequest "Inbox" . notificationSummary) Inbox.sample >>= Gtk.toWidget
      Sidebar.PullRequests ->
        PullRequests.new (openPullRequest "Pull requests" . summaryOf) PullRequests.sample
          >>= Gtk.toWidget
      -- The other pages get their screens next; until then a placeholder.
      _ -> GI.new Gtk.Label [#label := Sidebar.pageTitle page, #cssClasses := ["muted"]]
             >>= Gtk.toWidget
    void $ Gtk.stackAddNamed pages content (Just (Sidebar.pageName page))

  sidebar <- Sidebar.new Sidebar.sample Sidebar.Inbox $ \page ->
    Gtk.stackSetVisibleChildName pages (Sidebar.pageName page)

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
  #append body sidebar
  #append body pages
  void $ Gtk.stackAddNamed screens body (Just "main")

  footer <- Shortcut.bar
    [("j k", "move"), ("Enter", "open"), ("e", "done"), ("s", "star"), ("o", "open in browser")]
    (Just "2 forges · 3 accounts")

  root <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append root header
  #append root screens
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

-- | The detail header from a list row. File and commit counts are sample
-- values until the detail is loaded from the forge.
-- | Each tab of a pull request; sample content until the forge supplies it.
tabContent :: PullRequest.Tab -> IO Gtk.Widget
tabContent tab = case tab of
  PullRequest.Conversation -> Conversation.new Conversation.sample
  _ -> PullRequest.placeholder tab

-- | The detail header from an inbox notification about a pull request.
notificationSummary :: Inbox.Notification -> PullRequest.Summary
notificationSummary n = PullRequest.Summary
  { PullRequest.forge = Inbox.forge n
  , PullRequest.host = Inbox.host n
  , PullRequest.repo = Inbox.repo n
  , PullRequest.number = Inbox.number n
  , PullRequest.title = Inbox.title n
  , PullRequest.state =
      if Inbox.kind n == Inbox.MergedPullRequest then PullRequest.MergedState else PullRequest.Open
  , PullRequest.comments = 1
  , PullRequest.files = length (Inbox.changes n)
  , PullRequest.commits = 1
  -- The tab has room for a word, not the card's "1 of 1 passed".
  , PullRequest.checks = fmap (\(tone, _) -> (tone, short tone)) (Inbox.checks n)
  }
  where
    short tone = case tone of
      Success -> "passed"
      Danger -> "failed"
      Warning -> "running"
      _ -> ""

summaryOf :: PullRequests.PullRequest -> PullRequest.Summary
summaryOf pr = PullRequest.Summary
  { PullRequest.forge = PullRequests.forge pr
  , PullRequest.host = PullRequests.host pr
  , PullRequest.repo = PullRequests.repo pr
  , PullRequest.number = PullRequests.number pr
  , PullRequest.title = PullRequests.title pr
  , PullRequest.state = if PullRequests.draft pr then PullRequest.Draft else PullRequest.Open
  , PullRequest.comments = PullRequests.comments pr
  , PullRequest.files = 2
  , PullRequest.commits = 1
  , PullRequest.checks = case PullRequests.checks pr of
      PullRequests.ChecksPassed -> Just (Success, "1/1")
      PullRequests.ChecksFailed -> Just (Danger, "1 failed")
      PullRequests.ChecksRunning -> Just (Warning, "running")
      PullRequests.NoChecks -> Nothing
  }

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
  , SearchField.css, Segmented.css, Shortcut.css, Sidebar.css, Inbox.css, PullRequests.css
  , PullRequest.css, Conversation.css
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
