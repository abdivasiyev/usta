{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The main window: header bar on top, pages in the middle, the shortcut
-- strip at the bottom.
module Usta.UI.Window
  ( Start (..)
  , build
  , buildAt
  ) where

import Control.Monad (forM_, join, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.CommandPalette as CommandPalette
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.NavItem as NavItem
import qualified Usta.UI.Components.SearchField as SearchField
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Components.Progress as Progress
import qualified Usta.UI.Components.Shortcut as Shortcut
import qualified Usta.UI.Components.Skeleton as Skeleton
import qualified Usta.UI.Components.SyncStatus as SyncStatus
import qualified Usta.UI.Pages.AddAccount as AddAccount
import qualified Usta.UI.Pages.Inbox as Inbox
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import qualified Usta.UI.Pages.Issue as Issue
import qualified Usta.UI.Pages.Issues as Issues
import qualified Usta.UI.Pages.NewIssue as NewIssue
import qualified Usta.UI.Pages.NewPullRequest as NewPullRequest
import qualified Usta.UI.Pages.PullRequest as PullRequest
import qualified Usta.UI.Pages.PullRequest.Checks as Checks
import qualified Usta.UI.Pages.PullRequest.Commits as Commits
import qualified Usta.UI.Pages.PullRequest.Conversation as Conversation
import qualified Usta.UI.Pages.PullRequest.Files as Files
import qualified Usta.UI.Components.Diff as Diff
import qualified Usta.UI.Pages.PullRequests as PullRequests
import qualified Usta.UI.Pages.Repositories as Repositories
import qualified Usta.UI.Pages.Settings as Settings
import qualified Usta.UI.Pages.Welcome as Welcome
import qualified Usta.UI.Platform as Platform
import qualified Usta.UI.Sidebar as Sidebar
import Usta.UI.Style (Tone (..))
import qualified Usta.UI.Style as Style

-- | Where the app starts. Until accounts and the cache exist, the first-run
-- states are chosen on the command line to preview them.
type Start :: Type
data Start
  = Ready
    -- ^ Accounts connected and the cache filled.
  | FirstRun
    -- ^ No account yet: the welcome screen.
  | FirstSync
    -- ^ Accounts connected, the cache still empty.
  deriving (Eq)

build :: Gtk.Application -> IO Gtk.ApplicationWindow
build = buildAt Ready

buildAt :: Start -> Gtk.Application -> IO Gtk.ApplicationWindow
buildAt start app = do
  -- Two screens: "main" (sidebar and pages) and "detail" (a full-width
  -- screen such as one pull request or the new pull request form).
  screens <- GI.new Gtk.Stack [#hexpand := True, #vexpand := True]
  let backToMain = Gtk.stackSetVisibleChildName screens "main"
      showDetail :: Gtk.Widget -> IO ()
      showDetail widget = do
        old <- Gtk.stackGetChildByName screens "detail"
        forM_ old $ Gtk.stackRemove screens
        void $ Gtk.stackAddNamed screens widget (Just "detail")
        Gtk.stackSetVisibleChildName screens "detail"
      openPullRequestWith fetch backLabel summary =
        PullRequest.new backLabel backToMain (tabContent fetch summary) summary
          >>= Gtk.toWidget >>= showDetail
      openPullRequest = openPullRequestWith Nothing
      openNewPullRequest = NewPullRequest.new backToMain NewPullRequest.sample >>= showDetail
      openIssue backLabel model = Issue.new backLabel backToMain model >>= showDetail
      openNewIssue = Gtk.toWidget screens >>= \anchor -> NewIssue.open anchor NewIssue.sample
      openAddAccount = Gtk.toWidget screens >>= \anchor -> AddAccount.open anchor AddAccount.sample (pure ())
      openSettings = Settings.new backToMain openAddAccount (Settings.sample shortcuts) >>= showDetail

  -- The palette wraps the whole window, so it exists only further down.
  paletteOpener <- newIORef (pure ())
  let syncState
        | start == FirstSync = SyncStatus.Syncing "github.com"
        | otherwise = SyncStatus.Synced "1 min ago"
  header <- headerBar Platform.current syncState openSettings (join (readIORef paletteOpener))

  pages <- GI.new Gtk.Stack [#hexpand := True, #vexpand := True]
  forM_ [minBound .. maxBound] $ \page -> do
    content <- case page of
      Sidebar.Inbox | start == FirstSync ->
        Inbox.syncing "First sync: fetching notifications from 2 forges. Items appear as they arrive."
          >>= Gtk.toWidget
      Sidebar.Inbox ->
        let open n
              | Inbox.kind n == Inbox.Issue = openIssue "Inbox" (notificationIssue n)
              | otherwise = openPullRequest "Inbox" (notificationSummary n)
        in Inbox.new open Inbox.sample >>= Gtk.toWidget
      Sidebar.PullRequests ->
        PullRequests.new openNewPullRequest
          (\pr -> openPullRequestWith (PullRequests.fetching pr) "Pull requests" (summaryOf pr))
          PullRequests.sample
          >>= Gtk.toWidget
      Sidebar.Issues ->
        Issues.new openNewIssue (openIssue "Issues" . issueDetail) Issues.sample >>= Gtk.toWidget
      -- The forms open with sample fields until they take the repository.
      Sidebar.Repositories ->
        Repositories.new (const openNewIssue) (const openNewPullRequest) Repositories.sample
          >>= Gtk.toWidget
      -- The other pages get their screens next; until then a placeholder.
      _ -> GI.new Gtk.Label [#label := Sidebar.pageTitle page, #cssClasses := ["muted"]]
             >>= Gtk.toWidget
    void $ Gtk.stackAddNamed pages content (Just (Sidebar.pageName page))

  let sidebarModel
        | start == FirstSync = Sidebar.sample
            { Sidebar.counts = const Nothing
            , Sidebar.forgeSync = \host -> Just $
                if host == "github.com" then Sidebar.ForgeSyncing else Sidebar.ForgeDone
            , Sidebar.pinned = []
            }
        | otherwise = Sidebar.sample
  sidebar <- Sidebar.new sidebarModel Sidebar.Inbox $ \page ->
    Gtk.stackSetVisibleChildName pages (Sidebar.pageName page)

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
  #append body sidebar
  #append body pages
  void $ Gtk.stackAddNamed screens body (Just "main")

  footer <- if start == FirstSync
    then do
      bar <- Shortcut.bar [] (Just "Fetched 128 notifications")
      note <- GI.new Gtk.Label [#label := "You can keep working: items open as soon as they are cached."]
      Gtk.boxPrepend bar note
      pure bar
    else Shortcut.bar shortcuts (Just "2 forges · 3 accounts")

  root <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append root header
  #append root screens
  #append root footer

  -- GTK hides the titlebar widget in fullscreen, and ours holds search and
  -- sync. So the header lives in the content, and an empty hidden titlebar
  -- stops GTK from adding its own.
  noTitlebar <- GI.new Gtk.Box [#visible := False]

  let showPage page = backToMain >> Gtk.stackSetVisibleChildName pages (Sidebar.pageName page)
      paletteModel = do
        settings <- Gtk.settingsGetDefault
        scheme <- maybe (pure Gtk.InterfaceColorSchemeDefault) (`GI.get` #gtkInterfaceColorScheme) settings
        let (themeTitle, nextScheme)
              | scheme == Gtk.InterfaceColorSchemeDark = ("Switch to light theme", Gtk.InterfaceColorSchemeLight)
              | otherwise = ("Switch to dark theme", Gtk.InterfaceColorSchemeDark)
            keys = Platform.shortcut Platform.current
            command title detail action = CommandPalette.Item CommandPalette.Command title detail action
            commands =
              [ command "Sync now" (keys "R") (pure ())
              , command "New pull request" (keys "Shift P") openNewPullRequest
              , command "New issue" (keys "Shift I") openNewIssue
              , command "Add forge account" "" openAddAccount
              , command "Open settings" (keys ",") openSettings
              , command themeTitle "" $
                  forM_ settings $ \s -> GI.set s [#gtkInterfaceColorScheme := nextScheme]
              ]
            pullRequests = map (paletteItem openPullRequest) PullRequests.sample
            issues = map (issueItem openIssue) Issues.sample
            repositories = map (repositoryItem (showPage Sidebar.Repositories)) Repositories.sample
        pure CommandPalette.Model
          { CommandPalette.recent = take 1 pullRequests <> take 1 issues <> take 1 (drop 1 repositories)
          , CommandPalette.items = pullRequests <> issues <> repositories <> commands
          }
  rootWidget <- Gtk.toWidget root
  (withPalette, openPalette) <- CommandPalette.new rootWidget paletteModel
  writeIORef paletteOpener openPalette

  -- The welcome screen covers the whole window until an account is added.
  top <- GI.new Gtk.Stack [#transitionType := Gtk.StackTransitionTypeCrossfade]
  void $ Gtk.stackAddNamed top withPalette (Just "app")
  when (start == FirstRun) $ do
    let connect kind = Gtk.toWidget top >>= \anchor ->
          AddAccount.open anchor (AddAccount.for kind) (Gtk.stackSetVisibleChildName top "app")
    welcome <- Welcome.new connect
    void $ Gtk.stackAddNamed top welcome (Just "welcome")
    Gtk.stackSetVisibleChildName top "welcome"

  window <- GI.new Gtk.ApplicationWindow [ #application := app
                                         , #title := "usta"
                                         , #defaultWidth := 1280
                                         , #defaultHeight := 800
                                         , #titlebar := noTitlebar
                                         , #child := top
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
-- @fetch@ is how far the local mirror is with the branch, while it is
-- still fetching; the Files tab waits for it.
tabContent :: Maybe Double -> PullRequest.Summary -> PullRequest.Tab -> IO Gtk.Widget
tabContent fetch summary tab = case tab of
  PullRequest.Files | Just p <- fetch -> Files.fetching (pure ()) (Files.fetchingSample p)
  PullRequest.Conversation -> Conversation.new Conversation.sample
  PullRequest.Files -> Files.new Files.sample
  PullRequest.Commits -> Commits.new Commits.sample
  PullRequest.Checks -> Checks.new $ case PullRequest.checks summary of
    Just (Danger, _) -> Checks.failing
    _ -> Checks.passing

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

-- | An issue's detail from an inbox notification about it.
notificationIssue :: Inbox.Notification -> Issue.Model
notificationIssue n = Issue.Model
  { Issue.forge = Inbox.forge n
  , Issue.host = Inbox.host n
  , Issue.repo = Inbox.repo n
  , Issue.number = Inbox.number n
  , Issue.title = Inbox.title n
  , Issue.closed = False
  , Issue.comments =
      [ Issue.Comment "abdivasiyev" "AA" (Inbox.opened n)
          "Found while reviewing: describe what happens and what you expected."
      ]
  , Issue.events = []
  , Issue.assignees = [("AA", "abdivasiyev")]
  , Issue.labels = []
  , Issue.milestone = Nothing
  , Issue.linkedPullRequests = []
  }

-- | An issue's detail from its list row. The thread and side details are
-- sample content until the issue is loaded from the forge.
issueDetail :: Issues.Issue -> Issue.Model
issueDetail i = Issue.Model
  { Issue.forge = Issues.forge i
  , Issue.host = Issues.host i
  , Issue.repo = Issues.repo i
  , Issue.number = Issues.number i
  , Issue.title = Issues.title i
  , Issue.closed = Issues.closed i
  , Issue.comments =
      [ Issue.Comment (Issues.author i) "AA" ("opened " <> Issues.opened i)
          "Main.hs still prints Hello world.\nOpen a Gtk.ApplicationWindow on activate, with the header bar from the design."
      , Issue.Comment "abdivasiyev" "AA" "commented 1 hour ago"
          "gi-gtk4 is in now (#2). The window needs wrapGAppsHook4 in the flake so it finds the GSettings schemas."
      ]
  , Issue.events =
      [ Issue.LabelAdded tone name | Issues.Label tone name <- Issues.labels i ]
      <> [Issue.ReferencedBy "#6 feat: render main window with GTK4"]
  , Issue.assignees = [("AA", "abdivasiyev")]
  , Issue.labels = [ (tone, name) | Issues.Label tone name <- Issues.labels i ]
  , Issue.milestone = Just "First window"
  , Issue.linkedPullRequests = ["#6 feat: render main window with GTK4"]
  }

-- | "usta #2" from "abdivasiyev/usta" and 2.
shortRef :: Text -> Int -> Text
shortRef repo number = T.drop 1 (T.dropWhile (/= '/') repo) <> " #" <> T.pack (show number)

paletteItem :: (Text -> PullRequest.Summary -> IO ()) -> PullRequests.PullRequest -> CommandPalette.Item
paletteItem open pr = CommandPalette.Item CommandPalette.PullRequest (PullRequests.title pr)
  (shortRef (PullRequests.repo pr) (PullRequests.number pr)) (open "Back" (summaryOf pr))

issueItem :: (Text -> Issue.Model -> IO ()) -> Issues.Issue -> CommandPalette.Item
issueItem open i = CommandPalette.Item CommandPalette.Issue (Issues.title i)
  (shortRef (Issues.repo i) (Issues.number i)) (open "Back" (issueDetail i))

repositoryItem :: IO () -> Repositories.Repository -> CommandPalette.Item
repositoryItem open r = CommandPalette.Item CommandPalette.Repository (Repositories.name r)
  (Repositories.host r) open

summaryOf :: PullRequests.PullRequest -> PullRequest.Summary
summaryOf pr = PullRequest.Summary
  { PullRequest.forge = PullRequests.forge pr
  , PullRequest.host = PullRequests.host pr
  , PullRequest.repo = PullRequests.repo pr
  , PullRequest.number = PullRequests.number pr
  , PullRequest.title = PullRequests.title pr
  , PullRequest.state = if PullRequests.draft pr then PullRequest.Draft else PullRequest.Open
  , PullRequest.comments = PullRequests.comments pr
  , PullRequest.files = fst (PullRequests.changes pr)
  , PullRequest.commits = snd (PullRequests.changes pr)
  , PullRequest.checks = case PullRequests.checks pr of
      PullRequests.ChecksPassed -> Just (Success, "1/1")
      PullRequests.ChecksFailed -> Just (Danger, "1 failed")
      PullRequests.ChecksRunning -> Just (Warning, "running")
      PullRequests.NoChecks -> Nothing
  }

-- | The single-key shortcuts, for the footer strip and Settings.
shortcuts :: [(Text, Text)]
shortcuts =
  [("j k", "move"), ("Enter", "open"), ("e", "done"), ("s", "star"), ("o", "open in browser")]

headerBar :: Platform.Platform -> SyncStatus.State -> IO () -> IO () -> IO Gtk.HeaderBar
headerBar platform syncState onSettings onSearch = do
  header <- GI.new Gtk.HeaderBar [#cssClasses := ["u-header"]]
  Platform.configureHeaderBar platform header

  -- No centered title: the brand and search sit on the left.
  noTitle <- GI.new Gtk.Box []
  GI.set header [#titleWidget := noTitle]

  brand <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Icon.new 24 Icon.Logo >>= #append brand
  GI.new Gtk.Label [#label := "usta", #cssClasses := ["brand"]] >>= #append brand
  #packStart header brand

  (search, entry) <- SearchField.new SearchField.defaults
    { SearchField.shortcut = Just (Platform.shortcut platform "K") }
  -- The field only opens the palette, where the searching happens.
  GI.set entry [#canFocus := False]
  void $ GI.on entry #activate onSearch
  click <- GI.new Gtk.GestureClick []
  void $ GI.on click #pressed $ \_ _ _ -> onSearch
  Gtk.widgetAddController search click
  #packStart header search

  -- Retrying needs the sync engine, which comes later.
  sync <- SyncStatus.new syncState (pure ())

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
  void $ GI.on settings #clicked onSettings
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
  , PullRequest.css, Conversation.css, Diff.css, Files.css, Checks.css, Commits.css
  , NewPullRequest.css, Issues.css, Issue.css, MarkdownEditor.css
  , NewIssue.css, Repositories.css, Settings.css, CommandPalette.css, AddAccount.css, Welcome.css, SyncStatus.css, Skeleton.css, Progress.css
  , windowCss
  ]

windowCss :: Text
windowCss = mconcat
  [ "headerbar.u-header {"
  , "  min-height: 52px; padding: 0 8px; background: var(--surface); color: var(--text);"
  , "  border-bottom: 1px solid var(--border); box-shadow: none;"
  , "}"
  , "headerbar.u-header label.brand { font-weight: 700; font-size: 15px; margin-right: 12px; }"
  ]
