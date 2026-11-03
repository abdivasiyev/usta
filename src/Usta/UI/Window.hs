{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The main window: header bar on top, pages in the middle, the shortcut
-- strip at the bottom.
module Usta.UI.Window
  ( Start (..)
  , build
  , buildAt
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, join, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
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
import Usta.UI.Page (Page (..))
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
import Usta.UI.Source (Connected (..), Source (..))
import Usta.Details (From (..))
import Usta.Types.Parse (parse, unparse)
-- For the commits' fields.
import qualified Usta.Types.Commit as C
import Usta.UI.Format (Clock, ago, clock, short)
import qualified Usta.Forge as Forge
import qualified Usta.Sync as Sync
import Data.Containers.ListUtils (nubOrd)
import Data.List (find)
import qualified Usta.Types.Resource as Resource
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

build :: Source -> Gtk.Application -> IO Gtk.ApplicationWindow
build = buildAt Ready

-- | The pages show @source@'s lists, reload the ones that change, and
-- reload all of them each minute so "5 min ago" stays true.
buildAt :: Start -> Source -> Gtk.Application -> IO Gtk.ApplicationWindow
buildAt start source app = do
  -- Two screens: "main" (sidebar and pages) and "detail" (a full-width
  -- screen such as one pull request or the new pull request form).
  screens <- GI.new Gtk.Stack [#hexpand := True, #vexpand := True]
  states <- newIORef []
  let backToMain = Gtk.stackSetVisibleChildName screens "main"
      showDetail :: Gtk.Widget -> IO ()
      showDetail widget = do
        old <- Gtk.stackGetChildByName screens "detail"
        forM_ old $ Gtk.stackRemove screens
        void $ Gtk.stackAddNamed screens widget (Just "detail")
        Gtk.stackSetVisibleChildName screens "detail"
      openPullRequestWith fetch backLabel summary =
        PullRequest.new backLabel backToMain (tabContent source fetch summary) summary
          >>= Gtk.toWidget >>= showDetail
      openPullRequest = openPullRequestWith Nothing
      openNewPullRequest = NewPullRequest.new backToMain NewPullRequest.sample >>= showDetail
      openIssue backLabel model = Issue.new backLabel backToMain model >>= showDetail
      openNewIssue = Gtk.toWidget screens >>= \anchor -> NewIssue.open anchor NewIssue.sample
      addAccountThen after = Gtk.toWidget screens >>= \anchor ->
        AddAccount.open anchor (AddAccount.for Forge.Forgejo) source.signIn after
      openAddAccount = addAccountThen (pure ())
      -- The accounts are real; the other sections are sample settings for now.
      openSettings = do
        known <- readIORef states
        accounts <- source.accounts
        anchor <- Gtk.toWidget screens
        let row a = Settings.Account
              { Settings.forge = a.forge
              , Settings.host = a.host
              , Settings.user = a.login
              , Settings.auth = Settings.AccessToken
              , Settings.connected = lookup a.accountId known /= Just (Sync.Failed Forge.SignedOut)
              , Settings.onRemove = source.remove a.accountId
                  >>= either (Settings.notice anchor "The account was not removed") (const openSettings)
              , Settings.onReauthenticate =
                  AddAccount.open anchor (AddAccount.for a.forge) {AddAccount.server = "https://" <> a.host}
                    source.signIn openSettings
              }
            model = (Settings.sample shortcuts) {Settings.accounts = map row accounts}
        Settings.new backToMain (addAccountThen openSettings) model >>= showDetail

  -- The palette wraps the whole window, so it exists only further down.
  paletteOpener <- newIORef (pure ())
  syncChip <- GI.new Gtk.Box []
  header <- headerBar Platform.current syncChip source.syncNow openSettings (join (readIORef paletteOpener))

  let openNotification n
        | n.kind == Inbox.Issue = openIssue "Inbox" (notificationIssue n)
        | otherwise = openPullRequest "Inbox" (notificationSummary n)
  -- "Mark as done" asks the forge, so it runs off the main thread; the
  -- pages redraw once the cache has the answer.
  inboxRef <- newIORef (pure ())
  let setDone n done = do
        anchor <- Gtk.toWidget screens
        void $ forkIO $ do
          result <- source.setDone n done
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            either (Settings.notice anchor (if done then "Not marked done" else "Not moved to the inbox"))
              (const (join (readIORef inboxRef))) result
            pure False
      inboxActions = Inbox.Actions {open = openNotification, setDone = setDone}
  initial <- source.inbox
  inboxPages <- forM inboxViews $ \(page, view) -> (,) page <$> Inbox.new view inboxActions initial
  let reloadInbox = do
        everything <- source.inbox
        forM_ inboxPages $ \(_, p) -> p.update everything
  pullRequestPage <- source.pullRequests >>= PullRequests.new openNewPullRequest
    (\pr -> openPullRequestWith pr.fetching "Pull requests" (summaryOf pr))
  issuePage <- source.issues >>= Issues.new openNewIssue (openIssue "Issues" . issueDetail)
  -- The forms open with sample fields until they take the repository.
  -- Declining starts no clone, so no mirror state tells the page to redraw.
  pageRef <- newIORef (pure ())
  let chooseMirror r choice = do
        answered <- source.mirror r.host r.name choice
        when (either (const False) (const True) answered) $
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT (join (readIORef pageRef) >> pure False)
        pure answered
  let loadContents r = do
        c <- clock
        commits <- source.recentCommits r.host r.name r.defaultBranch
        readme <- source.readme r.host r.name r.defaultBranch
        pure $ do
          (_, cs) <- commits
          (_, file) <- readme
          pure Repositories.Contents
            { commits = [Repositories.Commit {hash = unparse x.sha, message = x.subject, age = short c x.authoredAt} | x <- cs]
            , readme = fmap (\(name, text) -> (name, Repositories.readmeBlocks text)) file
            }
  repositoryPage <- source.repositories >>= Repositories.new Repositories.Actions
    { newIssue = const openNewIssue
    , newPullRequest = const openNewPullRequest
    , chooseMirror = chooseMirror
    , lookUpOrigins = \r -> source.mirrorOrigins r.host r.name
    , loadContents = loadContents
    }
  writeIORef pageRef (source.repositories >>= repositoryPage.update)
  source.onMirrorStatus (source.repositories >>= repositoryPage.update)
  pages <- GI.new Gtk.Stack [#hexpand := True, #vexpand := True]
  shownPage <- newIORef Sidebar.Inbox
  sidebarSlot <- GI.new Gtk.Box []
  footerSlot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  -- Accounts are read at each redraw: signing in or out changes them.
  let liveModel = do
        known <- readIORef states
        accounts <- source.accounts
        notifications <- source.inbox
        let count n = if n > 0 then Just n else Nothing
            syncing host = or [s == Sync.Syncing | a <- accounts, a.host == host, Just s <- [lookup a.accountId known]]
        pure Sidebar.Model
          { Sidebar.counts = \page -> case page of
              Sidebar.Done -> Nothing
              _ -> case lookup page inboxViews of
                Just view -> count (length (filter (Inbox.lists view) notifications))
                Nothing -> Nothing
          , Sidebar.forges = [(a.forge, a.host) | a <- accounts]
          , Sidebar.forgeSync = \host -> if syncing host then Just Sidebar.ForgeSyncing else Nothing
          , Sidebar.pinned = []
          }
      sidebarModel
        | start == FirstSync = pure Sidebar.sample
            { Sidebar.counts = const Nothing
            , Sidebar.forgeSync = \host -> Just $
                if host == "github.com" then Sidebar.ForgeSyncing else Sidebar.ForgeDone
            , Sidebar.pinned = []
            }
        | otherwise = liveModel
      drawSidebar = do
        model <- sidebarModel
        page <- readIORef shownPage
        Sidebar.new model page (\p -> writeIORef shownPage p >> Gtk.stackSetVisibleChildName pages (Sidebar.pageName p)) openAddAccount
          >>= Gtk.toWidget >>= replaceChild sidebarSlot
      drawSyncChip = do
        known <- readIORef states
        accounts <- source.accounts
        c <- clock
        -- A removed account's last status no longer counts.
        let hostOf key = (.host) <$> find ((== key) . (.accountId)) accounts
            state
              | start == FirstSync = SyncStatus.Syncing "github.com"
              | otherwise = headline c [(host, s) | (key, s) <- known, Just host <- [hostOf key]]
        SyncStatus.new state source.syncNow >>= Gtk.toWidget >>= replaceChild syncChip
      reload resource = do
        case resource of
          Resource.Notifications -> reloadInbox
          -- Inbox rows show their pull request's checks and review.
          Resource.PullRequests -> do
            reloadInbox
            source.pullRequests >>= pullRequestPage.update
          Resource.Issues -> source.issues >>= issuePage.update
          Resource.Repositories -> source.repositories >>= repositoryPage.update
        drawSidebar
  let drawFooter = do
        footer <- if start == FirstSync
          then do
            bar <- Shortcut.bar [] (Just "Fetched 128 notifications")
            note <- GI.new Gtk.Label [#label := "You can keep working: items open as soon as they are cached."]
            Gtk.boxPrepend bar note
            pure bar
          else do
            accounts <- source.accounts
            Shortcut.bar shortcuts (Just (plural (length (nubOrd [a.host | a <- accounts])) "forge" "forges"
                                            <> " · " <> plural (length accounts) "account" "accounts"))
        Gtk.toWidget footer >>= replaceChild footerSlot
  writeIORef inboxRef (reloadInbox >> drawSidebar)
  -- Signing in or out changes the lists and the accounts at once.
  source.onChange $ \resource -> reload resource >> drawSyncChip >> drawFooter
  source.onStatus $ \status -> do
    modifyIORef' states (\known -> (status.accountId, status.state) : filter ((/= status.accountId) . fst) known)
    drawSyncChip
    drawSidebar
    drawFooter
  void $ GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT 60 $
    mapM_ reload [minBound .. maxBound] >> drawSyncChip >> pure True
  drawSyncChip

  forM_ [minBound .. maxBound] $ \page -> do
    content <- case page of
      Sidebar.Inbox | start == FirstSync ->
        Inbox.syncing "First sync: fetching notifications from 2 forges. Items appear as they arrive."
          >>= Gtk.toWidget
      _ | Just p <- lookup page inboxPages -> Gtk.toWidget p.widget
      Sidebar.PullRequests -> Gtk.toWidget pullRequestPage.widget
      Sidebar.Issues -> Gtk.toWidget issuePage.widget
      Sidebar.Repositories -> Gtk.toWidget repositoryPage.widget
      -- The other pages get their screens next; until then a placeholder.
      _ -> GI.new Gtk.Label [#label := Sidebar.pageTitle page, #cssClasses := ["muted"]]
             >>= Gtk.toWidget
    void $ Gtk.stackAddNamed pages content (Just (Sidebar.pageName page))

  drawSidebar

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
  #append body sidebarSlot
  #append body pages
  void $ Gtk.stackAddNamed screens body (Just "main")

  drawFooter

  root <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append root header
  #append root screens
  #append root footerSlot

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
              [ command "Sync now" (keys "R") source.syncNow
              , command "New pull request" (keys "Shift P") openNewPullRequest
              , command "New issue" (keys "Shift I") openNewIssue
              , command "Add forge account" "" openAddAccount
              , command "Open settings" (keys ",") openSettings
              , command themeTitle "" $
                  forM_ settings $ \s -> GI.set s [#gtkInterfaceColorScheme := nextScheme]
              ]
        pullRequests <- map (paletteItem openPullRequest) <$> source.pullRequests
        issues <- map (issueItem openIssue) <$> source.issues
        repositories <- map (repositoryItem (showPage Sidebar.Repositories)) <$> source.repositories
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
          AddAccount.open anchor (AddAccount.for kind) source.signIn (Gtk.stackSetVisibleChildName top "app")
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
tabContent :: Source -> Maybe Double -> PullRequest.Summary -> PullRequest.Tab -> IO Gtk.Widget
tabContent source fetch summary tab = case tab of
  PullRequest.Files | Just p <- fetch -> Files.fetching (pure ()) (Files.fetchingSample p)
  PullRequest.Conversation -> Conversation.new Conversation.sample
  PullRequest.Files ->
    loadInto (source.pullFiles summary.host summary.repo summary.number) $ \(from, changes) ->
      Files.new (Files.fromFileDiffs (said from "Diff computed locally" "Diff from the forge's API") changes)
  PullRequest.Commits ->
    loadInto (source.pullCommits summary.host summary.repo summary.number) $ \(from, commits) -> do
      c <- clock
      let changesOf commit = case parse commit.hash of
            Left _ -> pure (Left "That commit hash is not one usta can read.")
            Right sha -> fmap (map Files.fromFileDiff . snd) <$> source.commitDiff summary.host summary.repo sha
      Commits.new (said from "history from the local mirror" "history from the forge's API") changesOf
        (map (Commits.fromCommit c) commits)
  PullRequest.Checks -> Checks.new $ case summary.checks of
    Just (Danger, _) -> Checks.failing
    _ -> Checks.passing

-- | The sidebar's pages that list notifications.
inboxViews :: [(Sidebar.Page, Inbox.View)]
inboxViews =
  [ (Sidebar.Inbox, Inbox.Everything)
  , (Sidebar.ReviewRequests, Inbox.Only Inbox.ReviewRequested)
  , (Sidebar.AssignedToMe, Inbox.Only Inbox.Assigned)
  , (Sidebar.Mentions, Inbox.Only Inbox.Mentioned)
  , (Sidebar.Done, Inbox.Finished)
  ]

-- | The detail header from an inbox notification about a pull request.
notificationSummary :: Inbox.Notification -> PullRequest.Summary
notificationSummary n = PullRequest.Summary
  { PullRequest.forge = n.forge
  , PullRequest.host = n.host
  , PullRequest.repo = n.repo
  , PullRequest.number = n.number
  , PullRequest.title = n.title
  , PullRequest.state =
      if n.kind == Inbox.MergedPullRequest then PullRequest.MergedState else PullRequest.Open
  , PullRequest.comments = 1
  , PullRequest.files = length n.changes
  , PullRequest.commits = 1
  -- The tab has room for a word, not the card's "1 of 1 passed".
  , PullRequest.checks = fmap (\(tone, _) -> (tone, word tone)) n.checks
  }
  where
    word tone = case tone of
      Success -> "passed"
      Danger -> "failed"
      Warning -> "running"
      _ -> ""

-- | An issue's detail from an inbox notification about it.
notificationIssue :: Inbox.Notification -> Issue.Model
notificationIssue n = Issue.Model
  { Issue.forge = n.forge
  , Issue.host = n.host
  , Issue.repo = n.repo
  , Issue.number = n.number
  , Issue.title = n.title
  , Issue.closed = False
  , Issue.comments =
      [ Issue.Comment "abdivasiyev" "AA" n.opened
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
  { Issue.forge = i.forge
  , Issue.host = i.host
  , Issue.repo = i.repo
  , Issue.number = i.number
  , Issue.title = i.title
  , Issue.closed = i.closed
  , Issue.comments =
      [ Issue.Comment i.author "AA" ("opened " <> i.opened)
          "Main.hs still prints Hello world.\nOpen a Gtk.ApplicationWindow on activate, with the header bar from the design."
      , Issue.Comment "abdivasiyev" "AA" "commented 1 hour ago"
          "gi-gtk4 is in now (#2). The window needs wrapGAppsHook4 in the flake so it finds the GSettings schemas."
      ]
  , Issue.events =
      [ Issue.LabelAdded tone name | Issues.Label tone name <- i.labels ]
      <> [Issue.ReferencedBy "#6 feat: render main window with GTK4"]
  , Issue.assignees = [("AA", "abdivasiyev")]
  , Issue.labels = [ (tone, name) | Issues.Label tone name <- i.labels ]
  , Issue.milestone = Just "First window"
  , Issue.linkedPullRequests = ["#6 feat: render main window with GTK4"]
  }

-- | "usta #2" from "abdivasiyev/usta" and 2.
shortRef :: Text -> Int -> Text
shortRef repo number = T.drop 1 (T.dropWhile (/= '/') repo) <> " #" <> T.pack (show number)

paletteItem :: (Text -> PullRequest.Summary -> IO ()) -> PullRequests.PullRequest -> CommandPalette.Item
paletteItem open pr = CommandPalette.Item CommandPalette.PullRequest pr.title
  (shortRef pr.repo pr.number) (open "Back" (summaryOf pr))

issueItem :: (Text -> Issue.Model -> IO ()) -> Issues.Issue -> CommandPalette.Item
issueItem open i = CommandPalette.Item CommandPalette.Issue i.title
  (shortRef i.repo i.number) (open "Back" (issueDetail i))

repositoryItem :: IO () -> Repositories.Repository -> CommandPalette.Item
repositoryItem open r = CommandPalette.Item CommandPalette.Repository r.name
  r.host open

summaryOf :: PullRequests.PullRequest -> PullRequest.Summary
summaryOf pr = PullRequest.Summary
  { PullRequest.forge = pr.forge
  , PullRequest.host = pr.host
  , PullRequest.repo = pr.repo
  , PullRequest.number = pr.number
  , PullRequest.title = pr.title
  , PullRequest.state = if pr.draft then PullRequest.Draft else PullRequest.Open
  , PullRequest.comments = pr.comments
  , PullRequest.files = fst pr.changes
  , PullRequest.commits = snd pr.changes
  , PullRequest.checks = case pr.checks of
      PullRequests.ChecksPassed -> Just (Success, "1/1")
      PullRequests.ChecksFailed -> Just (Danger, "1 failed")
      PullRequests.ChecksRunning -> Just (Warning, "running")
      PullRequests.NoChecks -> Nothing
  }

-- | The single-key shortcuts, for the footer strip and Settings.
shortcuts :: [(Text, Text)]
shortcuts =
  [("j k", "move"), ("Enter", "open"), ("e", "done"), ("s", "star"), ("o", "open in browser")]

-- | @syncChip@ holds the sync state, which the window keeps current;
-- @onSync@ runs for the refresh button.
headerBar :: Platform.Platform -> Gtk.Box -> IO () -> IO () -> IO () -> IO Gtk.HeaderBar
headerBar platform syncChip onSync onSettings onSearch = do
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

  refresh <- Button.new Button.defaults
    { Button.icon = Just "view-refresh-symbolic"
    , Button.variant = Button.Flat
    , Button.tooltip = Just "Sync now"
    }
  void $ GI.on refresh #clicked onSync
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
  #packEnd header syncChip
  pure header

-- | One state for the header from each account's, with its host: a
-- failure first, then offline, then what is syncing, then the oldest sync.
-- With no account synced, the cache is all there is.
headline :: Clock -> [(Text, Sync.State)] -> SyncStatus.State
headline c known
  | not (null [() | (_, Sync.Failed f) <- known, f /= Forge.Offline]) = SyncStatus.Failed
  | not (null [() | (_, Sync.Failed Forge.Offline) <- known]) = SyncStatus.Offline
  | not (null syncing) = SyncStatus.Syncing (T.intercalate ", " (nubOrd syncing))
  | not (null times) = SyncStatus.Synced (ago c (minimum times))
  | otherwise = SyncStatus.Offline
  where
    syncing = [host | (host, Sync.Syncing) <- known]
    times = [t | (_, Sync.Synced t) <- known]

-- | Where an answer came from, in words.
said :: From -> Text -> Text -> Text
said from mirror forge = case from of
  FromMirror -> mirror
  FromForge -> forge

-- | A skeleton until @load@ answers off the main thread, then what
-- @render@ makes of it, or the problem.
loadInto :: IO (Either Text a) -> (a -> IO Gtk.Widget) -> IO Gtk.Widget
loadInto load render = do
  slot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #hexpand := True, #vexpand := True]
  Skeleton.detail >>= #append slot
  void $ forkIO $ do
    result <- try load
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      widget <- case result of
        Right (Right a) -> render a
        Right (Left problem) -> notice problem
        Left (_ :: SomeException) -> notice "Loading failed unexpectedly."
      replaceChild slot widget
      pure False
  Gtk.toWidget slot
  where
    notice text = GI.new Gtk.Label [#label := text, #wrap := True, #cssClasses := ["muted"], #marginTop := 24, #marginStart := 24, #marginEnd := 24] >>= Gtk.toWidget

-- | Shows @widget@ as the box's only child.
replaceChild :: Gtk.Box -> Gtk.Widget -> IO ()
replaceChild box widget = do
  old <- Gtk.widgetGetFirstChild box
  forM_ old (Gtk.boxRemove box)
  #append box widget

plural :: Int -> Text -> Text -> Text
plural n one many = T.pack (show n) <> " " <> if n == 1 then one else many

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
