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
import Control.Exception (SomeException, bracket_, try)
import Control.Monad (forM, forM_, join, void, when)
import Data.Maybe (fromMaybe)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import System.Directory (getHomeDirectory)
import qualified Data.Text as T
import qualified GI.Gio as Gio
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
import qualified Usta.UI.Components.AssignPickers as AssignPickers
import qualified Usta.UI.Components.CommentCard as CommentCard
import qualified Usta.UI.Components.TimelineRow as TimelineRow
import qualified Usta.UI.Components.LabelPicker as LabelPicker
import qualified Usta.UI.Components.Thread as Thread
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
import qualified Usta.UI.Pages.RepositoryActions as RepositoryActions
import qualified Usta.UI.Pages.RepositoryCommits as RepositoryCommits
import qualified Usta.UI.Components.SidePanel as SidePanel
import qualified Usta.UI.Components.BranchPicker as BranchPicker
import qualified Usta.UI.Pages.PullRequest.Checks as Checks
import qualified Usta.UI.Pages.PullRequest.Commits as Commits
import qualified Usta.UI.Pages.PullRequest.Conversation as Conversation
import qualified Usta.UI.Pages.PullRequest.Files as Files
import qualified Usta.UI.Components.Diff as Diff
import qualified Usta.UI.Pages.PullRequests as PullRequests
import qualified Usta.UI.Pages.Repositories as Repositories
import qualified Usta.UI.Pages.RepositoryFiles as RepositoryFiles
import qualified Usta.UI.Components.CodeView as CodeView
import qualified Usta.UI.Components.ImageView as ImageView
import qualified Usta.UI.Components.PdfView as PdfView
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.Mermaid as Mermaid
import qualified Usta.UI.Pages.Settings as Settings
import qualified Usta.UI.Pages.Welcome as Welcome
import qualified Usta.UI.Platform as Platform
import qualified Usta.UI.Sidebar as Sidebar
import Usta.UI.Source (Connected (..), Source (..))
import qualified Usta.UI.Source as Source
import Usta.Details (From (..))
import qualified Usta.Types.Diff as Changes
import qualified Usta.Types.Discussion as D
import Usta.Types.Thread (Threads (..))
import Usta.Preferences (Preferences (..), Theme (..))
import Usta.Types.Discussion (Merge (..))
import Usta.Types.Login (Login)
import Usta.Types.Milestone (Milestone (..))
import qualified Usta.Types.PullRequest as P
import qualified Usta.Types.Timeline as Tl
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
buildAt start given app = do
  -- The Files tab's side panels, kept as left from one pull request to
  -- the next.
  panels <- Files.newPanels
  -- A thin moving bar under the header while a forge or mirror call takes
  -- longer than 300 ms (its size is not known, so it does not count).
  activity <- Progress.bar Progress.Unknown
  Gtk.widgetAddCssClass activity "u-activity"
  GI.set activity [#visible := False]
  waiting <- newIORef (0 :: Int)
  let onMain act = void $ GLib.idleAdd GLib.PRIORITY_DEFAULT (act >> pure False)
      track :: IO a -> IO a
      track act = bracket_
        (do atomicModifyIORef' waiting (\n -> (n + 1, ()))
            onMain $ void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 300 $ do
              busy <- (> 0) <$> readIORef waiting
              when busy $ GI.set activity [#visible := True]
              pure False)
        (do atomicModifyIORef' waiting (\n -> (n - 1, ()))
            onMain $ do
              idle <- (== 0) <$> readIORef waiting
              when idle $ GI.set activity [#visible := False])
        act
      source = Source.tracking track given
  -- Two screens: "main" (sidebar and pages) and "detail-N" (a full-width
  -- screen such as one pull request or the new pull request form).
  -- Screens and pages cross-fade; GTK skips it when animations are off.
  screens <- GI.new Gtk.Stack [ #hexpand := True
                              , #vexpand := True
                              , #transitionType := Gtk.StackTransitionTypeCrossfade
                              , #transitionDuration := 180
                              ]
  states <- newIORef []
  shownDetails <- newIORef (0 :: Int)
  -- The saved theme, before anything is drawn.
  applyTheme <- Style.themes
  source.preferences >>= applyTheme . (.theme)
  notifying <- Platform.canNotify
  let backToMain = Gtk.stackSetVisibleChildName screens "main"
      showDetail :: Gtk.Widget -> IO ()
      -- The new screen fades in over the old one, which goes once hidden.
      showDetail widget = do
        n <- (+ 1) <$> readIORef shownDetails
        writeIORef shownDetails n
        let name = "detail-" <> T.pack (show n)
            old = "detail-" <> T.pack (show (n - 1))
        void $ Gtk.stackAddNamed screens widget (Just name)
        Gtk.stackSetVisibleChildName screens name
        void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 400 $ do
          Gtk.stackGetChildByName screens old >>= mapM_ (Gtk.stackRemove screens)
          pure False
      openPullRequestWith fetch backLabel summary =
        PullRequest.new backLabel backToMain (source.editTitle summary.host summary.repo summary.number)
          (Issue.Watch (source.watching summary.host summary.repo summary.number) (source.setWatching summary.host summary.repo summary.number))
          (tabContent source panels (openPullRequestWith fetch backLabel summary) fetch summary) summary
          >>= Gtk.toWidget >>= showDetail
      openPullRequest = openPullRequestWith Nothing
      -- In @at@ (host and repository) when given, else the first one.
      openNewPullRequestIn at = do
        repos <- source.repositories
        let places =
              [ NewPullRequest.Place {forge = r.forge, host = r.host, repo = r.name, defaultBranch = r.defaultBranch}
              | r <- repos
              ]
            chosen = length (takeWhile (\p -> Just (p.host, p.repo) /= at) places)
            opened p number title branches = openPullRequest "Pull requests" PullRequest.Summary
              { PullRequest.forge = p.forge, PullRequest.host = p.host, PullRequest.repo = p.repo
              , PullRequest.number = number, PullRequest.title = title, PullRequest.state = PullRequest.Open
              , PullRequest.comments = 0, PullRequest.files = 0, PullRequest.commits = 0
              , PullRequest.checks = Nothing, PullRequest.branches = branches
              }
        NewPullRequest.new NewPullRequest.Actions
          { branchesOf = \p -> source.branches p.host p.repo
          , compareOf = \p into from -> fmap
              (\(cs, paths) -> ([(T.take 7 (unparse c.sha), c.subject) | c <- cs], paths))
              <$> source.compare p.host p.repo into from
          , create = \p -> source.createPull p.host p.repo
          , opened = opened
          , cancel = backToMain
          }
          NewPullRequest.Model {places = places, chosen = if chosen < length places then chosen else 0}
          >>= showDetail
      openNewPullRequest = openNewPullRequestIn Nothing
      -- The header shows at once; the thread loads from the forge under it.
      openIssue backLabel model = do
        let (h, r, n) = (model.host, model.repo, model.number)
            refresh = openIssue backLabel model
            actionsFor d goals me c = Issue.Actions
              { comment = source.comment h r n
              , setClosed = source.setClosed h r n
              , labels = labelPicker source h r n refresh
              , talk = talkFor source h r n refresh me c
              , assigning = assignPicker source h r n refresh me d.assignees
              , milestoning = milestonePicker source h r n refresh c d.milestone
              , milestoneNow = find (\g -> Just g.title == d.milestone) goals
              , refresh = refresh
              }
            load = do
              d <- source.discussion h r n
              goals <- either (const []) id <$> source.milestones h r
              events <- either (const []) id <$> source.timeline h r n
              me <- maybe "" (.login) . find (\a -> a.host == h) <$> source.accounts
              pure (fmap (\x -> (x, goals, me, events)) d)
        Issue.new backLabel backToMain (source.editTitle h r n) (Issue.Watch (source.watching h r n) (source.setWatching h r n)) model (\showClosed ->
          loadInto load $ \(d, goals, me, events) -> do
            c <- clock
            showClosed d.closed
            Issue.thread (actionsFor d goals me c) (Issue.fromDiscussion c d model {Issue.timeline = events}))
          >>= showDetail
      -- In @at@ (host and repository) when given, else the first one.
      openNewIssueIn at = do
        anchor <- Gtk.toWidget screens
        repos <- source.repositories
        accounts <- source.accounts
        let places =
              [ NewIssue.Place {forge = r.forge, host = r.host, repo = r.name, login = a.login}
              | r <- repos, a <- take 1 [a | a <- accounts, a.host == r.host]
              ]
            chosen = length (takeWhile (\p -> Just (p.host, p.repo) /= at) places)
            opened p number title = openIssue "Issues" Issue.Model
              { Issue.forge = p.forge, Issue.host = p.host, Issue.repo = p.repo, Issue.number = number
              , Issue.title = title, Issue.closed = False, Issue.comments = [], Issue.events = [], Issue.timeline = []
              , Issue.assignees = [], Issue.labels = [], Issue.milestone = Nothing, Issue.linkedPullRequests = []
              }
        NewIssue.open anchor (\p -> source.createIssue p.host p.repo) opened
          NewIssue.Model {places = places, chosen = if chosen < length places then chosen else 0}
      openNewIssue = openNewIssueIn Nothing
      addAccountThen after = Gtk.toWidget screens >>= \anchor ->
        AddAccount.open anchor (AddAccount.for Forge.Forgejo) source.signIn after
      openAddAccount = addAccountThen (pure ())
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
        chosen <- source.preferences
        folder <- source.mirrorsFolder
        home <- getHomeDirectory
        let model = (Settings.sample shortcuts)
              { Settings.accounts = map row accounts
              , Settings.mirrorPath = maybe (T.pack folder) ("~" <>) (T.stripPrefix (T.pack home) (T.pack folder))
              , Settings.onShowMirrors = do
                  launcher <- Gio.fileNewForPath folder >>= Gtk.fileLauncherNew . Just
                  Gtk.fileLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
              , Settings.onPrune = source.pruneMirrors
              , Settings.onClearCache = source.clearCache
              , Settings.preferences = chosen
              , Settings.canNotify = notifying
              , Settings.onPreferences = \p -> do
                  applyTheme p.theme
                  source.setPreferences p
              }
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
      -- The preview's description, latest comments and files, from the
      -- forge; a comment is new when it came after the user's own last one.
      previewDetails n = do
        discussed <- source.discussion n.host n.repo n.number
        changed <- if n.kind == Inbox.Issue then pure (Right []) else fmap (map change . snd) <$> source.pullFiles n.host n.repo n.number
        me <- maybe "" (.login) . find (\a -> a.host == n.host) <$> source.accounts
        c <- clock
        pure $ do
          d <- discussed
          let everyone = d.opening : d.comments
              mineAt = [x.createdAt | x <- everyone, unparse x.author == me]
              fresh x = n.unread && unparse x.author /= me && all (< x.createdAt) mineAt
              toSaid x = Inbox.Said {author = unparse x.author, when' = ago c x.createdAt, body = x.body, new = fresh x}
          pure Inbox.Details
            { description = (toSaid d.opening) {Inbox.new = False}
            , latest = map toSaid (reverse (take 2 (reverse d.comments)))
            , comments = length d.comments
            , files = either (const []) id changed
            }
      change f = Inbox.Change f.path (length [() | Changes.Insertion {} <- f.lines]) (length [() | Changes.Deletion {} <- f.lines])
      -- All in one go off the main thread; the inbox redraws once.
      setAllDone ns done = do
        anchor <- Gtk.toWidget screens
        void $ forkIO $ do
          results <- mapM (\n -> source.setDone n done) ns
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            case [problem | Left problem <- results] of
              problem : _ -> Settings.notice anchor (if done then "Not all were marked done" else "Not all moved back to the inbox") problem
              [] -> pure ()
            join (readIORef inboxRef)
            pure False
      inboxActions = Inbox.Actions {open = openNotification, setDone = setDone, details = previewDetails, setAllDone = setAllDone}
  initial <- source.inbox
  inboxPages <- forM inboxViews $ \(page, view) -> (,) page <$> Inbox.new view inboxActions initial
  -- Each reload tells the desktop about what is new since the last one,
  -- as the preferences say, when the desktop would show it.
  shown <- newIORef initial
  let tell everything = do
        before <- readIORef shown
        writeIORef shown everything
        chosen <- source.preferences
        active <- Gtk.applicationGetActiveWindow app >>= maybe (pure False) Gtk.windowIsActive
        when (notifying && chosen.notify && not (chosen.onlyInBackground && active)) $
          forM_ (Inbox.worthTelling before everything) $ \(what, n) -> do
            note <- Gio.notificationNew what
            Gio.notificationSetBody note (Just (n.repo <> " #" <> T.pack (show n.number) <> ": " <> n.title))
            Gio.applicationSendNotification app
              (Just (n.host <> "/" <> n.repo <> "#" <> T.pack (show n.number))) note
      reloadInbox = do
        everything <- source.inbox
        tell everything
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
            , readme = file
            }
  repositoryPage <- source.repositories >>= Repositories.new Repositories.Actions
    { newIssue = \r -> openNewIssueIn (Just (r.host, r.name))
    , newPullRequest = \r -> openNewPullRequestIn (Just (r.host, r.name))
    , chooseMirror = chooseMirror
    , lookUpOrigins = \r -> source.mirrorOrigins r.host r.name
    , loadContents = loadContents
    , files = \r opening -> RepositoryFiles.new RepositoryFiles.Actions
        { loadTree = fmap (\(from, es) -> (said from "From the local mirror" "From the forge's API", es))
            <$> source.tree r.host r.name r.defaultBranch
        , loadFile = \p -> fmap snd <$> source.file r.host r.name r.defaultBranch p
        }
        RepositoryFiles.Place {forge = r.forge, host = r.host, repo = r.name, branch = r.defaultBranch}
        opening
    , branches = \r base -> fmap snd <$> source.branchList r.host r.name base
    , tags = \r -> fmap snd <$> source.tagList r.host r.name
    , tabs = \r ->
        [ Repositories.Tab
            { followsBranch = True
            , title = "Commits"
            , build = do
                c <- clock
                RepositoryCommits.new c RepositoryCommits.Actions
                  { branch = r.defaultBranch
                  , page = \n -> fmap snd <$> source.history r.host r.name r.defaultBranch n
                  , changes = \x -> fmap (map Files.fromFileDiff . snd) <$> source.commitDiff r.host r.name x.sha
                  }
            }
        , Repositories.Tab
            { followsBranch = False
            , title = counted "Pull requests" r.pullRequests
            , build = do
                synced <- filter (\pr -> pr.host == r.host && pr.repo == r.name) <$> source.pullRequests
                let load = do
                      c <- clock
                      fmap (map (PullRequests.listed c r.forge)) <$> source.repoPullRequests r.host r.name
                PullRequests.forRepository synced load (\pr -> openPullRequestWith pr.fetching "Repositories" (summaryOf pr))
                  >>= Gtk.toWidget
            }
        , Repositories.Tab
            { followsBranch = False
            , title = counted "Issues" r.issues
            , build = do
                synced <- filter (\i -> i.host == r.host && i.repo == r.name) <$> source.issues
                let load = do
                      c <- clock
                      fmap (map (Issues.fromIssue c r.forge)) <$> source.repoIssues r.host r.name
                Issues.forRepository synced load (openIssue "Repositories" . issueDetail) >>= Gtk.toWidget
            }
        , Repositories.Tab
            { followsBranch = False
            , title = "Actions"
            , build = do
                c <- clock
                RepositoryActions.new c (source.runs r.host r.name) (source.checkLog r.host r.name)
            }
        ]
    }
  writeIORef pageRef (source.repositories >>= repositoryPage.update)
  source.onMirrorStatus (source.repositories >>= repositoryPage.update)
  pages <- GI.new Gtk.Stack [ #hexpand := True
                            , #vexpand := True
                            , #transitionType := Gtk.StackTransitionTypeCrossfade
                            , #transitionDuration := 150
                            ]
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
              | null accounts = SyncStatus.NoAccounts
              | otherwise = headline c [(host, s) | (key, s) <- known, Just host <- [hostOf key]]
        SyncStatus.new state >>= Gtk.toWidget >>= replaceChild syncChip
      reload resource = do
        case resource of
          Resource.Notifications -> reloadInbox
          -- Inbox rows show their pull request's checks and review, and
          -- the badges that come from how the user is involved.
          Resource.PullRequests -> do
            reloadInbox
            source.pullRequests >>= pullRequestPage.update
          Resource.Issues -> do
            reloadInbox
            source.issues >>= issuePage.update
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
  #append root activity
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
        let (themeTitle, nextTheme)
              | scheme == Gtk.InterfaceColorSchemeDark = ("Switch to light theme", Light)
              | otherwise = ("Switch to dark theme", Dark)
            keys = Platform.shortcut Platform.current
            command title detail action = CommandPalette.Item CommandPalette.Command title detail action
            commands =
              [ command "Sync now" (keys "R") source.syncNow
              , command "New pull request" (keys "Shift P") openNewPullRequest
              , command "New issue" (keys "Shift I") openNewIssue
              , command "Add forge account" "" openAddAccount
              , command "Open settings" (keys ",") openSettings
              , command themeTitle "" $ do
                  chosen <- source.preferences
                  applyTheme nextTheme
                  source.setPreferences chosen {theme = nextTheme}
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
-- | @reopen@ loads the pull request again, after a change on the forge.
tabContent :: Source -> Files.Panels -> IO () -> Maybe Double -> PullRequest.Summary -> (PullRequest.State -> IO ()) -> PullRequest.Tab -> IO Gtk.Widget
tabContent source panels reopen fetch summary showState tab = case tab of
  PullRequest.Files | Just p <- fetch -> Files.fetching (pure ()) (Files.fetchingSample p)
  PullRequest.Conversation -> do
    let at = (summary.host, summary.repo, summary.number)
        (host, repo, n) = at
        load = do
          d <- source.discussion host repo n
          -- The timeline lists the commits, and the review threads follow
          -- the comments; without either it still loads.
          commits <- source.pullCommits host repo n
          threads <- source.threads host repo n
          reviewers <- either (const []) id <$> source.reviewers host repo n
          ready <- either (const Nothing) Just <$> source.readiness host repo n
          goals <- either (const []) id <$> source.milestones host repo
          events <- either (const []) id <$> source.timeline host repo n
          me <- maybe "" (.login) . find (\a -> a.host == host) <$> source.accounts
          pure (fmap (\x -> (x, either (const []) snd commits, either (const (Threads [] Nothing)) id threads, me, (reviewers, ready, goals, events))) d)
        actionsFor d goals me c = Conversation.Actions
          { comment = source.comment host repo n
          , setClosed = source.setClosed host repo n
          , merge = source.merge host repo n
          , scheduleMerge = source.scheduleMerge host repo n
          , cancelScheduledMerge = source.cancelScheduledMerge host repo n
          , markReady = source.markReady host repo n
          , updateBranch = source.updateBranch host repo n
          , requestReview = source.requestReview host repo n
          , unrequestReview = source.unrequestReview host repo n
          , candidates = source.reviewCandidates host repo n
          , labels = labelPicker source host repo n reopen
          , talk = talkFor source host repo n reopen me c
          , assigning = assignPicker source host repo n reopen me d.assignees
          , milestoning = milestonePicker source host repo n reopen c d.milestone
          , milestoneNow = find (\g -> Just g.title == d.milestone) goals
          , refresh = reopen
          }
        checks = case fmap fst summary.checks of
          Just Success -> Just True
          Just Danger -> Just False
          _ -> Nothing
    loadInto load $ \(d, commits, threads, me, (reviewers, ready, goals, events)) -> do
      c <- clock
      showState $ case (d.merged, d.closed) of
        (True, _) -> PullRequest.MergedState
        (_, True) -> PullRequest.Closed
        _ -> summary.state
      let shown t = Thread.Shown
            { thread = t, outdated = False, showCode = True, lastReviewed = threads.lastReviewed
            , me = me, clock = c, reply = source.reply host repo n t }
          model = Conversation.fromDiscussion c d commits summary.branches checks
          running = fmap fst summary.checks == Just Warning
      Conversation.new (actionsFor d goals me c) model
        { Conversation.threads = map shown threads.threads
        , Conversation.timeline = [e | e <- events, not (pushed e)]
        , Conversation.reviewers = reviewers
        , Conversation.merge = (\st -> st {Conversation.readiness = ready, Conversation.checksRunning = running}) <$> model.merge
        }
  PullRequest.Files ->
    -- The diff with its review threads; without threads it still shows.
    let withThreads = do
          files <- source.pullFiles summary.host summary.repo summary.number
          threads <- source.threads summary.host summary.repo summary.number
          -- The newest commit, for permalinks to lines.
          commits <- source.pullCommits summary.host summary.repo summary.number
          me <- maybe "" (.login) . find (\a -> a.host == summary.host) <$> source.accounts
          c <- clock
          -- The forge names each file's diff by a digest of its path.
          hashes <- forM (either (const []) (map (.path) . snd) files) $ \p ->
            (,) p . fromMaybe "" <$> GLib.computeChecksumForString
              (if summary.forge == Forge.GitHub then GLib.ChecksumTypeSha256 else GLib.ChecksumTypeSha1) p (-1)
          let head' = case either (const []) snd commits of
                [] -> Nothing
                cs -> Just (unparse (last cs).sha)
              links = Files.lineLinks summary.forge (pullUrl summary) head' (\p -> fromMaybe "" (lookup p hashes))
          pure ((\f -> (f, either (const (Threads [] Nothing)) id threads, me, c, links)) <$> files)
    in loadInto withThreads $ \((from, changes), threads, me, c, links) ->
      Files.new Files.Actions
        { review = source.review summary.host summary.repo summary.number
        , merge = source.merge summary.host summary.repo summary.number Squash True
        , version = \old path ->
            fmap snd <$> source.file summary.host summary.repo ((if old then snd else fst) summary.branches) path
        , refresh = reopen
        , reply = source.reply summary.host summary.repo summary.number
        , discard = fmap (() <$) (source.discardPending summary.host summary.repo summary.number)
        }
        panels
        (Files.fromFileDiffs c (said from "Diff computed locally" "Diff from the forge's API") changes)
          {Files.threads = threads, Files.me = me, Files.links = links}
  PullRequest.Commits ->
    loadInto (source.pullCommits summary.host summary.repo summary.number) $ \(from, commits) -> do
      c <- clock
      let changesOf commit = case parse commit.hash of
            Left _ -> pure (Left "That commit hash is not one usta can read.")
            Right sha -> fmap (map Files.fromFileDiff . snd) <$> source.commitDiff summary.host summary.repo sha
      Commits.new (said from "history from the local mirror" "history from the forge's API") changesOf
        (map (Commits.fromCommit c) commits)
  PullRequest.Checks ->
    loadInto (source.checks summary.host summary.repo summary.number) $ \found ->
      Checks.new Checks.Model
        { provider = if summary.forge == Forge.Forgejo then "Forgejo Actions" else "Checks"
        , checks = found
        , logOf = source.checkLog summary.host summary.repo
        }

-- | The labels picker of an issue or pull request; @refresh@ reloads the
-- page after a change.
labelPicker :: Source -> Text -> Text -> Int -> IO () -> [Text] -> LabelPicker.Picker
labelPicker source host repo n refresh current = LabelPicker.Picker
  { repo = repo
  , current = current
  , load = source.labels host repo
  , save = source.setLabels host repo n
  , create = source.createLabel host repo
  , manage = do
      launcher <- Gtk.uriLauncherNew (Just ("https://" <> host <> "/" <> repo <> "/labels"))
      Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
  , saved = refresh
  }

-- | A push, which the Conversation tab shows as its commits instead.
pushed :: Tl.Event -> Bool
pushed e = case e.what of
  Tl.Pushed _ -> True
  _ -> False

-- | What comments on an issue or pull request can do.
talkFor :: Source -> Text -> Text -> Int -> IO () -> Text -> Clock -> Issue.Talk
talkFor source host repo n refresh me c = Issue.Talk
  { me = me
  , clock = c
  , editComment = source.editComment host repo n
  , deleteComment = source.deleteComment host repo
  , react = source.react host repo n
  , refresh = refresh
  }

-- | The assignees picker of an issue or pull request, for @me@ (a login
-- as text) and who it is assigned to.
assignPicker :: Source -> Text -> Text -> Int -> IO () -> Text -> [Login] -> AssignPickers.Assignees
assignPicker source host repo n refresh me current = AssignPickers.Assignees
  { current = current
  , me = either (const Nothing) Just (parse me)
  , load = source.assignable host repo
  , save = source.setAssignees host repo n
  , saved = refresh
  }

-- | The milestone picker of an issue or pull request.
milestonePicker :: Source -> Text -> Text -> Int -> IO () -> Clock -> Maybe Text -> AssignPickers.Milestones
milestonePicker source host repo n refresh c current = AssignPickers.Milestones
  { current = current
  , load = source.milestones host repo
  , save = source.setMilestone host repo n
  , manage = do
      launcher <- Gtk.uriLauncherNew (Just ("https://" <> host <> "/" <> repo <> "/milestones"))
      Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
  , saved = refresh
  , clock = c
  }

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
  , PullRequest.branches = maybe ("", "") id n.branch
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
  , Issue.comments = []
  , Issue.events = []
  , Issue.timeline = []
  , Issue.assignees = []
  , Issue.labels = []
  , Issue.milestone = Nothing
  , Issue.linkedPullRequests = []
  }

-- | An issue's header from its list row; the thread is loaded after.
issueDetail :: Issues.Issue -> Issue.Model
issueDetail i = Issue.Model
  { Issue.forge = i.forge
  , Issue.host = i.host
  , Issue.repo = i.repo
  , Issue.number = i.number
  , Issue.title = i.title
  , Issue.closed = i.closed
  , Issue.comments = []
  , Issue.events = []
  , Issue.timeline = []
  , Issue.assignees = []
  , Issue.labels = [(tone, name) | Issues.Label tone name <- i.labels]
  , Issue.milestone = Nothing
  , Issue.linkedPullRequests = []
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

-- | A pull request's page on its forge.
pullUrl :: PullRequest.Summary -> Text
pullUrl s = "https://" <> s.host <> "/" <> s.repo <> path <> T.pack (show s.number)
  where
    path = case s.forge of
      Forge.GitHub -> "/pull/"
      Forge.GitLab -> "/-/merge_requests/"
      Forge.Forgejo -> "/pulls/"

-- | A tab's title with its count, e.g. "Pull requests  3"; no count for 0.
counted :: Text -> Int -> Text
counted title n = if n > 0 then title <> "  " <> T.pack (show n) else title

summaryOf :: PullRequests.PullRequest -> PullRequest.Summary
summaryOf pr = PullRequest.Summary
  { PullRequest.forge = pr.forge
  , PullRequest.host = pr.host
  , PullRequest.repo = pr.repo
  , PullRequest.number = pr.number
  , PullRequest.title = pr.title
  , PullRequest.state = case pr.state of
      P.Open -> PullRequest.Open
      P.Draft -> PullRequest.Draft
      P.Merged -> PullRequest.MergedState
      P.Closed -> PullRequest.Closed
  , PullRequest.comments = pr.comments
  , PullRequest.files = fst pr.changes
  , PullRequest.commits = snd pr.changes
  , PullRequest.checks = case pr.checks of
      PullRequests.ChecksPassed -> Just (Success, "1/1")
      PullRequests.ChecksFailed -> Just (Danger, "1 failed")
      PullRequests.ChecksRunning -> Just (Warning, "running")
      PullRequests.NoChecks -> Nothing
  , PullRequest.branches = (pr.branch, pr.base)
  }

-- | The single-key shortcuts, for the footer strip and Settings.
shortcuts :: [(Text, Text)]
shortcuts =
  [("j k", "move"), ("Enter", "open"), ("e", "done"), ("Shift E", "all done"), ("o", "open in browser")]

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
  -- The skeleton shows only if loading takes longer than 300 ms.
  placeholder <- Skeleton.detail
  Skeleton.delayed placeholder
  #append slot placeholder
  void $ forkIO $ do
    result <- try load
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      widget <- case result of
        Right (Right a) -> render a
        Right (Left problem) -> notice problem
        Left (_ :: SomeException) -> notice "Loading failed unexpectedly."
      Skeleton.appear widget
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
  , NewPullRequest.css, Issues.css, Issue.css, MarkdownEditor.css, CodeView.css, RepositoryFiles.css, ImageView.css, PdfView.css, Markdown.css, Mermaid.css, Thread.css, LabelPicker.css, AssignPickers.css, CommentCard.css, TimelineRow.css, RepositoryActions.css, SidePanel.css, BranchPicker.css, RepositoryCommits.css
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
