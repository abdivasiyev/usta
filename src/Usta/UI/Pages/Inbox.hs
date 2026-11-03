{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The inbox: notifications from every forge on the left, the selected
-- one previewed on the right.
module Usta.UI.Pages.Inbox
  ( Kind (..)
  , Reason (..)
  , Change (..)
  , Notification (..)
  , View (..)
  , Actions (..)
  , lists
  , webUrl
  , fromNotification
  , sample
  , new
  , syncing
  , css
  ) where

import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Components.Skeleton as Skeleton
import Usta.UI.Page (Page (..), refill)
import Usta.UI.Format (Clock, ago, short)
import Usta.UI.Style (Tone (..))
import Usta.Types.Involvement (Involvement (..))
import qualified Usta.Types.Notification as N
import Usta.Types.Parse (unparse)
import qualified Usta.Types.PullRequest as P

type Kind :: Type
data Kind = PullRequest | MergedPullRequest | Issue
  deriving (Eq, Show)

-- | Why a notification is in the inbox. Add a constructor and its label
-- and tone to introduce a new one.
type Reason :: Type
data Reason = ReviewRequested | ChecksFailed | Mentioned | Assigned | WasMerged
  deriving (Eq, Show, Enum, Bounded)

reasonBadge :: Reason -> (Tone, Text)
reasonBadge reason = case reason of
  ReviewRequested -> (Warning, "Review requested")
  ChecksFailed -> (Danger, "Checks failed")
  Mentioned -> (Accent, "Mentioned")
  Assigned -> (Neutral, "Assigned")
  WasMerged -> (Merged, "Merged")

-- | A changed file: path, lines added, lines removed.
type Change :: Type
data Change = Change Text Int Int

type Notification :: Type
data Notification = Notification
  { kind :: Kind
  , forge :: Forge.Kind
  , host :: Text
  , repo :: Text
  , number :: Int
  , title :: Text
  , reason :: Maybe Reason
    -- ^ 'Nothing' when the forge's reason has no badge, or it gives none.
  , unread :: Bool
  , done :: Bool
    -- ^ Marked done in usta.
  , age :: Text
  , branch :: Maybe (Text, Text)
    -- ^ Source and target branch, for pull requests.
  , opened :: Text
  , checks :: Maybe (Tone, Text)
  , review :: Maybe (Tone, Text)
  , changes :: [Change]
  }

-- | Which notifications a page lists.
type View :: Type
data View
  = Everything
    -- ^ The inbox: unread and not marked done.
  | Only Reason
    -- ^ The inbox, with that badge only.
  | Finished
    -- ^ Marked done here, or read on the forge.
  deriving (Eq, Show)

lists :: View -> Notification -> Bool
lists view n = case view of
  Everything -> waiting
  Only reason -> waiting && n.reason == Just reason
  Finished -> not waiting
  where
    waiting = n.unread && not n.done

-- | What the preview's buttons do.
type Actions :: Type
data Actions = Actions
  { open :: Notification -> IO ()
    -- ^ "Open review" or "Open issue".
  , setDone :: Notification -> Bool -> IO ()
    -- ^ "Mark as done" ('True') or "Move to inbox" ('False').
  }

-- | The notification's pull request or issue on the forge's website.
webUrl :: Notification -> Text
webUrl n = "https://" <> n.host <> "/" <> n.repo <> "/" <> path <> "/" <> T.pack (show n.number)
  where
    path = case (n.forge, n.kind) of
      (_, Issue) | n.forge == Forge.GitLab -> "-/issues"
                 | otherwise -> "issues"
      (Forge.GitHub, _) -> "pull"
      (Forge.GitLab, _) -> "-/merge_requests"
      (Forge.Forgejo, _) -> "pulls"

type Filter :: Type
data Filter = All | Unread | Participating
  deriving (Eq, Enum, Bounded)

keeps :: Filter -> Notification -> Bool
keeps f n = case f of
  All -> True
  Unread -> n.unread
  Participating -> n.reason `elem` map Just [ReviewRequested, Mentioned, Assigned]

-- | The notifications @view@ lists, out of all of them. The selected one
-- stays selected when the list is updated; the first one is selected when
-- it is gone.
new :: View -> Actions -> [Notification] -> IO (Page Notification)
new view actions notifications = do
  items <- newIORef (filter (lists view) notifications)
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #hexpand := True]

  previewScroll <- GI.new Gtk.ScrolledWindow [#hexpand := True, #vexpand := True]

  list <- GI.new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeSingle
                             , #cssClasses := ["u-inbox-list"]
                             ]
  empty <- GI.new Gtk.Label [ #label := emptyText
                            , #wrap := True
                            , #cssClasses := ["muted", "u-inbox-empty"]
                            ]
  -- Removing every row removes the placeholder too, so it is set again.
  let fill shown = refill list notificationRow shown >> Gtk.listBoxSetPlaceholder list (Just empty)
  readIORef items >>= fill

  selectedKey <- newIORef Nothing
  void $ GI.on list #rowSelected $ \selected -> case selected of
    Nothing -> Gtk.scrolledWindowSetChild previewScroll (Nothing :: Maybe Gtk.Widget)
    Just row -> do
      index <- Gtk.listBoxRowGetIndex row
      shown <- readIORef items
      when (index >= 0 && fromIntegral index < length shown) $ do
        let n = shown !! fromIntegral index
        writeIORef selectedKey (Just (key n))
        content <- preview actions n
        GI.set previewScroll [#child := content]

  -- With the list focused: j and k move, e marks done (or moves back to
  -- the inbox), o opens the forge's page, and Enter opens it here.
  let selectedOne = do
        selected <- Gtk.listBoxGetSelectedRow list
        index <- maybe (pure (-1)) Gtk.listBoxRowGetIndex selected
        shown <- readIORef items
        pure [n | (i, n) <- zip [0 ..] shown, i == index]
      step by = do
        selected <- Gtk.listBoxGetSelectedRow list
        from <- maybe (pure (-1)) Gtk.listBoxRowGetIndex selected
        -- Rows the filter hides are skipped.
        let go at = when (at >= 0) $ do
              row <- Gtk.listBoxGetRowAtIndex list at
              forM_ row $ \r -> do
                visible <- Gtk.widgetGetChildVisible r
                if visible
                  then Gtk.listBoxSelectRow list (Just r) >> void (Gtk.widgetGrabFocus r)
                  else go (at + by)
        go (from + by)
  keys <- GI.new Gtk.ShortcutController []
  shortcut keys "j" (step 1)
  shortcut keys "k" (step (-1))
  shortcut keys "e" (selectedOne >>= mapM_ (\n -> actions.setDone n (not (lists Finished n))))
  shortcut keys "o" (selectedOne >>= mapM_ openInBrowser)
  Gtk.widgetAddController list keys
  void $ GI.on list #rowActivated $ \_ -> selectedOne >>= mapM_ actions.open

  (filters, buttons) <- Segmented.new ["All", "Unread", "Participating"] 0
  countLabel <- GI.new Gtk.Label [ #cssClasses := ["muted", "count"]
                                 , #hexpand := True
                                 , #halign := Gtk.AlignEnd
                                 ]
  -- Done has nothing to filter: everything in it is read.
  doneNote <- GI.new Gtk.Label [ #label := "Marked done here, or read on the forge"
                               , #cssClasses := ["muted", "count"]
                               ]
  current <- newIORef All
  let apply f = do
        writeIORef current f
        Gtk.listBoxInvalidateFilter list
        shown <- readIORef items
        let count = length (filter (keeps f) shown)
        GI.set countLabel [#label := T.pack (show count) <> " items"]
  Gtk.listBoxSetFilterFunc list $ Just $ \row -> do
    f <- readIORef current
    index <- Gtk.listBoxRowGetIndex row
    shown <- readIORef items
    pure (keeps f (shown !! fromIntegral index))
  forM_ (zip [minBound ..] buttons) $ \(f, button) ->
    void $ GI.on button #toggled $ do
      active <- GI.get button #active
      when active (apply f)
  apply All

  filterBar <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                              , #cssClasses := ["u-filter-bar"]
                              ]
  #append filterBar =<< if view == Finished then Gtk.toWidget doneNote else Gtk.toWidget filters
  #append filterBar countLabel

  listScroll <- GI.new Gtk.ScrolledWindow [ #child := list
                                          , #vexpand := True
                                          , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                          ]
  listPane <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #widthRequest := 460
                             , #hexpand := False
                             , #cssClasses := ["u-inbox-pane"]
                             ]
  #append listPane filterBar
  #append listPane listScroll

  #append page listPane
  #append page previewScroll

  let select = do
        shown <- readIORef items
        wanted <- readIORef selectedKey
        let index = maybe 0 (\k -> length (takeWhile ((/= k) . key) shown)) wanted
            at = if index < length shown then index else 0
        row <- Gtk.listBoxGetRowAtIndex list (fromIntegral at)
        Gtk.listBoxSelectRow list row
  select
  pure Page
    { widget = page
    , update = \everything -> do
        let shown = filter (lists view) everything
        writeIORef items shown
        fill shown
        readIORef current >>= apply
        select
    }
  where
    key n = (n.host, n.repo, n.number)
    emptyText = case view of
      Finished -> "Nothing is done yet. Mark a notification done and it moves here."
      Only _ -> "Nothing here: no notification in the inbox has this badge."
      Everything -> "All caught up."

-- | The inbox during the first sync: @note@ on top, skeleton rows and a
-- skeleton preview until notifications arrive.
syncing :: Text -> IO Gtk.Box
syncing note = do
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #hexpand := True]

  listPane <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #widthRequest := 460
                             , #hexpand := False
                             , #cssClasses := ["u-inbox-pane"]
                             ]
  GI.new Gtk.Label [ #label := note
                   , #xalign := 0
                   , #wrap := True
                   , #cssClasses := ["u-sync-note"]
                   ] >>= #append listPane
  rows <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  forM_ (take 7 (cycle [330, 280, 360, 250, 310, 290, 260])) $ \w ->
    Skeleton.listRow w >>= #append rows
  Gtk.toWidget rows >>= Skeleton.delayed
  #append listPane rows

  previewPane <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                                , #spacing := 16
                                , #hexpand := True
                                , #cssClasses := ["u-preview"]
                                ]
  Skeleton.detail >>= #append previewPane
  Skeleton.block 88 >>= #append previewPane
  Gtk.toWidget previewPane >>= Skeleton.delayed

  #append page listPane
  #append page previewPane
  pure page

notificationRow :: Notification -> IO Gtk.Box
notificationRow n = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]

  icon <- case n.kind of
    PullRequest -> Icon.toned 16 Success Icon.PullRequest
    MergedPullRequest -> Icon.toned 16 Merged Icon.PullRequest
    Issue -> Icon.toned 16 Neutral Icon.Issue
  GI.set icon [#valign := Gtk.AlignStart, #marginTop := 2]
  #append row icon

  body <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 4
                         , #hexpand := True
                         ]
  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  Forge.smallDot n.forge >>= #append meta
  label n.repo ["meta", "mono"] >>= #append meta
  label ("#" <> T.pack (show n.number)) ["meta"] >>= #append meta
  ageLabel <- label n.age ["meta"]
  GI.set ageLabel [#hexpand := True, #halign := Gtk.AlignEnd]
  #append meta ageLabel
  #append body meta

  titleLabel <- label n.title (["title"] <> ["unread" | n.unread])
  GI.set titleLabel [#ellipsize := Pango.EllipsizeModeEnd]
  #append body titleLabel

  badges <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  forM_ n.reason $ \reason ->
    let (tone, text) = reasonBadge reason in Badge.new tone text >>= #append badges
  when n.unread $ Badge.dot Accent >>= #append badges
  #append body badges

  #append row body
  pure row

preview :: Actions -> Notification -> IO Gtk.Box
preview actions n = do
  pane <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 20
                         , #cssClasses := ["u-preview"]
                         ]

  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  label n.repo ["muted", "mono"] >>= #append meta
  label ("#" <> T.pack (show n.number)) ["muted"] >>= #append meta
  stateBadge >>= #append meta
  #append header meta
  titleLabel <- label n.title ["preview-title"]
  GI.set titleLabel [#wrap := True]
  #append header titleLabel
  forM_ n.branch $ \(from, into) -> do
    line <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    label from ["chip", "mono"] >>= #append line
    label "into" ["muted"] >>= #append line
    label into ["chip", "mono"] >>= #append line
    label ("· " <> n.opened) ["muted"] >>= #append line
    #append header line
  when (n.branch == Nothing) $ label n.opened ["muted"] >>= #append header
  #append pane header

  when (isJust n.checks || isJust n.review || not (null n.changes)) $ do
    stats <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 12
                            , #homogeneous := True
                            ]
    forM_ n.checks $ \(tone, text) -> Card.stat tone "Checks" text >>= #append stats
    when (not (null n.changes)) $ changesCard n.changes >>= #append stats
    forM_ n.review $ \(tone, text) -> Card.stat tone "Your review" text >>= #append stats
    #append pane stats

  when (not (null n.changes)) $ do
    files <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
    Card.sectionTitle "Files" >>= #append files
    card <- Card.new
    forM_ n.changes $ \(Change path added removed) -> do
      fileRow <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                                , #spacing := 6
                                , #cssClasses := ["file-row"]
                                ]
      pathLabel <- label path ["mono"]
      GI.set pathLabel [#hexpand := True]
      #append fileRow pathLabel
      label ("+" <> T.pack (show added)) ["mono", "added"] >>= #append fileRow
      label ("−" <> T.pack (show removed)) ["mono", "removed"] >>= #append fileRow
      #append card fileRow
    #append files card
    #append pane files

  buttons <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  let primary = if n.kind == Issue then "Open issue" else "Open review"
  open <- Button.new Button.defaults { Button.label = Just primary, Button.variant = Button.Primary }
  void $ GI.on open #clicked (actions.open n)
  #append buttons open
  let finished = lists Finished n
  done <- Button.new Button.defaults
    { Button.label = Just (if finished then "Move to inbox" else "Mark as done") }
  void $ GI.on done #clicked (actions.setDone n (not finished))
  #append buttons done
  browser <- Button.new Button.defaults { Button.label = Just "Open in browser" }
  void $ GI.on browser #clicked (openInBrowser n)
  #append buttons browser
  #append pane buttons

  pure pane
  where
    stateBadge = case n.kind of
      PullRequest -> Badge.new Success "Open"
      MergedPullRequest -> Badge.new Merged "Merged"
      Issue -> Badge.new Success "Open"

openInBrowser :: Notification -> IO ()
openInBrowser n = do
  launcher <- Gtk.uriLauncherNew (Just (webUrl n))
  Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing

changesCard :: [Change] -> IO Gtk.Box
changesCard cs = do
  card <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 4
                         , #cssClasses := ["u-card", "u-stat"]
                         ]
  label "Changes" ["caption"] >>= #append card
  value <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 4]
  let added = sum [a | Change _ a _ <- cs]
      removed = sum [r | Change _ _ r <- cs]
      count = length cs
      noun = if count == 1 then " file" else " files"
  label ("+" <> T.pack (show added)) ["value", "added"] >>= #append value
  label ("−" <> T.pack (show removed)) ["value", "removed"] >>= #append value
  label ("in " <> T.pack (show count) <> noun) ["value"] >>= #append value
  #append card value
  pure card

-- | The inbox row for a forge's notification, with the pull request it is
-- about when that is synced, and how the item involves the user (for a
-- forge that does not say why it notified). 'Nothing' for a release, commit or discussion,
-- which the inbox does not show yet.
-- | @done@ says it was marked done in usta.
fromNotification :: Clock -> Forge.Kind -> N.Notification -> Maybe P.PullRequest -> [Involvement] -> Bool -> Maybe Notification
fromNotification c forge n pr involvement done = do
  kind <- case n.subject of
    N.PullRequestSubject _
      | fmap (.state) pr == Just P.Merged -> Just MergedPullRequest
      | otherwise -> Just PullRequest
    N.IssueSubject _ -> Just Issue
    N.OtherSubject -> Nothing
  number <- case n.subject of
    N.PullRequestSubject num -> Just num
    N.IssueSubject num -> Just num
    N.OtherSubject -> Nothing
  pure Notification
    { kind = kind
    , forge = forge
    , host = unparse n.host
    , repo = unparse n.repo
    , number = unparse number
    , title = n.title
    , reason = badge kind
    , unread = n.unread
    , done = done
    , age = short c n.updatedAt
    , branch = fmap (\p -> (p.headBranch, p.baseBranch)) pr
    , opened = case pr of
        Just p | kind == PullRequest -> "opened " <> ago c p.createdAt
        _ -> "updated " <> ago c n.updatedAt
    , checks = fmap checks (pr >>= (.checks))
    , review = fmap review (pr >>= (.review))
    , changes = []
      -- Per-file changes come from the local mirror, which is not built yet.
    }
  where
    badge kind
      | kind == MergedPullRequest = Just WasMerged
      | otherwise = case n.reason of
          Just N.ReviewRequested -> Just ReviewRequested
          Just N.Mentioned -> Just Mentioned
          Just N.TeamMentioned -> Just Mentioned
          Just N.Assigned -> Just Assigned
          -- A forge that gives no reason (Forgejo): how it involves you.
          Nothing
            | AsReviewer `elem` involvement -> Just ReviewRequested
            | AsAssignee `elem` involvement -> Just Assigned
            | AsMentioned `elem` involvement -> Just Mentioned
          _ | (pr >>= (.checks)) == Just P.ChecksFailed -> Just ChecksFailed
            | otherwise -> Nothing
    checks state = case state of
      P.ChecksPassed -> (Success, "Passed")
      P.ChecksFailed -> (Danger, "Failed")
      P.ChecksRunning -> (Warning, "Running")
    review state = case state of
      P.AwaitingReview -> (Warning, "Requested")
      P.Approved -> (Success, "Approved")
      P.ChangesRequested -> (Danger, "Changes requested")

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [Notification]
sample =
  [ pr Forge.GitHub "abdivasiyev/usta" 2 "feat: add gi-gtk4 dependency" ReviewRequested True "5m"
      ("feat/gtk4-dep", "master") "opened 5 min ago"
      (Just (Success, "1 of 1 passed")) (Just (Warning, "Requested"))
      [Change "usta.cabal" 3 1, Change "flake.nix" 1 1]
  , pr Forge.Forgejo "abdivasiyev/homelab" 48 "chore: move cache priorities into nix.settings" ChecksFailed True "22m"
      ("cache-priorities", "master") "opened 22 min ago"
      (Just (Danger, "1 of 2 failed")) (Just (Warning, "Requested"))
      [Change "modules/cache.nix" 12 4]
  , issue Forge.GitHub "abdivasiyev/usta" 3 "Diff view: keep scroll position when switching files" Mentioned True "1h"
  , merged Forge.GitHub "abdivasiyev/usta" 1 "feat: scaffold Nix flake and Hello world app" "3h"
  , issue Forge.Forgejo "abdivasiyev/nix" 12 "Restart nix-daemon after substituter changes" Assigned False "Yesterday"
  , pr Forge.Forgejo "abdivasiyev/homelab" 47 "feat: add binary cache health check" ReviewRequested False "Yesterday"
      ("cache-health", "master") "opened yesterday"
      (Just (Warning, "Running")) (Just (Warning, "Requested"))
      [Change "modules/cache-health.nix" 40 0, Change "hosts/homelab.nix" 2 0]
  , issue Forge.GitHub "abdivasiyev/usta" 4 "Support GitLab merge request discussions" Assigned False "Mon"
  ]
  where
    pr f r num t why new' age br op ch rv cs =
      Notification PullRequest f (hostOf f) r num t (Just why) new' False age (Just br) op ch rv cs
    merged f r num t age =
      Notification MergedPullRequest f (hostOf f) r num t (Just WasMerged) False False age Nothing ("merged " <> age <> " ago") Nothing Nothing []
    issue f r num t why new' age =
      Notification Issue f (hostOf f) r num t (Just why) new' False age Nothing ("opened " <> age <> " ago") Nothing Nothing []
    hostOf f = case f of
      Forge.GitHub -> "github.com"
      Forge.GitLab -> "gitlab.com"
      Forge.Forgejo -> "git.azizovich.uz"

css :: Text
css = mconcat
  [ "box.u-inbox-pane { background: var(--surface); border-right: 1px solid var(--border); }"
  , "label.u-sync-note {"
  , "  padding: 12px 16px; font-size: 14px; color: var(--text-soft);"
  , "  border-bottom: 1px solid var(--border);"
  , "}"
  , "box.u-filter-bar { padding: 10px 12px; border-bottom: 1px solid var(--border); }"
  , "box.u-filter-bar label.count { font-size: 12px; }"
  , "list.u-inbox-list { background: var(--surface); }"
  , "label.u-inbox-empty { padding: 32px 24px; font-size: 14px; }"
  , "list.u-inbox-list row {"
  , "  padding: 12px 16px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: var(--surface); color: var(--text);"
  , "}"
  , "list.u-inbox-list row:hover { background: var(--window); }"
  , "list.u-inbox-list row:selected {"
  , "  background: var(--selected); color: var(--text); box-shadow: inset 3px 0 0 var(--accent);"
  , "}"
  , "list.u-inbox-list label.meta { font-size: 12px; color: var(--text-muted); }"
  , "list.u-inbox-list label.title.unread { font-weight: 600; }"
  , "box.u-preview { padding: 24px 32px; }"
  , "box.u-preview label.preview-title { font-size: 22px; font-weight: 600; }"
  , "box.u-preview label.chip {"
  , "  background: var(--neutral-bg); color: var(--text); padding: 1px 6px; border-radius: 4px;"
  , "}"
  , "box.u-preview box.file-row { padding: 10px 12px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-preview box.file-row:last-child { border-bottom: none; }"
  , "label.added { color: var(--success); }"
  , "label.removed { color: var(--danger); }"
  ]
