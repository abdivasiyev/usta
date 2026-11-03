{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Repositories across every forge: a filterable list on the left, the
-- selected repository's overview on the right.
module Usta.UI.Pages.Repositories
  ( Commit (..)
  , Block (..)
  , Repository (..)
  , fromRepository
  , ChooseMirror
  , sample
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.SearchField as SearchField
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.UI.Page (Page (..), refill)
import Usta.UI.Style (Tone (..))
import qualified Usta.Git as Git
import Usta.Types.Mirror (Choice (..), Transport (..))
import Usta.Types.Parse (unparse)
import qualified Usta.Types.Repository as R

type Commit :: Type
data Commit = Commit
  { hash :: Text
  , message :: Text
  , age :: Text
  }

-- | The README, already parsed from Markdown into the blocks we show.
type Block :: Type
data Block = Heading Text | Paragraph Text | Code Text

type Repository :: Type
data Repository = Repository
  { forge :: Forge.Kind
  , host :: Text
  , name :: Text
    -- ^ Full name, e.g. "abdivasiyev/usta".
  , description :: Text
  , pullRequests :: Int
  , issues :: Int
  , defaultBranch :: Text
  , mirror :: (Tone, Text)
    -- ^ State of the local mirror, e.g. (Success, "Up to date").
  , mirrorDetail :: Maybe Text
    -- ^ More about that state, such as why a fetch failed.
  , canMirror :: Bool
    -- ^ Offers "Mirror…": the repository has no mirror yet, or its mirror
    -- failed.
  , recentCommits :: [Commit]
  , readme :: [Block]
  }

-- | @onNewIssue@ and @onNewPullRequest@ run for the header buttons, with the
-- repository shown.
-- | The selected repository stays selected when the list is updated; the
-- first one is selected when it is gone.
-- | Records the user's answer for a repository's mirror; 'Left' says why
-- it did not work. Runs off the GTK main thread, since it asks the forge.
type ChooseMirror :: Type
type ChooseMirror = Repository -> Choice -> IO (Either Text ())

new :: (Repository -> IO ()) -> (Repository -> IO ()) -> ChooseMirror -> [Repository] -> IO (Page Repository)
new onNewIssue onNewPullRequest chooseMirror repositories = do
  items <- newIORef repositories
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #hexpand := True]

  overviewScroll <- GI.new Gtk.ScrolledWindow [#hexpand := True, #vexpand := True]

  list <- GI.new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeSingle
                             , #cssClasses := ["u-repo-list"]
                             ]
  refill list repositoryRow repositories

  selectedKey <- newIORef Nothing
  void $ GI.on list #rowSelected $ \selected -> case selected of
    Nothing -> Gtk.scrolledWindowSetChild overviewScroll (Nothing :: Maybe Gtk.Widget)
    Just row -> do
      index <- Gtk.listBoxRowGetIndex row
      shown <- readIORef items
      when (index >= 0 && fromIntegral index < length shown) $ do
        let r = shown !! fromIntegral index
        writeIORef selectedKey (Just (key r))
        content <- overview onNewIssue onNewPullRequest chooseMirror r
        GI.set overviewScroll [#child := content]

  (search, entry) <- SearchField.new SearchField.defaults
    { SearchField.placeholder = "Filter repositories"
    , SearchField.shortcut = Nothing
    , SearchField.width = -1
    }
  query <- newIORef ""
  Gtk.listBoxSetFilterFunc list $ Just $ \row -> do
    q <- readIORef query
    index <- Gtk.listBoxRowGetIndex row
    shown <- readIORef items
    pure (matches q (shown !! fromIntegral index))
  void $ GI.on entry #searchChanged $ do
    text <- Gtk.editableGetText entry
    writeIORef query (T.toLower text)
    Gtk.listBoxInvalidateFilter list

  searchBar <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["search-bar"]]
  #append searchBar search

  listScroll <- GI.new Gtk.ScrolledWindow [ #child := list
                                          , #vexpand := True
                                          , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                          ]
  listPane <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #widthRequest := 288
                             , #hexpand := False
                             , #cssClasses := ["u-repo-pane"]
                             ]
  #append listPane searchBar
  #append listPane listScroll

  #append page listPane
  #append page overviewScroll

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
    , update = \shown -> do
        writeIORef items shown
        refill list repositoryRow shown
        select
    }
  where
    key r = (r.host, r.name)

-- | Case-insensitive match on the name or host; @q@ is already lowercase.
matches :: Text -> Repository -> Bool
matches q r = q `T.isInfixOf` T.toLower r.name || q `T.isInfixOf` T.toLower r.host

repositoryRow :: Repository -> IO Gtk.Box
repositoryRow r = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
  nameLine <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Forge.smallDot r.forge >>= #append nameLine
  label r.name ["mono", "name"] >>= #append nameLine
  #append row nameLine
  label (T.intercalate " · " (r.host : counts r)) ["meta"] >>= #append row
  pure row

-- | "1 PR", "3 issues"; nothing for zero.
counts :: Repository -> [Text]
counts r =
  [ plural r.pullRequests "PR" "PRs" | r.pullRequests > 0 ]
  <> [ plural r.issues "issue" "issues" | r.issues > 0 ]
  where
    plural n one many = T.pack (show n) <> " " <> if n == 1 then one else many

overview :: (Repository -> IO ()) -> (Repository -> IO ()) -> ChooseMirror -> Repository -> IO Gtk.Box
overview onNewIssue onNewPullRequest chooseMirror r = do
  pane <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 20
                         , #cssClasses := ["u-repo"]
                         ]

  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  titles <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 4
                           , #hexpand := True
                           ]
  label r.host ["muted"] >>= #append titles
  label r.name ["mono", "repo-name"] >>= #append titles
  descriptionLabel <- label r.description ["description"]
  GI.set descriptionLabel [#wrap := True]
  #append titles descriptionLabel
  #append header titles
  newIssue <- Button.new Button.defaults { Button.label = Just "New issue" }
  void $ GI.on newIssue #clicked (onNewIssue r)
  newPullRequest <- Button.new Button.defaults
    { Button.label = Just "New pull request", Button.variant = Button.Primary }
  void $ GI.on newPullRequest #clicked (onNewPullRequest r)
  mirrorButtons <- if r.canMirror
    then do
      b <- Button.new Button.defaults { Button.label = Just "Mirror…" }
      void $ GI.on b #clicked $ Gtk.toWidget b >>= \anchor -> askMirror anchor r chooseMirror
      pure [b]
    else pure []
  forM_ (mirrorButtons <> [newIssue, newPullRequest]) $ \b -> do
    GI.set b [#valign := Gtk.AlignStart]
    #append header b
  #append pane header

  stats <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 12
                          , #homogeneous := True
                          ]
  Card.stat Neutral "Open pull requests" (T.pack (show r.pullRequests)) >>= #append stats
  Card.stat Neutral "Open issues" (T.pack (show r.issues)) >>= #append stats
  branch <- Card.stat Neutral "Default branch" r.defaultBranch
  Gtk.widgetAddCssClass branch "branch"
  #append stats branch
  let (mirrorTone, mirrorText) = r.mirror
  mirrorCard <- Card.stat mirrorTone "Local mirror" mirrorText
  forM_ r.mirrorDetail $ \detail -> GI.set mirrorCard [#tooltipText := detail]
  #append stats mirrorCard
  #append pane stats

  when (not (null r.recentCommits)) $ do
    section <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
    Card.sectionTitle ("Recent commits on " <> r.defaultBranch) >>= #append section
    card <- Card.new
    forM_ r.recentCommits $ \c -> do
      line <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                             , #spacing := 12
                             , #cssClasses := ["commit-row"]
                             ]
      label (T.take 7 c.hash) ["mono", "hash"] >>= #append line
      messageLabel <- label c.message []
      GI.set messageLabel [#hexpand := True]
      #append line messageLabel
      label c.age ["muted", "age"] >>= #append line
      #append card line
    #append section card
    #append pane section

  when (not (null r.readme)) $ do
    section <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
    Card.sectionTitle "README.md" >>= #append section
    card <- Card.new
    Gtk.widgetAddCssClass card "readme"
    GI.set card [#spacing := 16]
    forM_ r.readme $ \block -> blockWidget block >>= #append card
    #append section card
    #append pane section

  pure pane

blockWidget :: Block -> IO Gtk.Widget
blockWidget block = case block of
  Heading text -> label text ["heading"] >>= Gtk.toWidget
  Paragraph text -> do
    l <- label text []
    GI.set l [#wrap := True]
    Gtk.toWidget l
  Code text -> do
    l <- label text ["mono", "code"]
    GI.set l [#selectable := True]
    Gtk.toWidget l

-- | Asks whether to mirror @r@, and how. Its "Don't mirror" is kept, so the
-- question is not asked again; the button stays for changing one's mind.
askMirror :: Gtk.Widget -> Repository -> ChooseMirror -> IO ()
askMirror anchor r chooseMirror = do
  root <- Gtk.widgetGetRoot anchor
  parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
  dialog <- GI.new Gtk.Window [ #title := "Mirror " <> r.name <> "?"
                              , #modal := True
                              , #resizable := False
                              , #destroyWithParent := True
                              , #defaultWidth := 520
                              , #cssClasses := ["usta", "u-mirror"]
                              ]
  forM_ parent $ \p -> Gtk.windowSetTransientFor dialog (Just p)
  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12, #cssClasses := ["body"]]
  label ("Mirror " <> r.name <> "?") ["title"] >>= #append body
  wrapped "A local mirror makes diffs and history fast, and works offline. Without one, usta asks the forge each time." ["muted"]
    >>= #append body

  label "What to download" ["caption"] >>= #append body
  (depth, depthButtons) <- Segmented.new ["Everything", "History only"] 0
  GI.set depth [#halign := Gtk.AlignStart]
  #append body depth
  wrapped "Everything: the whole repository, once. History only: file contents are fetched the first time a diff needs them." ["hint"]
    >>= #append body

  label "Connect over" ["caption"] >>= #append body
  (over, overButtons) <- Segmented.new ["HTTPS", "SSH"] 0
  GI.set over [#halign := Gtk.AlignStart]
  #append body over
  wrapped "HTTPS uses your account's token. SSH uses your own keys; connect to the server with ssh once first, so its host key is known." ["hint"]
    >>= #append body

  problem <- wrapped "" ["status", "problem"]
  GI.set problem [#visible := False]
  #append body problem

  footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  decline <- Button.new Button.defaults { Button.label = Just "Don't mirror" }
  spacer <- GI.new Gtk.Box [#hexpand := True]
  cancel <- Button.new Button.defaults { Button.label = Just "Cancel" }
  go <- Button.new Button.defaults { Button.label = Just "Mirror", Button.variant = Button.Primary }
  #append footer decline
  #append footer spacer
  #append footer cancel
  #append footer go
  #append body footer

  let answer choice busyLabel = do
        GI.set go [#sensitive := False, #label := busyLabel]
        GI.set decline [#sensitive := False]
        void $ forkIO $ do
          outcome <- try (chooseMirror r choice)
          let result = case outcome of
                Left (_ :: SomeException) -> Left "Choosing the mirror failed unexpectedly."
                Right done -> done
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            case result of
              Right () -> Gtk.windowDestroy dialog
              Left message -> do
                GI.set problem [#label := message, #visible := True]
                GI.set go [#sensitive := True, #label := "Mirror"]
                GI.set decline [#sensitive := True]
            pure False
  void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
  void $ GI.on decline #clicked (answer Declined "Mirror")
  void $ GI.on go #clicked $ case (depthButtons, overButtons) of
    (everything : _, viaHttps : _) -> do
      full <- GI.get everything #active
      https <- GI.get viaHttps #active
      answer (Mirrored (if full then Git.Full else Git.Partial) (if https then Https else Ssh)) "Starting…"
    _ -> pure ()
  GI.set dialog [#child := body]
  #present dialog
  where
    wrapped text classes = do
      l <- label text classes
      GI.set l [#wrap := True]
      pure l

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

-- | The list entry for a synced repository. Commits and the README come
-- from the local mirror, which is not read yet; the mirror state is the
-- source's to fill in.
fromRepository :: Forge.Kind -> R.Repository -> Repository
fromRepository forge r = Repository
  { forge = forge
  , host = unparse r.host
  , name = unparse r.name
  , description = fromMaybe "" r.description
  , pullRequests = unparse r.openPullRequests
  , issues = unparse r.openIssues
  , defaultBranch = r.defaultBranch
  , mirror = (Neutral, "Not mirrored")
  , mirrorDetail = Nothing
  , canMirror = False
  , recentCommits = []
  , readme = []
  }

sample :: [Repository]
sample =
  [ Repository Forge.GitHub "github.com" "abdivasiyev/usta"
      "A desktop client to connect with your forge" 1 3 "master" (Success, "Up to date") Nothing False
      [ Commit "a41f0c2" "feat: scaffold Nix flake and Hello world app" "3h"
      , Commit "8427b36" "Initial commit" "2 days"
      ]
      [ Heading "usta"
      , Paragraph "A desktop client to connect with your forge."
      , Code "nix run"
      ]
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab"
      "NixOS configuration for the home server" 2 0 "master" (Success, "Up to date") Nothing False
      [Commit "5c2e91d" "feat(cache): serve the binary cache over HTTPS" "1d"]
      [Heading "homelab", Paragraph "NixOS configuration for the home server.", Code "nix run .#deploy"]
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix"
      "Nix configurations for my machines" 0 1 "master" (Warning, "2 behind") Nothing False
      [Commit "e03b7aa" "fix: restart nix-daemon after substituter changes" "3d"]
      []
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/cartoons"
      "Cartoons made with Remotion" 0 0 "main" (Success, "Up to date") Nothing False
      [] []
  , Repository Forge.GitHub "github.com" "abdivasiyev/dotfiles"
      "Shell and editor settings" 0 0 "master" (Neutral, "Not mirrored") Nothing True
      [] []
  ]

css :: Text
css = mconcat
  [ "window.u-mirror box.body { padding: 20px; }"
  , "window.u-mirror label.title { font-size: 15px; font-weight: 600; }"
  , "window.u-mirror label.caption { font-weight: 600; margin-top: 4px; }"
  , "window.u-mirror label.hint { font-size: 12px; color: var(--text-muted); }"
  , "window.u-mirror label.problem { color: var(--danger); }"
  , "box.u-repo-pane { background: var(--surface); border-right: 1px solid var(--border); }"
  , "box.u-repo-pane box.search-bar { padding: 12px; }"
  , "list.u-repo-list { background: var(--surface); }"
  , "list.u-repo-list row {"
  , "  padding: 12px 16px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: var(--surface); color: var(--text);"
  , "}"
  , "list.u-repo-list row:hover { background: var(--window); }"
  , "list.u-repo-list row:selected { background: var(--selected); color: var(--text); }"
  , "list.u-repo-list label.name { font-weight: 600; }"
  , "list.u-repo-list label.meta { font-size: 12px; color: var(--text-muted); }"
  , "box.u-repo { padding: 24px 32px; }"
  , "box.u-repo label.repo-name { font-size: 26px; font-weight: 700; }"
  , "box.u-repo label.description { font-size: 15px; color: var(--text-soft); }"
  , "box.u-repo box.u-stat { padding: 14px 12px; }"
  , "box.u-repo box.u-stat label.value { font-size: 18px; }"
  , "box.u-repo box.u-stat.branch label.value { font-family: 'JetBrains Mono', monospace; font-size: 16px; }"
  , "box.u-repo box.commit-row { padding: 10px 12px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-repo box.commit-row:last-child { border-bottom: none; }"
  , "box.u-repo label.hash { color: var(--accent); }"
  , "box.u-repo label.age { font-size: 13px; }"
  , "box.u-repo box.readme { padding: 20px; }"
  , "box.u-repo box.readme label.heading { font-size: 20px; font-weight: 600; }"
  , "box.u-repo box.readme label.code {"
  , "  padding: 12px; border-radius: 6px; background: var(--neutral-bg); color: var(--text);"
  , "}"
  ]
