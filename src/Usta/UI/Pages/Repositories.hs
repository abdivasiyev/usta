{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Repositories across every forge: a filterable list on the left, the
-- selected repository's overview on the right.
module Usta.UI.Pages.Repositories
  ( Commit (..)
  , Block (..)
  , Repository (..)
  , fromRepository
  , sample
  , new
  , css
  ) where

import Control.Monad (forM, forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.SearchField as SearchField
import Usta.UI.Style (Tone (..))
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
  , recentCommits :: [Commit]
  , readme :: [Block]
  }

-- | @onNewIssue@ and @onNewPullRequest@ run for the header buttons, with the
-- repository shown.
new :: (Repository -> IO ()) -> (Repository -> IO ()) -> [Repository] -> IO Gtk.Box
new onNewIssue onNewPullRequest repositories = do
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #hexpand := True]

  overviewScroll <- GI.new Gtk.ScrolledWindow [#hexpand := True, #vexpand := True]

  list <- GI.new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeSingle
                             , #cssClasses := ["u-repo-list"]
                             ]
  rows <- forM repositories $ \r -> do
    row <- GI.new Gtk.ListBoxRow [#child GI.:=> repositoryRow r]
    #append list row
    pure row

  void $ GI.on list #rowSelected $ \selected ->
    forM_ selected $ \row -> do
      index <- Gtk.listBoxRowGetIndex row
      when (index >= 0 && fromIntegral index < length repositories) $ do
        content <- overview onNewIssue onNewPullRequest (repositories !! fromIntegral index)
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
    pure (matches q (repositories !! fromIntegral index))
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

  forM_ (take 1 rows) $ Gtk.listBoxSelectRow list . Just
  pure page

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

overview :: (Repository -> IO ()) -> (Repository -> IO ()) -> Repository -> IO Gtk.Box
overview onNewIssue onNewPullRequest r = do
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
  forM_ [newIssue, newPullRequest] $ \b -> do
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
  Card.stat mirrorTone "Local mirror" mirrorText >>= #append stats
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

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

-- | The list entry for a synced repository. Commits, the README and the
-- mirror state come from the local mirror, which is not built yet.
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
  , recentCommits = []
  , readme = []
  }

sample :: [Repository]
sample =
  [ Repository Forge.GitHub "github.com" "abdivasiyev/usta"
      "A desktop client to connect with your forge" 1 3 "master" (Success, "Up to date")
      [ Commit "a41f0c2" "feat: scaffold Nix flake and Hello world app" "3h"
      , Commit "8427b36" "Initial commit" "2 days"
      ]
      [ Heading "usta"
      , Paragraph "A desktop client to connect with your forge."
      , Code "nix run"
      ]
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab"
      "NixOS configuration for the home server" 2 0 "master" (Success, "Up to date")
      [Commit "5c2e91d" "feat(cache): serve the binary cache over HTTPS" "1d"]
      [Heading "homelab", Paragraph "NixOS configuration for the home server.", Code "nix run .#deploy"]
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix"
      "Nix configurations for my machines" 0 1 "master" (Warning, "2 behind")
      [Commit "e03b7aa" "fix: restart nix-daemon after substituter changes" "3d"]
      []
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/cartoons"
      "Cartoons made with Remotion" 0 0 "main" (Success, "Up to date")
      [] []
  , Repository Forge.GitHub "github.com" "abdivasiyev/dotfiles"
      "Shell and editor settings" 0 0 "master" (Neutral, "Not mirrored")
      [] []
  ]

css :: Text
css = mconcat
  [ "box.u-repo-pane { background: var(--surface); border-right: 1px solid var(--border); }"
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
