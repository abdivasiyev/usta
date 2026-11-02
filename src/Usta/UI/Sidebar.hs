{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The left sidebar: inbox sections, the main pages, forges and pinned
-- repositories.
module Usta.UI.Sidebar
  ( Page (..)
  , pageName
  , pageTitle
  , ForgeKind (..)
  , Model (..)
  , sample
  , new
  , css
  ) where

import Control.Monad (forM, forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.NavItem as NavItem

-- | Every place the sidebar can take you.
data Page
  = Inbox
  | ReviewRequests
  | AssignedToMe
  | Mentions
  | Done
  | PullRequests
  | Issues
  | Repositories
  deriving (Eq, Show, Enum, Bounded)

-- | A stable name, e.g. for a 'Gtk.Stack' child.
pageName :: Page -> Text
pageName = T.toLower . T.pack . show

pageTitle :: Page -> Text
pageTitle page = case page of
  Inbox -> "Inbox"
  ReviewRequests -> "Review requests"
  AssignedToMe -> "Assigned to me"
  Mentions -> "Mentions"
  Done -> "Done"
  PullRequests -> "Pull requests"
  Issues -> "Issues"
  Repositories -> "Repositories"

data ForgeKind = GitHub | GitLab | Forgejo
  deriving (Eq, Show)

-- | What the sidebar shows. Comes from the cache once sync exists.
data Model = Model
  { counts :: Page -> Maybe Int
  , forges :: [(ForgeKind, Text)]
  , pinned :: [Text]
  }

sample :: Model
sample = Model
  { counts = \page -> case page of
      Inbox -> Just 7
      ReviewRequests -> Just 2
      AssignedToMe -> Just 3
      Mentions -> Just 1
      _ -> Nothing
  , forges = [(GitHub, "github.com"), (Forgejo, "git.azizovich.uz")]
  , pinned = ["abdivasiyev/usta", "abdivasiyev/homelab", "abdivasiyev/nix"]
  }

-- | @onSelect@ runs when a page entry is clicked; that entry becomes the
-- highlighted one.
new :: Model -> Page -> (Page -> IO ()) -> IO Gtk.Box
new model selected onSelect = do
  sidebar <- vbox 20
  Gtk.widgetAddCssClass sidebar "u-sidebar"
  -- Entries expand to push counts right; don't let that widen the sidebar.
  GI.set sidebar [#widthRequest := 240, #hexpand := False]

  let groups = [[Inbox .. Done], [PullRequests .. Repositories]]
  entries <- fmap concat $ forM groups $ \pages -> do
    group <- vbox 2
    #append sidebar group
    forM pages $ \page -> do
      button <- NavItem.new NavItem.defaults
        { NavItem.label = pageTitle page
        , NavItem.count = counts model page
        , NavItem.active = page == selected
        }
      #append group button
      pure (page, button)

  forM_ entries $ \(page, button) ->
    void $ GI.on button #clicked $ do
      forM_ entries $ \(_, other) -> Gtk.widgetRemoveCssClass other "active"
      Gtk.widgetAddCssClass button "active"
      onSelect page

  forgeGroup <- vbox 2
  Card.sectionTitle "Forges" >>= #append forgeGroup
  forM_ (forges model) $ \(kind, host) -> do
    dot <- forgeDot kind
    NavItem.new NavItem.defaults { NavItem.label = host, NavItem.leading = Just dot }
      >>= #append forgeGroup
  NavItem.new NavItem.defaults { NavItem.label = "+ Add forge", NavItem.link = True }
    >>= #append forgeGroup
  #append sidebar forgeGroup

  pinnedGroup <- vbox 2
  Card.sectionTitle "Pinned repositories" >>= #append pinnedGroup
  forM_ (pinned model) $ \repo ->
    NavItem.new NavItem.defaults { NavItem.label = repo, NavItem.mono = True }
      >>= #append pinnedGroup
  #append sidebar pinnedGroup

  pure sidebar

-- | Each forge keeps its own color, independent of status tones.
forgeDot :: ForgeKind -> IO Gtk.Widget
forgeDot kind = do
  dot <- GI.new Gtk.Box [ #cssClasses := ["u-dot", forgeClass kind]
                        , #valign := Gtk.AlignCenter
                        ]
  Gtk.toWidget dot
  where
    forgeClass GitHub = "forge-github"
    forgeClass GitLab = "forge-gitlab"
    forgeClass Forgejo = "forge-forgejo"

vbox :: Int -> IO Gtk.Box
vbox gap = GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := fromIntegral gap]

css :: Text
css = mconcat
  [ "box.u-sidebar {"
  , "  background: var(--sidebar); border-right: 1px solid var(--border); padding: 16px 12px;"
  , "}"
  , "box.u-sidebar label.u-section-title { margin: 0 10px 6px; }"
  , "box.u-dot.forge-github { background: var(--text); }"
  , "box.u-dot.forge-gitlab { background: #6D28D9; }"
  , "box.u-dot.forge-forgejo { background: #C2410C; }"
  , "@media (prefers-color-scheme: dark) {"
  , "  box.u-dot.forge-gitlab { background: #A78BFA; }"
  , "  box.u-dot.forge-forgejo { background: #F08A4B; }"
  , "}"
  ]
