{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The left sidebar: inbox sections, the main pages, forges and pinned
-- repositories.
module Usta.UI.Sidebar
  ( Page (..)
  , pageName
  , pageTitle
  , Model (..)
  , ForgeSync (..)
  , sample
  , new
  , css
  ) where

import Control.Monad (forM, forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.NavItem as NavItem

-- | Every place the sidebar can take you.
type Page :: Type
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

-- | What the sidebar shows. Comes from the cache once sync exists.
type Model :: Type
data Model = Model
  { counts :: Page -> Maybe Int
  , forges :: [(Forge.Kind, Text)]
  , forgeSync :: Text -> Maybe ForgeSync
    -- ^ Per host, while the first sync runs.
  , pinned :: [Text]
  }

type ForgeSync :: Type
data ForgeSync = ForgeSyncing | ForgeDone

sample :: Model
sample = Model
  { counts = \page -> case page of
      Inbox -> Just 7
      ReviewRequests -> Just 2
      AssignedToMe -> Just 3
      Mentions -> Just 1
      _ -> Nothing
  , forges = [(Forge.GitHub, "github.com"), (Forge.Forgejo, "git.azizovich.uz")]
  , forgeSync = const Nothing
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
        , NavItem.count = model.counts page
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
  forM_ model.forges $ \(kind, host) -> do
    let state = model.forgeSync host
    leading <- case state of
      Just ForgeSyncing -> GI.new Gtk.Spinner [#spinning := True, #cssClasses := ["sync-spinner"]]
                             >>= Gtk.toWidget
      _ -> Forge.dot kind
    item <- NavItem.new NavItem.defaults { NavItem.label = host, NavItem.leading = Just leading }
    -- The state goes at the row's end, where counts sit on other entries.
    forM_ state $ \s -> do
      row <- Gtk.buttonGetChild item >>= maybe (pure Nothing) (GI.castTo Gtk.Box)
      let (text, cls) = case s of
            ForgeSyncing -> ("Syncing", "syncing")
            ForgeDone -> ("Done", "done")
      forM_ row $ \r -> GI.new Gtk.Label [#label := text, #cssClasses := ["sync-state", cls]] >>= #append r
    #append forgeGroup item
  NavItem.new NavItem.defaults { NavItem.label = "+ Add forge", NavItem.link = True }
    >>= #append forgeGroup
  #append sidebar forgeGroup

  when (not (null model.pinned)) $ do
    pinnedGroup <- vbox 2
    Card.sectionTitle "Pinned repositories" >>= #append pinnedGroup
    forM_ model.pinned $ \repo ->
      NavItem.new NavItem.defaults { NavItem.label = repo, NavItem.mono = True }
        >>= #append pinnedGroup
    #append sidebar pinnedGroup

  pure sidebar

vbox :: Int -> IO Gtk.Box
vbox gap = GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := fromIntegral gap]

css :: Text
css = mconcat
  [ "box.u-sidebar label.sync-state { font-size: 13px; }"
  , "box.u-sidebar label.sync-state.syncing { color: var(--text-muted); }"
  , "box.u-sidebar label.sync-state.done { color: var(--success); }"
  , "box.u-sidebar spinner.sync-spinner { min-width: 12px; min-height: 12px; color: var(--accent); }"
  , "box.u-sidebar {"
  , "  background: var(--sidebar); border-right: 1px solid var(--border); padding: 16px 12px;"
  , "}"
  , "box.u-sidebar label.u-section-title { margin: 0 10px 6px; }"
  ]
