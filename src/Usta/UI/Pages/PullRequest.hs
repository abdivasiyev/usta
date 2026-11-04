{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | One pull request: header with a way back, and tabs for the
-- conversation, changed files, commits and checks.
module Usta.UI.Pages.PullRequest
  ( State (..)
  , Summary (..)
  , Tab (..)
  , new
  , placeholder
  , css
  ) where

import Control.Monad (forM, forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Pages.Issue as Issue
import Usta.UI.Style (Tone (..), toneClass)

type State :: Type
data State = Open | Draft | MergedState | Closed
  deriving (Eq, Show, Enum, Bounded)

stateBadge :: State -> (Tone, Text)
stateBadge s = case s of
  Open -> (Success, "Open")
  Draft -> (Neutral, "Draft")
  MergedState -> (Merged, "Merged")
  Closed -> (Danger, "Closed")

-- | What the header and tab bar need; the tabs load their own content.
type Summary :: Type
data Summary = Summary
  { forge :: Forge.Kind
  , host :: Text
  , repo :: Text
  , number :: Int
  , title :: Text
  , state :: State
  , comments :: Int
  , files :: Int
  , commits :: Int
  , checks :: Maybe (Tone, Text)
    -- ^ Shown next to the Checks tab, e.g. @(Success, "1/1")@.
  , branches :: (Text, Text)
    -- ^ The branch with the changes, and the one it would merge into.
  }

type Tab :: Type
data Tab = Conversation | Files | Commits | Checks
  deriving (Eq, Show, Enum, Bounded)

tabName :: Tab -> Text
tabName = T.toLower . T.pack . show

-- | @backLabel@ names where Back goes, e.g. "Pull requests" or "Inbox";
-- @tabContent@ builds each tab's screen, given what sets the header's
-- state badge, for when the forge says otherwise than the list did.
new :: Text -> IO () -> (Text -> IO (Either Text ())) -> ((State -> IO ()) -> Tab -> IO Gtk.Widget) -> Summary -> IO Gtk.Box
new backLabel onBack rename tabContent summary = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #hexpand := True
                         , #vexpand := True
                         , #cssClasses := ["u-pull-request"]
                         ]
  (bar, showState) <- header summary backLabel onBack rename
  #append page bar

  content <- GI.new Gtk.Stack [ #vexpand := True
                              , #hexpand := True
                              , #transitionType := Gtk.StackTransitionTypeCrossfade
                              , #transitionDuration := 150
                              ]
  forM_ [minBound .. maxBound] $ \tab -> do
    widget <- tabContent showState tab
    void $ Gtk.stackAddNamed content widget (Just (tabName tab))

  tabBar summary (Gtk.stackSetVisibleChildName content . tabName) >>= #append page
  #append page content
  pure page

-- | A stand-in for a tab whose screen does not exist yet.
placeholder :: Tab -> IO Gtk.Widget
placeholder tab =
  GI.new Gtk.Label [#label := T.pack (show tab), #cssClasses := ["muted"]] >>= Gtk.toWidget

header :: Summary -> Text -> IO () -> (Text -> IO (Either Text ())) -> IO (Gtk.Box, State -> IO ())
header s backLabel onBack rename = do
  bar <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 12
                        , #cssClasses := ["pr-header"]
                        ]
  back <- Button.new Button.defaults
    { Button.label = Just backLabel
    , Button.icon = Just "go-previous-symbolic"
    , Button.variant = Button.Flat
    }
  void $ GI.on back #clicked onBack
  #append bar back

  titles <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 2
                           , #hexpand := True
                           , #valign := Gtk.AlignCenter
                           ]
  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Forge.smallDot s.forge >>= #append meta
  label (s.host <> "/" <> s.repo) ["meta", "mono"] >>= #append meta
  label ("#" <> T.pack (show s.number)) ["meta"] >>= #append meta
  badge <- GI.new Gtk.Box []
  let showState state = do
        old <- Gtk.widgetGetFirstChild badge
        forM_ old (Gtk.boxRemove badge)
        let (tone, text) = stateBadge state
        Badge.new tone text >>= #append badge
  showState s.state
  #append meta badge
  #append titles meta
  Issue.editableTitle s.title ["pr-title"] rename >>= #append titles
  #append bar titles

  Button.new Button.defaults { Button.label = Just "Check out locally" } >>= #append bar
  browser <- Button.new Button.defaults { Button.label = Just "Open in browser" }
  void $ GI.on browser #clicked $ do
    launcher <- Gtk.uriLauncherNew (Just url)
    Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
  #append bar browser
  pure (bar, showState)
  where
    path = case s.forge of
      Forge.GitHub -> "/pull/"
      Forge.GitLab -> "/-/merge_requests/"
      Forge.Forgejo -> "/pulls/"
    url = "https://" <> s.host <> "/" <> s.repo <> path <> T.pack (show s.number)

-- | Underlined tabs with counts; calls @onSelect@ with the chosen tab.
tabBar :: Summary -> (Tab -> IO ()) -> IO Gtk.Box
tabBar s onSelect = do
  bar <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 4
                        , #cssClasses := ["pr-tabs"]
                        ]
  buttons <- forM [minBound .. maxBound] $ \tab -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    label (T.pack (show tab)) [] >>= #append row
    case (tab, s.checks) of
      (Checks, Just (tone, text)) -> label text ["tab-count", toneClass tone] >>= #append row
      (Checks, Nothing) -> pure ()
      -- A pull request has at least one file and commit, so 0 is not
      -- known yet: no count is better than a wrong one.
      (_, _) | tab `elem` [Files, Commits] && count tab == 0 -> pure ()
      _ -> label (T.pack (show (count tab))) ["tab-count"] >>= #append row
    button <- GI.new Gtk.ToggleButton [#child := row]
    #append bar button
    pure (tab, button)
  case buttons of
    (_, first) : rest -> do
      forM_ rest $ \(_, b) -> GI.set b [#group := first]
      GI.set first [#active := True]
    [] -> pure ()
  forM_ buttons $ \(tab, button) ->
    void $ GI.on button #toggled $ do
      active <- GI.get button #active
      when active (onSelect tab)
  pure bar
  where
    count tab = case tab of
      Conversation -> s.comments
      Files -> s.files
      Commits -> s.commits
      Checks -> 0

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-pull-request box.pr-header {"
  , "  padding: 8px 16px; background: var(--surface); border-bottom: 1px solid var(--border);"
  , "}"
  , "box.u-pull-request label.meta { font-size: 12px; color: var(--text-muted); }"
  , "box.u-pull-request label.pr-title { font-size: 16px; font-weight: 600; }"
  , "box.u-pull-request box.pr-tabs {"
  , "  padding: 0 16px; background: var(--surface); border-bottom: 1px solid var(--border);"
  , "}"
  , "box.pr-tabs button {"
  , "  min-height: 44px; padding: 0 12px; border: none; border-radius: 0; background: transparent;"
  , "  box-shadow: none; background-image: none; color: var(--text-soft);"
  , "}"
  , "box.pr-tabs button:hover { color: var(--text); }"
  , "box.pr-tabs button:checked {"
  , "  color: var(--text); font-weight: 600; box-shadow: inset 0 -2px 0 var(--accent);"
  , "}"
  , "box.pr-tabs label.tab-count { font-size: 12px; font-weight: 400; color: var(--text-muted); }"
  , "box.pr-tabs label.tab-count.tone-success { color: var(--success); }"
  , "box.pr-tabs label.tab-count.tone-danger { color: var(--danger); }"
  , "box.pr-tabs label.tab-count.tone-warning { color: var(--warning); }"
  ]
