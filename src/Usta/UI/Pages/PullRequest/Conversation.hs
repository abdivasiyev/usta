{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Conversation tab of a pull request: description, timeline,
-- review threads, the merge box and a comment field, with branch,
-- reviewers, labels and linked issues on the side.
module Usta.UI.Pages.PullRequest.Conversation
  ( Event (..)
  , Thread (..)
  , MergeState (..)
  , Model (..)
  , sample
  , new
  , css
  ) where

import Control.Monad (forM_, unless)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Icon as Icon
import Usta.UI.Style (Tone (..))

-- | Something that happened on the pull request. Add a constructor and a
-- case in 'eventRow' for a new kind.
type Event :: Type
data Event
  = Committed Text Text
    -- ^ Short hash and message.
  | CheckFinished Bool Text
    -- ^ Passed or failed, and what ran.
  | ReviewRequestedFrom Text

-- | An inline review comment on one line.
type Thread :: Type
data Thread = Thread
  { location :: Text
  , line :: Text
  , author :: Text
  , body :: Text
  , resolved :: Bool
  }

type MergeState :: Type
data MergeState = MergeState
  { checksPassed :: Bool
  , approvalsMissing :: Int
  }

type Model :: Type
data Model = Model
  { openedBy :: Text
  , openedAgo :: Text
  , description :: Text
  , events :: [Event]
  , threads :: [Thread]
  , merge :: MergeState
  , source :: Text
  , target :: Text
  , reviewers :: [(Tone, Text)]
  , labels :: [(Tone, Text)]
  , linkedIssues :: [Text]
  }

new :: Model -> IO Gtk.Widget
new m = do
  columns <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #cssClasses := ["u-conversation"]
                            ]
  main <- vbox 16
  GI.set main [#hexpand := True, #cssClasses := ["main"]]

  descriptionCard m >>= #append main
  timeline <- vbox 10
  Gtk.widgetAddCssClass timeline "timeline"
  forM_ (events m) $ \e -> eventRow e >>= #append timeline
  #append main timeline
  forM_ (threads m) $ \t -> threadCard t >>= #append main
  mergeBox (merge m) >>= #append main
  commentBox >>= #append main

  -- Keep the reading column a comfortable width on wide windows.
  clamp <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #hexpand := True]
  GI.set main [#widthRequest := 560]
  #append clamp main
  scroll <- GI.new Gtk.ScrolledWindow [ #child := clamp
                                      , #hexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]
  #append columns scroll
  details m >>= #append columns
  Gtk.toWidget columns

descriptionCard :: Model -> IO Gtk.Box
descriptionCard m = do
  card <- Card.new
  top <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 10
                        , #cssClasses := ["card-header"]
                        ]
  Avatar.new 24 "AA" >>= #append top
  label (openedBy m) ["strong"] >>= #append top
  label ("opened this " <> openedAgo m) ["muted"] >>= #append top
  #append card top
  text <- label (description m) ["card-body"]
  GI.set text [#wrap := True]
  #append card text
  pure card

eventRow :: Event -> IO Gtk.Box
eventRow e = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  case e of
    Committed hash message -> do
      label hash ["mono", "hash"] >>= #append row
      label message ["soft"] >>= #append row
    CheckFinished passed what -> do
      Icon.toned 14 (if passed then Success else Danger) (if passed then Icon.Check else Icon.Cross)
        >>= #append row
      label what ["mono", "strong"] >>= #append row
      label (if passed then "passed" else "failed") ["soft"] >>= #append row
    ReviewRequestedFrom who -> do
      Badge.dot Warning >>= #append row
      label "Review requested from" ["soft"] >>= #append row
      label who ["strong"] >>= #append row
  pure row

threadCard :: Thread -> IO Gtk.Box
threadCard t = do
  card <- Card.new
  top <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 8
                        , #cssClasses := ["card-header", "plain"]
                        ]
  label "Comment on" ["strong"] >>= #append top
  where' <- label (location t) ["mono", "link"]
  GI.set where' [#hexpand := True]
  #append top where'
  unless (resolved t) $ Badge.new Warning "Unresolved" >>= #append top
  #append card top
  label ("+  " <> line t) ["mono", "diff-add"] >>= #append card
  comment <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 10
                            , #cssClasses := ["card-body"]
                            ]
  avatar <- Avatar.new 24 "AA"
  GI.set avatar [#valign := Gtk.AlignStart]
  #append comment avatar
  text <- vbox 4
  label (author t) ["strong"] >>= #append text
  bodyLabel <- label (body t) []
  GI.set bodyLabel [#wrap := True]
  #append text bodyLabel
  #append comment text
  #append card comment
  pure card

mergeBox :: MergeState -> IO Gtk.Box
mergeBox s = do
  card <- Card.new
  Gtk.widgetAddCssClass card "merge-box"
  status <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["merge-row"]]
  if checksPassed s
    then do
      Icon.toned 16 Success Icon.Check >>= #append status
      label "All checks have passed" [] >>= #append status
    else do
      Icon.toned 16 Danger Icon.Cross >>= #append status
      label "Some checks failed" [] >>= #append status
  #append card status
  unless (approvalsMissing s == 0) $ do
    waiting <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["merge-row"]]
    Badge.dot Warning >>= #append waiting
    label "Waiting for 1 approving review" [] >>= #append waiting
    #append card waiting
  actions <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["merge-row"]]
  Gtk.dropDownNewFromStrings ["Squash and merge", "Create a merge commit", "Rebase and merge"]
    >>= #append actions
  GI.new Gtk.CheckButton [#label := "Delete branch after merge", #active := True] >>= #append actions
  mergeButton <- Button.new Button.defaults { Button.label = Just "Merge", Button.variant = Button.Primary }
  GI.set mergeButton [ #hexpand := True
                     , #halign := Gtk.AlignEnd
                     , #sensitive := approvalsMissing s == 0 && checksPassed s
                     ]
  #append actions mergeButton
  #append card actions
  pure card

commentBox :: IO Gtk.Box
commentBox = do
  box <- vbox 8
  label "Add a comment" ["strong"] >>= #append box
  editor <- GI.new Gtk.TextView [ #wrapMode := Gtk.WrapModeWordChar
                                , #topMargin := 10
                                , #bottomMargin := 10
                                , #leftMargin := 12
                                , #rightMargin := 12
                                , #heightRequest := 96
                                , #cssClasses := ["editor"]
                                ]
  #append box editor
  buttons <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 8
                            , #halign := Gtk.AlignEnd
                            ]
  Button.new Button.defaults { Button.label = Just "Close pull request" } >>= #append buttons
  Button.new Button.defaults { Button.label = Just "Comment", Button.variant = Button.Primary }
    >>= #append buttons
  #append box buttons
  pure box

details :: Model -> IO Gtk.Box
details m = do
  side <- vbox 20
  GI.set side [#widthRequest := 260, #hexpand := False, #cssClasses := ["details"]]

  section side "Branch" $ \box -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    label (source m) ["chip", "mono"] >>= #append row
    label "→" ["muted"] >>= #append row
    label (target m) ["chip", "mono"] >>= #append row
    #append box row
  section side "Reviewers" $ \box ->
    forM_ (reviewers m) $ \(tone, name) -> do
      row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
      Badge.dot tone >>= #append row
      label name [] >>= #append row
      #append box row
  section side "Labels" $ \box -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    forM_ (labels m) $ \(tone, name) -> Badge.new tone name >>= #append row
    #append box row
  section side "Linked issues" $ \box ->
    forM_ (linkedIssues m) $ \issue -> label issue ["link"] >>= #append box
  pure side
  where
    section :: Gtk.Box -> Text -> (Gtk.Box -> IO ()) -> IO ()
    section side title fill = do
      box <- vbox 6
      Card.sectionTitle title >>= #append box
      fill box
      #append side box

vbox :: Int -> IO Gtk.Box
vbox gap = GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := fromIntegral gap]

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: Model
sample = Model
  { openedBy = "abdivasiyev"
  , openedAgo = "5 min ago"
  , description = "Adds the GTK4 bindings so the next PR can open the main window.\n\n• gi-gtk4 in usta.cabal\n• Same dependency in the flake derivation"
  , events =
      [ Committed "e1c90fa" "feat: add gi-gtk4 dependency"
      , CheckFinished True "nix build"
      , ReviewRequestedFrom "abdivasiyev"
      ]
  , threads =
      [ Thread "flake.nix:23" "executableHaskellDepends = with hsPkgs; [ base gi-gtk4 ];" "abdivasiyev"
          "Add wrapGAppsHook4 here once Main opens a window, otherwise the app can't find the GSettings schemas."
          False
      ]
  , merge = MergeState { checksPassed = True, approvalsMissing = 1 }
  , source = "feat/gtk4-dep"
  , target = "master"
  , reviewers = [(Warning, "abdivasiyev")]
  , labels = [(Accent, "gtk"), (Neutral, "build")]
  , linkedIssues = ["#5 Render the main window with GTK4"]
  }

css :: Text
css = mconcat
  [ "box.u-conversation box.main { padding: 20px 32px; }"
  , "box.u-conversation box.details { padding: 20px 16px; }"
  , "box.u-conversation label.strong { font-weight: 600; }"
  , "box.u-conversation label.link { color: var(--accent); }"
  , "box.u-conversation label.hash { color: var(--accent); }"
  , "box.u-conversation box.card-header {"
  , "  padding: 10px 14px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: var(--window); border-radius: 8px 8px 0 0; font-size: 13px;"
  , "}"
  , "box.u-conversation box.card-header.plain { background: transparent; }"
  , "box.u-conversation .card-body { padding: 14px 16px; }"
  , "box.u-conversation box.timeline {"
  , "  margin-left: 12px; padding-left: 14px; border-left: 2px solid var(--border); font-size: 13px;"
  , "}"
  , "box.u-conversation label.diff-add {"
  , "  padding: 6px 14px; background: var(--success-bg); color: var(--success);"
  , "}"
  , "box.u-conversation box.merge-row { padding: 12px 16px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-conversation box.merge-row:last-child { border-bottom: none; }"
  , "box.u-conversation dropdown > button {"
  , "  min-height: 36px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
  , "}"
  , "box.u-conversation textview.editor {"
  , "  border: 1px solid var(--input-border); border-radius: 8px;"
  , "}"
  , "box.u-conversation textview.editor text { background: var(--surface); color: var(--text); }"
  , "box.u-conversation label.chip {"
  , "  background: var(--neutral-bg); color: var(--text); padding: 1px 6px; border-radius: 4px;"
  , "  font-size: 12px;"
  , "}"
  ]
