{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Opening a pull request: branches, title, description, what it will
-- contain, and reviewers and labels.
module Usta.UI.Pages.NewPullRequest
  ( Model (..)
  , sample
  , new
  , css
  ) where

import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.UI.Style (Tone (..))

data Model = Model
  { repositories :: [Text]
  , targets :: [Text]
    -- ^ Branches to merge into; the first is preselected.
  , sources :: [Text]
    -- ^ Local branches to open the pull request from.
  , canMerge :: Bool
  , title :: Text
  , description :: Text
  , commits :: [(Text, Text)]
    -- ^ Short hash and subject.
  , changedFiles :: [Text]
  , added :: Int
  , removed :: Int
  , labels :: Text
  }

-- | @onCancel@ leaves the form.
new :: IO () -> Model -> IO Gtk.Widget
new onCancel m = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #hexpand := True
                         , #vexpand := True
                         , #cssClasses := ["u-new-pr"]
                         ]
  header <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 12
                           , #cssClasses := ["form-header"]
                           ]
  cancel <- Button.new Button.defaults
    { Button.label = Just "Cancel"
    , Button.icon = Just "go-previous-symbolic"
    , Button.variant = Button.Flat
    }
  void $ GI.on cancel #clicked onCancel
  #append header cancel
  label "New pull request" ["form-title"] >>= #append header
  #append page header

  columns <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
  main <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 16
                         , #hexpand := True
                         , #cssClasses := ["form-main"]
                         ]
  branches m >>= #append main
  mergeStatus m >>= #append main
  titleEntry <- GI.new Gtk.Entry [#text := title m, #cssClasses := ["title-entry"]]
  Gtk.toWidget titleEntry >>= field "Title" >>= #append main
  editor (description m) >>= #append main
  contents m >>= #append main
  scroll <- GI.new Gtk.ScrolledWindow [ #child := main
                                      , #hexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]
  #append columns scroll
  options m >>= #append columns
  #append page columns
  Gtk.toWidget page

branches :: Model -> IO Gtk.Box
branches m = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  repo <- Gtk.dropDownNewFromStrings (repositories m)
  GI.set repo [#cssClasses := ["mono"]]
  repoField <- Gtk.toWidget repo >>= field "Repository"
  GI.set repoField [#hexpand := True]
  #append row repoField
  into <- Gtk.dropDownNewFromStrings (targets m)
  Gtk.toWidget into >>= field "Into" >>= #append row
  arrow <- label "←" ["muted", "arrow"]
  GI.set arrow [#valign := Gtk.AlignEnd]
  #append row arrow
  from <- Gtk.dropDownNewFromStrings (sources m)
  Gtk.toWidget from >>= field "From" >>= #append row
  pure row

mergeStatus :: Model -> IO Gtk.Box
mergeStatus m = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  if canMerge m
    then do
      Icon.toned 14 Success Icon.Check >>= #append row
      label "Can merge cleanly · branch pushed to origin" ["small", "ok"] >>= #append row
    else do
      Icon.toned 14 Danger Icon.Cross >>= #append row
      label "Conflicts with the target branch" ["small", "conflict"] >>= #append row
  pure row

-- | Markdown description with Write and Preview tabs.
editor :: Text -> IO Gtk.Box
editor text = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  (tabs, buttons) <- Segmented.new ["Write", "Preview"] 0
  Gtk.widgetAddCssClass tabs "editor-tabs"
  #append box tabs

  buffer <- GI.new Gtk.TextBuffer []
  Gtk.textBufferSetText buffer text (-1)
  view <- GI.new Gtk.TextView [ #buffer := buffer
                              , #wrapMode := Gtk.WrapModeWordChar
                              , #heightRequest := 160
                              , #topMargin := 12
                              , #bottomMargin := 12
                              , #leftMargin := 12
                              , #rightMargin := 12
                              , #monospace := True
                              , #cssClasses := ["editor"]
                              ]
  rendered <- label "" ["preview"]
  GI.set rendered [#wrap := True, #yalign := 0]
  stack <- GI.new Gtk.Stack []
  void $ Gtk.stackAddNamed stack view (Just "write")
  void $ Gtk.stackAddNamed stack rendered (Just "preview")
  #append box stack

  -- Preview shows the text as it will read; Markdown rendering comes later.
  forM_ (zip ["write", "preview"] buttons) $ \(name, button) ->
    void $ GI.on button #toggled $ do
      active <- GI.get button #active
      when active $ do
        when (name == "preview") $ do
          start <- Gtk.textBufferGetStartIter buffer
          end <- Gtk.textBufferGetEndIter buffer
          current <- Gtk.textBufferGetText buffer start end False
          GI.set rendered [#label := current]
        Gtk.stackSetVisibleChildName stack name
  pure box

contents :: Model -> IO Gtk.Box
contents m = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
  let summary = count (length (commits m)) "commit" <> " · " <> count (length (changedFiles m)) "file"
                <> " changed"
  Card.sectionTitle summary >>= #append box
  card <- Card.new
  forM_ (commits m) $ \(short, subject) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["content-row"]]
    label short ["mono", "hash"] >>= #append row
    label subject [] >>= #append row
    #append card row
  files <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["content-row"]]
  names <- label (T.intercalate " · " (changedFiles m)) ["mono"]
  GI.set names [#hexpand := True]
  #append files names
  label ("+" <> T.pack (show (added m))) ["mono", "added"] >>= #append files
  label ("−" <> T.pack (show (removed m))) ["mono", "removed"] >>= #append files
  #append card files
  #append box card
  pure box
  where
    count n noun = T.pack (show n) <> " " <> noun <> (if n == 1 then "" else "s")

options :: Model -> IO Gtk.Box
options m = do
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 18
                         , #widthRequest := 280
                         , #hexpand := False
                         , #cssClasses := ["form-side"]
                         ]
  reviewersEntry <- GI.new Gtk.Entry [#placeholderText := "Add people"]
  Gtk.toWidget reviewersEntry >>= field "Reviewers" >>= #append side
  labelsEntry <- GI.new Gtk.Entry [#text := labels m]
  Gtk.toWidget labelsEntry >>= field "Labels" >>= #append side
  GI.new Gtk.CheckButton [#label := "Allow edits from maintainers"] >>= #append side
  bottom <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 8
                           , #vexpand := True
                           , #valign := Gtk.AlignEnd
                           ]
  Button.new Button.defaults { Button.label = Just "Create pull request", Button.variant = Button.Primary }
    >>= #append bottom
  Button.new Button.defaults { Button.label = Just "Create as draft" } >>= #append bottom
  #append side bottom
  pure side

-- | A small caption above an input.
field :: Text -> Gtk.Widget -> IO Gtk.Box
field caption input = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  label caption ["caption"] >>= #append box
  #append box input
  pure box

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: Model
sample = Model
  { repositories = ["github.com/abdivasiyev/usta", "git.azizovich.uz/abdivasiyev/homelab"]
  , targets = ["master"]
  , sources = ["feat/main-window", "feat/gtk4-dep"]
  , canMerge = True
  , title = "feat: render main window with GTK4"
  , description = "Opens an ApplicationWindow instead of printing Hello world.\n\nCloses #5"
  , commits = [("7b2e1a4", "feat: open ApplicationWindow on activate"), ("c09d3f8", "build: add wrapGAppsHook4")]
  , changedFiles = ["app/Main.hs", "flake.nix"]
  , added = 24
  , removed = 2
  , labels = "gtk"
  }

css :: Text
css = mconcat
  [ "box.u-new-pr box.form-header {"
  , "  padding: 8px 16px; background: var(--surface); border-bottom: 1px solid var(--border);"
  , "}"
  , "box.u-new-pr label.form-title { font-size: 16px; font-weight: 600; }"
  , "box.u-new-pr box.form-main { padding: 20px 32px; }"
  , "box.u-new-pr box.form-side { padding: 20px 16px; }"
  , "box.u-new-pr label.caption { font-size: 13px; color: var(--text-soft); }"
  , "box.u-new-pr label.small { font-size: 13px; }"
  , "box.u-new-pr label.ok { color: var(--success); }"
  , "box.u-new-pr label.conflict { color: var(--danger); }"
  , "box.u-new-pr label.arrow { margin-bottom: 10px; }"
  , "box.u-new-pr label.hash { color: var(--accent); }"
  , "box.u-new-pr label.added { color: var(--success); }"
  , "box.u-new-pr label.removed { color: var(--danger); }"
  , "box.u-new-pr entry {"
  , "  min-height: 38px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none;"
  , "}"
  , "box.u-new-pr entry.title-entry { min-height: 42px; font-size: 15px; }"
  , "box.u-new-pr dropdown > button {"
  , "  min-height: 40px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
  , "}"
  , "box.u-new-pr textview.editor {"
  , "  border: 1px solid var(--input-border); border-radius: 0 8px 8px 8px;"
  , "  font-family: 'JetBrains Mono', monospace; font-size: 13px;"
  , "}"
  , "box.u-new-pr textview.editor text { background: var(--surface); color: var(--text); }"
  , "box.u-new-pr label.preview {"
  , "  min-height: 160px; padding: 12px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface);"
  , "}"
  , "box.u-new-pr box.content-row { padding: 10px 12px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-new-pr box.content-row:last-child { border-bottom: none; }"
  ]
