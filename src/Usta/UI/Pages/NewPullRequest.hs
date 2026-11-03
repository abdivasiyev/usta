{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Opening a pull request: branches, title, description, what it will
-- contain, and reviewers and labels.
module Usta.UI.Pages.NewPullRequest
  ( Model (..)
  , Place (..)
  , Actions (..)
  , sample
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.Types.Discussion (NewPull (..))
import Usta.Types.Parse (parse)

-- | A repository a pull request can be opened in.
type Place :: Type
data Place = Place
  { forge :: Forge.Kind
  , host :: Text
  , repo :: Text
  , defaultBranch :: Text
    -- ^ Where it merges into unless chosen otherwise.
  }

type Model :: Type
data Model = Model
  { places :: [Place]
  , chosen :: Int
    -- ^ The one selected at first.
  }

-- | What the form asks of the forge. Each runs off the main thread.
type Actions :: Type
data Actions = Actions
  { branchesOf :: Place -> IO (Either Text [Text])
  , create :: Place -> NewPull -> IO (Either Text Int)
  , opened :: Place -> Int -> Text -> (Text, Text) -> IO ()
    -- ^ With the new pull request's number, title and branches (head
    -- and base).
  , cancel :: IO ()
  }

new :: Actions -> Model -> IO Gtk.Widget
new actions m = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #hexpand := True
                         , #vexpand := True
                         , #cssClasses := ["u-new-pr"]
                         ]
  header <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 12
                           , #cssClasses := ["form-header"]
                           ]
  cancelButton <- Button.new Button.defaults
    { Button.label = Just "Cancel"
    , Button.icon = Just "go-previous-symbolic"
    , Button.variant = Button.Flat
    }
  void $ GI.on cancelButton #clicked actions.cancel
  #append header cancelButton
  label "New pull request" ["form-title"] >>= #append header
  #append page header

  columns <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
  main <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 16
                         , #hexpand := True
                         , #cssClasses := ["form-main"]
                         ]

  -- Repository, then the branches it has, loaded when it is chosen.
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  repo <- Gtk.dropDownNewFromStrings [p.host <> "/" <> p.repo | p <- m.places]
  GI.set repo [#cssClasses := ["mono"]]
  repoField <- Gtk.toWidget repo >>= field "Repository"
  GI.set repoField [#hexpand := True]
  #append row repoField
  intoList <- Gtk.stringListNew (Just [])
  into <- Gtk.dropDownNew (Just intoList) (Nothing :: Maybe Gtk.Expression)
  Gtk.toWidget into >>= field "Into" >>= #append row
  arrow <- label "←" ["muted", "arrow"]
  GI.set arrow [#valign := Gtk.AlignEnd]
  #append row arrow
  fromList <- Gtk.stringListNew (Just [])
  from <- Gtk.dropDownNew (Just fromList) (Nothing :: Maybe Gtk.Expression)
  Gtk.toWidget from >>= field "From" >>= #append row
  #append main row
  status <- label "" ["small", "muted"]
  #append main status

  titleEntry <- GI.new Gtk.Entry [#placeholderText := "What it does, e.g. feat: render the main window", #cssClasses := ["title-entry"]]
  Gtk.toWidget titleEntry >>= field "Title" >>= #append main
  (editorBox, buffer) <- editor ""
  #append main editorBox
  scroll <- GI.new Gtk.ScrolledWindow [ #child := main
                                      , #hexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]
  #append columns scroll

  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 18
                         , #widthRequest := 280
                         , #hexpand := False
                         , #cssClasses := ["form-side"]
                         ]
  reviewersEntry <- GI.new Gtk.Entry [#placeholderText := "User names, separated by commas"]
  Gtk.toWidget reviewersEntry >>= field "Reviewers" >>= #append side
  labelsEntry <- GI.new Gtk.Entry [#placeholderText := "Label names, separated by commas"]
  Gtk.toWidget labelsEntry >>= field "Labels" >>= #append side
  bottom <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 8
                           , #vexpand := True
                           , #valign := Gtk.AlignEnd
                           ]
  problem <- label "" ["small", "problem"]
  GI.set problem [#wrap := True]
  #append bottom problem
  createButton <- Button.new Button.defaults { Button.label = Just "Create pull request", Button.variant = Button.Primary }
  #append bottom createButton
  draftButton <- Button.new Button.defaults { Button.label = Just "Create as draft" }
  #append bottom draftButton
  #append side bottom
  #append columns side
  #append page columns

  let placeAt i = if i >= 0 && i < length m.places then Just (m.places !! i) else Nothing
  names <- newIORef ([] :: [Text])
  let selected list = do
        i <- fromIntegral <$> Gtk.dropDownGetSelected list
        shown <- readIORef names
        pure (if i < length shown then Just (shown !! i) else Nothing)
      fill shown place = do
        writeIORef names shown
        forM_ [intoList, fromList] $ \l -> do
          n <- Gio.listModelGetNItems l
          Gtk.stringListSplice l 0 n (Just shown)
        let at name = length (takeWhile (/= name) shown)
            base = at place.defaultBranch
        Gtk.dropDownSetSelected into (fromIntegral base)
        -- The first other branch, as the one with the changes.
        Gtk.dropDownSetSelected from (fromIntegral (length (takeWhile (== place.defaultBranch) shown)))
      load = do
        index <- fromIntegral <$> Gtk.dropDownGetSelected repo
        forM_ (placeAt index) $ \place -> do
          fill [] place
          GI.set status [#label := "Loading branches\8230"]
          void $ forkIO $ do
            found <- actions.branchesOf place
            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
              case found of
                Right shown -> do
                  fill shown place
                  GI.set status [#label := if length shown < 2 then "The repository has one branch: push another to open a pull request." else ""]
                Left text -> GI.set status [#label := text]
              pure False
      submit draft = do
        index <- fromIntegral <$> Gtk.dropDownGetSelected repo
        base <- selected into
        tip <- selected from
        titleText <- T.strip <$> Gtk.editableGetText titleEntry
        bodyText <- maybe "" id <$> GI.get buffer #text
        reviewersText <- Gtk.editableGetText reviewersEntry
        labelsText <- Gtk.editableGetText labelsEntry
        let reviewers = traverse parse (names' reviewersText)
        case (placeAt index, base, tip, reviewers) of
          _ | T.null titleText -> GI.set problem [#label := "Give the pull request a title."]
          (Just place, Just b, Just t, Right who)
            | b == t -> GI.set problem [#label := "Choose two different branches."]
            | otherwise -> do
                GI.set problem [#label := ""]
                forM_ [createButton, draftButton] $ \x -> GI.set x [#sensitive := False]
                let pull = NewPull
                      { title = titleText
                      , body = bodyText
                      , from = t
                      , into = b
                      , labels = names' labelsText
                      , reviewers = who
                      , draft = draft
                      }
                void $ forkIO $ do
                  result <- actions.create place pull
                  void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    case result of
                      Right number -> actions.opened place number titleText (t, b)
                      Left text -> do
                        GI.set problem [#label := text]
                        forM_ [createButton, draftButton] $ \x -> GI.set x [#sensitive := True]
                    pure False
          (_, _, _, Left _) -> GI.set problem [#label := "A reviewer is not a user name."]
          _ -> GI.set problem [#label := "Choose the repository and both branches."]
  Gtk.dropDownSetSelected repo (fromIntegral m.chosen)
  void $ GI.on repo (GI.PropertyNotify #selected) (const load)
  load
  void $ GI.on createButton #clicked (submit False)
  void $ GI.on draftButton #clicked (submit True)
  Gtk.toWidget page
  where
    names' = filter (not . T.null) . map T.strip . T.splitOn ","

-- | Markdown description with Write and Preview tabs.
editor :: Text -> IO (Gtk.Box, Gtk.TextBuffer)
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
  pure (box, buffer)

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
  { places =
      [ Place Forge.GitHub "github.com" "abdivasiyev/usta" "master"
      , Place Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" "master"
      ]
  , chosen = 0
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
  , "box.u-new-pr label.problem { color: var(--danger); }"
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
