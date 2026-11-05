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
import Control.Monad (forM, forM_, join, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Graphene as Graphene
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.FileIcon as FileIcon
import qualified Usta.UI.Components.BranchPicker as BranchPicker
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Diff as Diff
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Components.Skeleton as Skeleton
import Usta.UI.Format (clock)
import Usta.Types.Branch (Branch (..))
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
  { branchesOf :: Place -> IO (Either Text [Branch])
  , compareOf :: Place -> Text -> Text -> IO (Either Text ([(Text, Text)], [Diff.File]))
    -- ^ Into and from: the commits (short hash and subject) and the
    -- changes the pull request would have.
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
  -- Each picker is drawn again when a branch is chosen in it.
  intoSlot <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  Gtk.toWidget intoSlot >>= field "Into" >>= #append row
  arrow <- label "←" ["muted", "arrow"]
  GI.set arrow [#valign := Gtk.AlignEnd]
  #append row arrow
  fromSlot <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  Gtk.toWidget fromSlot >>= field "From" >>= #append row
  #append main row
  status <- label "" ["small", "muted"]
  GI.set status [#visible := False]
  #append main status

  titleEntry <- GI.new Gtk.Entry [#placeholderText := "What it does, e.g. feat: render the main window", #cssClasses := ["title-entry"]]
  Gtk.toWidget titleEntry >>= field "Title" >>= #append main
  (editorBox, buffer) <- MarkdownEditor.new MarkdownEditor.defaults {MarkdownEditor.height = 160}
  #append main editorBox
  contentsSlot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append main contentsSlot
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
  -- Enter in a field and Cmd Enter in the description create it.
  let createIt = void (Gtk.widgetActivate createButton)
  mapM_ (\e -> GI.on e #activate createIt) [titleEntry, reviewersEntry, labelsEntry]
  MarkdownEditor.sendOn editorBox False createIt
  draftButton <- Button.new Button.defaults { Button.label = Just "Create as draft" }
  #append bottom draftButton
  #append side bottom
  #append columns side
  #append page columns

  let placeAt i = if i >= 0 && i < length m.places then Just (m.places !! i) else Nothing
  into <- newIORef (Nothing :: Maybe Text)
  from <- newIORef (Nothing :: Maybe Text)
  -- The branches of the repository chosen, fetched once for both pickers.
  known <- newIORef (pure (Left "Loading branches\8230") :: IO (Either Text [Branch]))
  c <- clock
  previewRef <- newIORef (pure ())
  let picker slot ref place = do
        old <- Gtk.widgetGetFirstChild slot
        forM_ old (Gtk.boxRemove slot)
        shown <- readIORef ref
        fetch <- readIORef known
        b <- BranchPicker.new BranchPicker.Picker
          { shown = maybe "Choose a branch" id shown, defaultBranch = place.defaultBranch, clock = c
          , branches = fetch, tags = Nothing
          , choose = \name -> writeIORef ref (Just name) >> picker slot ref place >> join (readIORef previewRef)
          , note = "" }
        #append slot b
      pickers place = picker intoSlot into place >> picker fromSlot from place
      selected = readIORef
      load = do
        index <- fromIntegral <$> Gtk.dropDownGetSelected repo
        forM_ (placeAt index) $ \place -> do
          writeIORef into Nothing >> writeIORef from Nothing
          writeIORef known (pure (Left "Loading branches\8230"))
          pickers place
          say status "Loading branches\8230"
          void $ forkIO $ do
            found <- actions.branchesOf place
            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
              writeIORef known (pure found)
              case found of
                Right shown -> do
                  let others = [b.name | b <- shown, b.name /= place.defaultBranch]
                  writeIORef into (Just place.defaultBranch)
                  -- The first other branch, as the one with the changes.
                  writeIORef from (take 1 others `orFirst` Nothing)
                  pickers place
                  say status (if null others then "The repository has one branch: push another to open a pull request." else "")
                  join (readIORef previewRef)
                Left text -> say status text
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
                let pull = NewPull
                      { title = titleText
                      , body = bodyText
                      , from = t
                      , into = b
                      , labels = names' labelsText
                      , reviewers = who
                      , draft = draft
                      }
                    (pressed, other) = if draft then (draftButton, createButton) else (createButton, draftButton)
                Button.busy pressed [other] "Creating…" (actions.create place pull) $
                  either (\text -> GI.set problem [#label := text]) (\number -> actions.opened place number titleText (t, b))
          (_, _, _, Left _) -> GI.set problem [#label := "A reviewer is not a user name."]
          _ -> GI.set problem [#label := "Choose the repository and both branches."]
  -- Each change of branch asks again; only the latest answer is shown.
  asked <- newIORef (0 :: Int)
  let preview = do
        index <- fromIntegral <$> Gtk.dropDownGetSelected repo
        base <- selected into
        tip <- selected from
        turn <- (+ 1) <$> readIORef asked
        writeIORef asked turn
        let replace widget = do
              old <- Gtk.widgetGetFirstChild contentsSlot
              forM_ old (Gtk.boxRemove contentsSlot)
              forM_ widget (#append contentsSlot)
        case (placeAt index, base, tip) of
          (Just place, Just b, Just t) | b /= t -> do
            Skeleton.block 88 >>= replace . Just
            void $ forkIO $ do
              found <- actions.compareOf place b t
              void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                latest <- readIORef asked
                when (latest == turn) $ case found of
                  Right (commits, changes) -> contents scroll b t commits changes >>= Gtk.toWidget >>= replace . Just
                  Left text -> label text ["small", "muted"] >>= Gtk.toWidget >>= replace . Just
                pure False
          _ -> replace Nothing
  writeIORef previewRef preview
  Gtk.dropDownSetSelected repo (fromIntegral m.chosen)
  void $ GI.on repo (GI.PropertyNotify #selected) (const load)
  load
  void $ GI.on createButton #clicked (submit False)
  void $ GI.on draftButton #clicked (submit True)
  Gtk.toWidget page
  where
    names' = filter (not . T.null) . map T.strip . T.splitOn ","
    orFirst (x : _) _ = Just x
    orFirst [] d = d

-- | What the pull request would contain: its commits, and the files it
-- changes, one a row, with their diffs under them. A row scrolls
-- @scroll@ to its diff.
contents :: Gtk.ScrolledWindow -> Text -> Text -> [(Text, Text)] -> [Diff.File] -> IO Gtk.Box
contents scroll into from commits changes = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12]
  if null commits
    then label (from <> " has no commits that " <> into <> " lacks: there is nothing to merge.") ["small", "muted"]
           >>= #append box
    else do
      (tabs, buttons) <- Segmented.new
        [ "Commits " <> T.pack (show (length commits))
        , "Files changed " <> T.pack (show (length changes)) ] 0
      #append box tabs
      stack <- GI.new Gtk.Stack [#vhomogeneous := False, #interpolateSize := True]
      #append box stack

      commitCard <- Card.new
      forM_ commits $ \(short, subject) -> do
        row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["content-row"]]
        label short ["mono", "hash"] >>= #append row
        s' <- label subject []
        GI.set s' [#ellipsize := Pango.EllipsizeModeEnd]
        #append row s'
        #append commitCard row
      void $ Gtk.stackAddNamed stack commitCard (Just "commits")

      files <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 16]
      list <- Card.new
      #append files list
      diffs <- forM changes $ \f -> do
        d <- Diff.plain f
        #append files d
        pure d
      forM_ (zip changes diffs) $ \(f, d) -> do
        row <- GI.new Gtk.Button [#cssClasses := ["content-row", "file-row"]]
        inner <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
        FileIcon.file 16 f.path >>= #append inner
        name <- label f.path ["mono"]
        GI.set name [#hexpand := True, #ellipsize := Pango.EllipsizeModeMiddle]
        #append inner name
        let (added, removed) = T.breakOn " " (Diff.stat f)
        label added ["mono", "added"] >>= #append inner
        label (T.strip removed) ["mono", "removed"] >>= #append inner
        GI.set row [#child := inner]
        void $ GI.on row #clicked $ do
          (ok, point) <- Gtk.widgetComputePoint d scroll =<< pointAt 0 0
          adjustment <- Gtk.scrolledWindowGetVadjustment scroll
          now <- Gtk.adjustmentGetValue adjustment
          when ok $ Graphene.getPointY point >>= \y -> Gtk.adjustmentSetValue adjustment (now + realToFrac y - 12)
        #append list row
      void $ Gtk.stackAddNamed stack files (Just "files")
      forM_ (zip buttons ["commits", "files"]) $ \(b, name) ->
        void $ GI.on b #toggled $ GI.get b #active >>= \on' -> when on' (Gtk.stackSetVisibleChildName stack name)
  pure box
  where
    pointAt x y = do
      p <- Graphene.newZeroPoint
      Graphene.pointInit p x y

-- | A small caption above an input.
field :: Text -> Gtk.Widget -> IO Gtk.Box
field caption input = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  label caption ["caption"] >>= #append box
  #append box input
  pure box

-- | A note that takes no room when it says nothing.
say :: Gtk.Label -> Text -> IO ()
say l text = GI.set l [#label := text, #visible := not (T.null text)]

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
  , "box.u-new-pr menubutton.u-branch-button > button { min-height: 40px; border-radius: 8px; }"
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
  , "box.u-new-pr box.content-row:last-child, box.u-new-pr button.content-row:last-child { border-bottom: none; }"
  , "box.u-new-pr button.file-row { padding: 8px 12px; border: none; border-radius: 0; border-bottom: 1px solid var(--border-subtle); background: transparent; background-image: none; box-shadow: none; }"
  , "box.u-new-pr button.file-row:hover { background: var(--border-subtle); }"
  ]
