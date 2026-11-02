{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Files tab of a pull request: changed files on the left, their
-- diffs with review threads in the middle, and your review on the right.
module Usta.UI.Pages.PullRequest.Files
  ( Comment (..)
  , Model (..)
  , sample
  , new
  , Fetching (..)
  , fetchingSample
  , fetching
  , css
  ) where

import Control.Monad (filterM, forM, forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Diff as Diff
import Usta.UI.Components.Diff (Line (..))
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Progress as Progress
import qualified Usta.UI.Components.Skeleton as Skeleton
import Usta.UI.Style (Tone (..))

-- | A review comment attached to a line of a file's diff.
data Comment = Comment
  { onLine :: Int
    -- ^ Index into the file's diff lines.
  , author :: Text
  , pending :: Bool
  , body :: Text
  }

data Model = Model
  { files :: [(Diff.File, [Comment])]
  , range :: Text
    -- ^ The commits the diff was computed between, e.g. "8427b36..e1c90fa".
  , checks :: [(Bool, Text, Text)]
    -- ^ Passed, name, duration.
  , reviewers :: [(Tone, Text, Text)]
    -- ^ Status, name, state.
  , approvalsNeeded :: Int
  }

-- | One changed file as shown in both columns.
data Entry = Entry
  { file :: Diff.File
  , row :: Gtk.Button
  , mark :: Gtk.Box
  , card :: Gtk.Box
  }

-- | Space around and between the diffs; matches the CSS below.
padding, gap :: Double
padding = 16
gap = 16

new :: Model -> IO Gtk.Widget
new m = do
  columns <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #cssClasses := ["u-files"]
                            ]
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 6
                         , #widthRequest := 240
                         , #hexpand := False
                         , #cssClasses := ["file-list"]
                         ]
  filterEntry <- GI.new Gtk.SearchEntry [#placeholderText := "Filter files"]
  #append side filterEntry
  #append columns side

  main <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := round gap
                         , #cssClasses := ["diffs"]
                         ]
  toolbar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  label "Diff computed locally" ["muted", "small"] >>= #append toolbar
  label (range m) ["muted", "small", "mono"] >>= #append toolbar
  #append main toolbar
  scroll <- GI.new Gtk.ScrolledWindow [ #child := main
                                      , #hexpand := True
                                      , #vexpand := True
                                      ]
  adjustment <- Gtk.scrolledWindowGetVadjustment scroll

  progress <- label "" ["muted", "small"]
  GI.set progress [#vexpand := True, #valign := Gtk.AlignEnd]

  entriesRef <- newIORef []
  let updateProgress = do
        entries <- readIORef entriesRef
        seen <- length . filter id <$> mapM (\e -> Gtk.widgetHasCssClass (mark e) "done") entries
        GI.set progress [ #label := T.pack (show seen) <> " of " <> T.pack (show (length entries))
                                    <> " files viewed" ]

  entries <- forM (files m) $ \(f, comments) -> do
    attached <- forM comments $ \c -> do
      w <- thread c >>= Gtk.toWidget
      pure (onLine c, w)
    markBox <- viewedMark (Diff.viewed f)
    diff <- Diff.new f attached $ \isViewed -> do
      setViewed markBox isViewed
      updateProgress
    #append main diff
    button <- fileRow f markBox
    #append side button
    pure (Entry f button markBox diff)
  writeIORef entriesRef entries
  #append side progress
  updateProgress

  let setCurrent current =
        forM_ entries $ \e ->
          if Diff.path (file e) == Diff.path (file current)
            then Gtk.widgetAddCssClass (row e) "current"
            else Gtk.widgetRemoveCssClass (row e) "current"
      -- Where each shown diff starts inside the scrolled column.
      offsets = do
        toolbarHeight <- fromIntegral <$> Gtk.widgetGetHeight toolbar
        shown <- filterM (Gtk.widgetGetVisible . card) entries
        heights <- mapM (fmap fromIntegral . Gtk.widgetGetHeight . card) shown
        let starts = scanl (\y h -> y + h + gap) (padding + toolbarHeight + gap) heights
        pure (zip shown starts)

  forM_ entries $ \e ->
    void $ GI.on (row e) #clicked $ do
      positions <- offsets
      forM_ [y | (other, y) <- positions, Diff.path (file other) == Diff.path (file e)] $ \y ->
        Gtk.adjustmentSetValue adjustment (y - padding)
      setCurrent e

  -- The highlight follows whichever diff is at the top of the view.
  void $ GI.on adjustment #valueChanged $ do
    top <- Gtk.adjustmentGetValue adjustment
    positions <- offsets
    case [e | (e, y) <- positions, y - padding <= top + 1] of
      [] -> forM_ (take 1 positions) (setCurrent . fst)
      passed -> setCurrent (last passed)

  void $ GI.on filterEntry #searchChanged $ do
    query <- T.toLower <$> Gtk.editableGetText filterEntry
    forM_ entries $ \e -> do
      let keep = query `T.isInfixOf` T.toLower (Diff.path (file e))
      GI.set (row e) [#visible := keep]
      GI.set (card e) [#visible := keep]

  forM_ (take 1 entries) setCurrent
  #append columns scroll
  review m >>= #append columns
  Gtk.toWidget columns

-- | The square in the file list: empty, or checked once viewed.
viewedMark :: Bool -> IO Gtk.Box
viewedMark isViewed = do
  box <- GI.new Gtk.Box [#cssClasses := ["viewed-mark"], #valign := Gtk.AlignCenter]
  check <- Icon.toned 10 Success Icon.Check
  #append box check
  setViewed box isViewed
  pure box

setViewed :: Gtk.Box -> Bool -> IO ()
setViewed box isViewed = do
  if isViewed then Gtk.widgetAddCssClass box "done" else Gtk.widgetRemoveCssClass box "done"
  child <- Gtk.widgetGetFirstChild box
  forM_ child $ \c -> GI.set c [#visible := isViewed]

fileRow :: Diff.File -> Gtk.Box -> IO Gtk.Button
fileRow f markBox = do
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  #append content markBox
  name <- label (Diff.path f) ["mono", "small"]
  GI.set name [#hexpand := True]
  #append content name
  label ("+" <> count isAdded) ["mono", "small", "added"] >>= #append content
  label ("−" <> count isRemoved) ["mono", "small", "removed"] >>= #append content
  GI.new Gtk.Button [#child := content, #hasFrame := False, #cssClasses := ["file-row"]]
  where
    count p = T.pack (show (length (filter p (Diff.diffLines f))))
    isAdded (Added _ _) = True
    isAdded _ = False
    isRemoved (Removed _ _) = True
    isRemoved _ = False

thread :: Comment -> IO Gtk.Box
thread c = do
  card <- Card.new
  Gtk.widgetAddCssClass card "thread"
  top <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 10
                        , #cssClasses := ["thread-body"]
                        ]
  avatar <- Avatar.new 28 "AA"
  GI.set avatar [#valign := Gtk.AlignStart]
  #append top avatar
  text <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
  who <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  label (author c) ["strong"] >>= #append who
  if pending c then label "· pending" ["muted"] >>= #append who else pure ()
  #append text who
  bodyLabel <- label (body c) []
  GI.set bodyLabel [#wrap := True]
  #append text bodyLabel
  #append top text
  #append card top

  reply <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 8
                          , #cssClasses := ["thread-reply"]
                          ]
  entry <- GI.new Gtk.Entry [#placeholderText := "Reply…", #hexpand := True]
  #append reply entry
  Button.new Button.defaults { Button.label = Just "Resolve" } >>= #append reply
  #append card reply
  pure card

review :: Model -> IO Gtk.Box
review m = do
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 20
                         , #widthRequest := 280
                         , #hexpand := False
                         , #cssClasses := ["review"]
                         ]

  verdict <- section "Your review"
  radios <- forM ["Comment", "Approve", "Request changes"] $ \text ->
    GI.new Gtk.CheckButton [#label := text]
  case radios of
    first : rest -> do
      forM_ rest $ \r -> GI.set r [#group := first]
      GI.set first [#active := True]
    [] -> pure ()
  forM_ radios (#append verdict)
  label "Summary" ["soft", "small"] >>= #append verdict
  GI.new Gtk.TextView [ #wrapMode := Gtk.WrapModeWordChar
                      , #heightRequest := 72
                      , #topMargin := 8
                      , #bottomMargin := 8
                      , #leftMargin := 10
                      , #rightMargin := 10
                      , #cssClasses := ["editor"]
                      ] >>= #append verdict
  let pendingCount = length [() | (_, cs) <- files m, c <- cs, pending c]
  label (T.pack (show pendingCount) <> " pending comment" <> (if pendingCount == 1 then "" else "s"))
    ["muted", "small"] >>= #append verdict
  #append side verdict

  checksBox <- section "Checks"
  forM_ (checks m) $ \(passed, name, took) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    Icon.toned 14 (if passed then Success else Danger) (if passed then Icon.Check else Icon.Cross)
      >>= #append row
    nameLabel <- label name ["mono", "small"]
    GI.set nameLabel [#hexpand := True]
    #append row nameLabel
    label took ["muted", "small"] >>= #append row
    #append checksBox row
  #append side checksBox

  reviewersBox <- section "Reviewers"
  forM_ (reviewers m) $ \(tone, name, state) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    Badge.dot tone >>= #append row
    label name ["small"] >>= #append row
    label ("· " <> state) ["muted", "small"] >>= #append row
    #append reviewersBox row
  #append side reviewersBox

  bottom <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 8
                           , #vexpand := True
                           , #valign := Gtk.AlignEnd
                           ]
  mergeButton <- Button.new Button.defaults
    { Button.label = Just "Squash and merge", Button.variant = Button.Primary }
  GI.set mergeButton [#sensitive := approvalsNeeded m == 0]
  #append bottom mergeButton
  void $ if approvalsNeeded m == 0
    then pure ()
    else do
      needs <- label ("Needs " <> T.pack (show (approvalsNeeded m)) <> " approval") ["muted", "small"]
      GI.set needs [#xalign := 0.5]
      #append bottom needs
  #append side bottom
  pure side
  where
    section title = do
      box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
      Card.sectionTitle title >>= #append box
      pure box

-- | A pull request whose branch the local mirror is still fetching. The
-- file list comes from the forge API; diffs need the mirror.
data Fetching = Fetching
  { branchName :: Text
  , percent :: Double
  , fileCount :: Int
  , ready :: [Diff.File]
    -- ^ Already rendered, e.g. small files the API returned in full.
  , waiting :: [Text]
  }

fetchingSample :: Double -> Fetching
fetchingSample p = Fetching
  { branchName = "refactor/den-aspects"
  , percent = p
  , fileCount = 212
  , ready =
      [ Diff.File "flake.nix" False
          [ Hunk "@@ -1,14 +1,8 @@"
          , Removed 1 "  imports = [ ./hosts/mini ./hosts/maxi ./hosts/midi ];"
          , Added 1 "  imports = [ (inputs.import-tree ./modules) ];"
          ]
      ]
  , waiting =
      [ "modules/dendritic.nix", "modules/hosts/mini.nix", "modules/hosts/maxi.nix"
      , "modules/hosts/midi/_options.nix", "modules/aspects/developer.nix"
      ]
  }

-- | The Files tab while the branch is fetched: progress on top, the files
-- known so far on the left, ready diffs and placeholders for the rest.
-- @onCancel@ runs for Cancel.
fetching :: IO () -> Fetching -> IO Gtk.Widget
fetching onCancel f = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #cssClasses := ["u-files", "fetching"]
                         ]

  banner <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 12
                           , #cssClasses := ["fetch-banner"]
                           ]
  GI.new Gtk.Spinner [#spinning := True] >>= #append banner
  Progress.caption [Progress.Plain "Fetching ", Progress.Code (branchName f), Progress.Plain " into the local mirror"]
    >>= #append banner
  progress <- Progress.bar (Progress.Percent (percent f))
  GI.set progress [#hexpand := False, #widthRequest := 320, #valign := Gtk.AlignCenter]
  #append banner progress
  label (T.pack (show (round (percent f) :: Int)) <> "%") ["mono", "percent"] >>= #append banner
  spacer <- GI.new Gtk.Box [#hexpand := True]
  #append banner spacer
  cancel <- Button.new Button.defaults { Button.label = Just "Cancel" }
  void $ GI.on cancel #clicked onCancel
  #append banner cancel
  #append page banner

  columns <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 6
                         , #widthRequest := 240
                         , #hexpand := False
                         , #cssClasses := ["file-list"]
                         ]
  label (T.pack (show (fileCount f)) <> " files · list from the forge API") ["small", "muted", "list-note"]
    >>= #append side
  let entry path state current = do
        row <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                              , #spacing := 8
                              , #cssClasses := ["fetch-row"] <> ["current" | current]
                              ]
        name <- label path ["mono", "small"]
        -- Capped so a long path ellipsizes instead of widening the list.
        GI.set name [#hexpand := True, #ellipsize := Pango.EllipsizeModeEnd, #maxWidthChars := 22]
        #append row name
        label state ["small", T.toLower state] >>= #append row
        #append side row
  forM_ (zip [0 :: Int ..] (ready f)) $ \(i, file) -> entry (Diff.path file) "Ready" (i == 0)
  forM_ (waiting f) $ \path -> entry path "Waiting" False
  forM_ [190, 150, 170, 130, 160] $ \w -> do
    placeholder <- Skeleton.bar w 10
    GI.set placeholder [#marginStart := 8, #marginTop := 14]
    #append side placeholder
  #append columns side

  diffs <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                          , #spacing := round gap
                          , #cssClasses := ["diffs"]
                          ]
  forM_ (ready f) $ \file -> Diff.plain file >>= #append diffs
  note <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  GI.new Gtk.Spinner [#spinning := True] >>= #append note
  label "The rest renders as soon as the fetch finishes. Files you scroll to are rendered first." []
    >>= #append note
  #append diffs note
  forM_ [[610, 470, 690, 345, 570, 435], [540, 650, 395, 510]] $ \widths ->
    diffPlaceholder widths >>= #append diffs
  scroll <- GI.new Gtk.ScrolledWindow [ #child := diffs
                                      , #hexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]
  #append columns scroll
  #append page columns
  Gtk.toWidget page

-- | A diff card's shape: header, then lines of the given widths.
diffPlaceholder :: [Int32] -> IO Gtk.Box
diffPlaceholder widths = do
  card <- Card.new
  header <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 10
                           , #cssClasses := ["placeholder-header"]
                           ]
  Skeleton.bar 220 10 >>= #append header
  Skeleton.bar 48 10 >>= #append header
  #append card header
  body <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                         , #cssClasses := ["placeholder-body"]
                         ]
  gutter <- GI.new Gtk.Box [#widthRequest := 96, #cssClasses := ["placeholder-gutter"]]
  #append body gutter
  lines' <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 12
                           , #cssClasses := ["placeholder-lines"]
                           ]
  forM_ widths $ \w -> Skeleton.bar w 8 >>= #append lines'
  #append body lines'
  #append card body
  pure card

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: Model
sample = Model
  { files =
      [ ( Diff.File "usta.cabal" True
            [ Hunk "@@ -12,6 +12,8 @@ executable usta"
            , Context 13 13 "  main-is: Main.hs"
            , Context 14 14 "  hs-source-dirs: app"
            , Removed 15 "  build-depends:    base >=4.18 && <5"
            , Added 15 "  build-depends:"
            , Added 16 "    base,"
            , Added 17 "    gi-gtk4,"
            , Context 16 18 ""
            , Context 17 19 "  default-language: GHC2021"
            ]
        , [] )
      , ( Diff.File "flake.nix" False
            [ Hunk "@@ -20,7 +20,7 @@ ustaFor = pkgs:"
            , Context 21 21 "          isLibrary = false;"
            , Context 22 22 "          isExecutable = true;"
            , Removed 23 "          executableHaskellDepends = with hsPkgs; [ base ];"
            , Added 23 "          executableHaskellDepends = with hsPkgs; [ base gi-gtk4 ];"
            , Context 24 24 "          license = pkgs.lib.licenses.asl20;"
            ]
        , [ Comment 4 "abdivasiyev" True
              "Add wrapGAppsHook4 here once Main opens a window, otherwise the app can't find the GSettings schemas."
          ] )
      ]
  , range = "8427b36..e1c90fa"
  , checks = [(True, "nix build", "3m 24s")]
  , reviewers = [(Warning, "abdivasiyev", "awaiting")]
  , approvalsNeeded = 1
  }

css :: Text
css = mconcat
  [ "box.u-files label.small { font-size: 12px; }"
  , "box.u-files label.strong { font-weight: 600; }"
  , "box.u-files label.added { color: var(--success); }"
  , "box.u-files label.removed { color: var(--danger); }"
  , "box.u-files box.file-list {"
  , "  padding: 12px; background: var(--sidebar); border-right: 1px solid var(--border);"
  , "}"
  , "box.u-files button.file-row {"
  , "  padding: 8px; border-radius: 6px; border: none; box-shadow: none; background: transparent;"
  , "  background-image: none; color: var(--text); font-weight: 400;"
  , "}"
  , "box.u-files button.file-row:hover { background: var(--border-subtle); }"
  , "box.u-files button.file-row.current { background: var(--nav-active); }"
  , "box.u-files box.viewed-mark {"
  , "  min-width: 14px; min-height: 14px; border-radius: 3px; border: 1.5px solid var(--text-muted);"
  , "}"
  , "box.u-files box.viewed-mark.done { border-color: var(--success); }"
  , "box.u-files box.diffs { padding: 16px; }"
  , "box.u-files box.thread-body { padding: 12px; }"
  , "box.u-files box.thread-reply { padding: 10px 12px; border-top: 1px solid var(--border-subtle); }"
  , "box.u-files box.review {"
  , "  padding: 16px; background: var(--surface); border-left: 1px solid var(--border);"
  , "}"
  , "box.u-files textview.editor { border: 1px solid var(--input-border); border-radius: 6px; }"
  , "box.u-files textview.editor text { background: var(--surface); color: var(--text); }"
  , "box.u-files box.fetch-banner {"
  , "  padding: 12px 16px; background: var(--selected); border-bottom: 1px solid var(--border);"
  , "}"
  , "box.u-files box.fetch-banner spinner { color: var(--accent); }"
  , "box.u-files box.fetch-banner label.percent { color: var(--text-soft); }"
  , "box.u-files label.list-note { margin: 0 4px 6px; }"
  , "box.u-files box.fetch-row { padding: 8px; border-radius: 6px; }"
  , "box.u-files box.fetch-row.current { background: var(--nav-active); }"
  , "box.u-files box.fetch-row label.ready { color: var(--success); }"
  , "box.u-files box.fetch-row label.waiting { color: var(--text-muted); }"
  , "box.u-files.fetching box.diffs spinner { color: var(--accent); }"
  , "box.u-files box.placeholder-header {"
  , "  padding: 12px; background: var(--window); border-bottom: 1px solid var(--border-subtle);"
  , "  border-radius: 8px 8px 0 0;"
  , "}"
  , "box.u-files box.placeholder-gutter { background: var(--window); }"
  , "box.u-files box.placeholder-lines { padding: 12px 20px 16px; }"
  ]
