{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Files tab of a pull request: changed files on the left, their
-- diffs with review threads in the middle, and your review on the right.
module Usta.UI.Pages.PullRequest.Files
  ( Model (..)
  , Actions (..)
  , fromFileDiff
  , fromFileDiffs
  , sample
  , new
  , Fetching (..)
  , fetchingSample
  , fetching
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (filterM, forM, forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian, utc)
import qualified GI.GLib as GLib
import qualified GI.Graphene as Graphene
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Diff as Diff
import Usta.UI.Components.Diff (Line (..))
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.ImageView as ImageView
import qualified Usta.UI.Components.Progress as Progress
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Components.Skeleton as Skeleton
import qualified Usta.UI.Components.Thread as Thread
import Usta.UI.Format (Clock (..))
import Usta.UI.Style (Tone (..))
import qualified Usta.Types.Diff as D
import Usta.Types.Thread (Threads (..))
import qualified Usta.Types.Thread as Th
import Usta.Types.Discussion (Verdict (..))

type Model :: Type
data Model = Model
  { files :: [Diff.File]
  , threads :: Threads
    -- ^ The review conversations, placed under their lines.
  , me :: Text
    -- ^ The user's login, whose own notes are never new.
  , clock :: Clock
  , source :: Text
    -- ^ Where the diff came from, e.g. "Diff computed locally".
  , range :: Text
    -- ^ The commits the diff was computed between, e.g. "8427b36..e1c90fa";
    -- empty when not known.
  , checks :: [(Bool, Text, Text)]
    -- ^ Passed, name, duration.
  , reviewers :: [(Tone, Text, Text)]
    -- ^ Status, name, state.
  , approvalsNeeded :: Int
  , images :: [(Text, (Maybe Text, Maybe Text))]
    -- ^ By shown path, the image files changed: their path before (none
    -- when added) and after (none when deleted).
  }

-- | What the review panel does. Both run off the main thread; a 'Left' is
-- said in the panel.
type Actions :: Type
data Actions = Actions
  { review :: Verdict -> Text -> IO (Either Text ())
    -- ^ Submits the review; the panel then says so and empties.
  , merge :: IO (Either Text ())
    -- ^ Squash and merge; 'refresh' runs after.
  , version :: Bool -> Text -> IO (Either Text ByteString)
    -- ^ A file as it was before the pull request ('True') or is in it.
  , refresh :: IO ()
  , reply :: Th.Thread -> Text -> IO (Either Text ())
    -- ^ Adds a note to a thread.
  }

-- | One changed file as shown in both columns.
type Entry :: Type
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

new :: Actions -> Model -> IO Gtk.Widget
new actions m = do
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
  label m.source ["muted", "small"] >>= #append toolbar
  when (not (T.null m.range)) $ label m.range ["muted", "small", "mono"] >>= #append toolbar
  #append main toolbar
  threadBar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #visible := False, #cssClasses := ["thread-bar"]]
  (threadFilter, filterButtons) <- Segmented.new ["All threads", "Open only", "Hidden"] 0
  #append threadBar threadFilter
  threadSummary <- label "" ["small", "soft"]
  #append threadBar threadSummary
  #append main threadBar
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
        seen <- length . filter id <$> mapM (\e -> Gtk.widgetHasCssClass e.mark "done") entries
        GI.set progress [ #label := T.pack (show seen) <> " of " <> T.pack (show (length entries))
                                    <> " files viewed" ]

  placedRef <- newIORef []
  entries <- forM m.files $ \f -> do
    placed <- forM [(t, at) | t <- m.threads.threads, t.path == f.path, Just at <- [place f.diffLines t]] $ \(t, (index, outdated)) -> do
      drawn <- Thread.new Thread.Shown {thread = t, outdated = outdated, showCode = False, lastReviewed = m.threads.lastReviewed, me = m.me, clock = m.clock, reply = actions.reply t}
      pure (index, (t, outdated, drawn))
    modifyIORef' placedRef (<> map snd placed)
    let attached = [(index, drawn.widget) | (index, (_, _, drawn)) <- placed]
    -- An image shows before and after under its "Binary file" line.
    pictures <- forM (lookup f.path m.images) $ \(before, after) -> do
      slot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
      Skeleton.block 120 >>= #append slot
      void $ forkIO $ do
        let load old = maybe (pure Nothing) (fmap (either (const Nothing) Just) . actions.version old)
        olds <- load True before
        news <- load False after
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          old <- maybe (pure Nothing) ImageView.texture olds
          new' <- maybe (pure Nothing) ImageView.texture news
          Gtk.widgetGetFirstChild slot >>= mapM_ (Gtk.boxRemove slot)
          if isNothing old && isNothing new'
            then label "The image could not be loaded." ["muted", "small"] >>= #append slot
            else ImageView.pair old new' >>= #append slot
          pure False
      (,) 0 <$> Gtk.toWidget slot
    markBox <- viewedMark f.viewed
    diff <- Diff.new f (attached <> maybe [] pure pictures) $ \isViewed -> do
      setViewed markBox isViewed
      updateProgress
    #append main diff
    button <- fileRow f markBox [(outdated, drawn) | (_, (_, outdated, drawn)) <- placed]
    #append side button
    pure (Entry f button markBox diff)
  writeIORef entriesRef entries
  #append side progress
  updateProgress

  let setCurrent current =
        forM_ entries $ \e ->
          if e.file.path == current.file.path
            then Gtk.widgetAddCssClass e.row "current"
            else Gtk.widgetRemoveCssClass e.row "current"
      -- Where each shown diff starts inside the scrolled column.
      offsets = do
        toolbarHeight <- fromIntegral <$> Gtk.widgetGetHeight toolbar
        shown <- filterM (Gtk.widgetGetVisible . (.card)) entries
        heights <- mapM (fmap fromIntegral . Gtk.widgetGetHeight . (.card)) shown
        let starts = scanl (\y h -> y + h + gap) (padding + toolbarHeight + gap) heights
        pure (zip shown starts)

  forM_ entries $ \e ->
    void $ GI.on e.row #clicked $ do
      positions <- offsets
      forM_ [y | (other, y) <- positions, other.file.path == e.file.path] $ \y ->
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
      let keep = query `T.isInfixOf` T.toLower e.file.path
      GI.set e.row [#visible := keep]
      GI.set e.card [#visible := keep]

  forM_ (take 1 entries) setCurrent

  -- Threads: the filter, the summary, jumping to one, and the keys.
  placed <- readIORef placedRef
  let open' = [d | (_, o, d) <- placed, not o, not d.resolved]
      counted n word = T.pack (show n) <> " " <> word
      newOnes = length [() | (_, _, d) <- placed, d.isNew]
  unless (null placed) $ do
    GI.set threadBar [#visible := True]
    GI.set threadSummary [#label := T.intercalate " · " (
      [counted (length open') "open"]
        <> [counted n "resolved" | let n = length [() | (_, o, d) <- placed, not o, d.resolved], n > 0]
        <> [counted (length [() | (_, o, _) <- placed, o]) "outdated" | any (\(_, o, _) -> o) placed]
        <> [counted newOnes (if newOnes == 1 then "new reply" else "new replies") <> " since your last review" | newOnes > 0])]
  forM_ (zip [0 :: Int ..] filterButtons) $ \(which, button) ->
    void $ GI.on button #toggled $ do
      active <- GI.get button #active
      when active $ forM_ placed $ \(_, _, d) ->
        GI.set d.widget [#visible := case which of
          0 -> True
          1 -> not d.resolved
          _ -> False]
  let jump (_, _, d) = do
        d.setOpen True
        GI.set d.widget [#visible := True]
        origin <- Graphene.newZeroPoint
        (found, at) <- Gtk.widgetComputePoint d.widget main origin
        when found $ Graphene.getPointY at >>= \y -> Gtk.adjustmentSetValue adjustment (realToFrac y - padding)
  currentThread <- newIORef (-1 :: Int)
  let step by = unless (null placed) $ do
        i <- readIORef currentThread
        let next = (i + by) `mod` length placed
        writeIORef currentThread next
        jump (placed !! next)
  keys <- GI.new Gtk.ShortcutController []
  shortcut keys "bracketright" (step 1)
  shortcut keys "bracketleft" (step (-1))
  -- Enter and r act on the thread the brackets last went to.
  let onCurrent act = readIORef currentThread >>= \i ->
        when (i >= 0 && i < length placed) $ let (_, _, d) = placed !! i in act d
  shortcut keys "Return" (onCurrent (.toggle))
  shortcut keys "r" (onCurrent (.focusReply))
  Gtk.widgetAddController columns keys

  #append columns scroll
  review actions m (zip placed (map jump placed)) >>= #append columns
  Gtk.toWidget columns

-- | The last part of a path, as the Threads list names files.
fileName :: Text -> Text
fileName = T.takeWhileEnd (/= '/')

-- | Where a thread goes in its file's diff: under its line when that line
-- is still as it was, otherwise, outdated, under the nearest line left.
-- 'Nothing' when the diff has no new-side lines at all.
place :: [Line] -> Th.Thread -> Maybe (Int, Bool)
place lines' t = case [i | (i, n, text) <- numbered, n == t.line, Just text == commented] of
  i : _ -> Just (i, False)
  [] | null numbered -> Nothing
     | otherwise -> Just (snd (minimum [(abs (n - t.line), i) | (i, n, _) <- numbered]), True)
  where
    numbered = [(i, n, text) | (i, l) <- zip [0 ..] lines', Just (n, text) <- [newSide l]]
    newSide l = case l of
      Context _ n text -> Just (n, text)
      Added n text -> Just (n, text)
      _ -> Nothing
    -- The hunk's last line is the commented one, after its "+" or " ".
    commented = case reverse t.code of
      l : _ | Just (c, rest) <- T.uncons l, c /= '-' -> Just rest
      _ -> Nothing

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

-- | A file in the list, with its thread counts under the name.
fileRow :: Diff.File -> Gtk.Box -> [(Bool, Thread.Thread)] -> IO Gtk.Button
fileRow f markBox threads' = do
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  #append content markBox
  names <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #hexpand := True]
  label f.path ["mono", "small"] >>= #append names
  let opened = length [() | (o, d) <- threads', not o, not d.resolved]
      parts = [T.pack (show opened) <> " open" | opened > 0]
        <> [T.pack (show n) <> " resolved" | let n = length [() | (o, d) <- threads', not o, d.resolved], n > 0]
        <> [T.pack (show n) <> " outdated" | let n = length [() | (o, _) <- threads', o], n > 0]
  unless (null parts) $ label (T.intercalate " · " parts) ["muted", "tiny"] >>= #append names
  #append content names
  label ("+" <> count isAdded) ["mono", "small", "added"] >>= #append content
  label ("−" <> count isRemoved) ["mono", "small", "removed"] >>= #append content
  GI.new Gtk.Button [#child := content, #hasFrame := False, #cssClasses := ["file-row"]]
  where
    count p = T.pack (show (length (filter p f.diffLines)))
    isAdded (Added _ _) = True
    isAdded _ = False
    isRemoved (Removed _ _) = True
    isRemoved _ = False

-- | The review panel; @listed@ are the threads, each with how to jump to
-- it.
review :: Actions -> Model -> [((Th.Thread, Bool, Thread.Thread), IO ())] -> IO Gtk.Box
review actions m listed = do
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
  summary <- GI.new Gtk.TextView [ #wrapMode := Gtk.WrapModeWordChar
                                 , #heightRequest := 72
                                 , #topMargin := 8
                                 , #bottomMargin := 8
                                 , #leftMargin := 10
                                 , #rightMargin := 10
                                 , #cssClasses := ["editor"]
                                 ]
  #append verdict summary
  submit <- Button.new Button.defaults { Button.label = Just "Submit review", Button.variant = Button.Primary }
  #append verdict submit
  said <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #visible := False, #cssClasses := ["small"]]
  #append verdict said
  #append side verdict

  -- Every thread, to jump to.
  unless (null listed) $ do
    box <- section "Threads"
    forM_ listed $ \((t, outdated, d), go) -> do
      row' <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
      label (if outdated then "◷" else if d.resolved then "✓" else "●") [if outdated then "muted" else if d.resolved then "added" else "waiting"] >>= #append row'
      let first = case t.notes of
            n : _ -> " · " <> T.takeWhile (/= '\n') n.body
            [] -> ""
      text <- label (fileName t.path <> ":" <> T.pack (show t.line) <> (if outdated then " · outdated" else "") <> first) ["small"]
      GI.set text [#hexpand := True, #ellipsize := Pango.EllipsizeModeEnd]
      #append row' text
      when d.isNew $ Badge.new Accent "new" >>= #append row'
      button <- GI.new Gtk.Button [#child := row', #hasFrame := False, #cssClasses := ["thread-row"]]
      void $ GI.on button #clicked go
      #append box button
    #append side box

  -- Buttons wait for the forge, so a second click sends nothing twice.
  let run button doing act done = do
        GI.set said [#visible := False]
        Button.busy button ([] :: [Gtk.Button]) doing act $
          either (\text -> GI.set said [#label := text, #visible := True, #cssClasses := ["small", "problem"]]) (const done)
      -- In the order of the radios.
      verdicts = [CommentOnly, Approve, RequestChanges]
      send = do
        chosen <- filterM (`GI.get` #active) radios
        let pick = [v | (v, r) <- zip verdicts radios, r `elem` chosen]
        buffer <- Gtk.textViewGetBuffer summary
        text <- maybe "" id <$> GI.get buffer #text
        -- A comment-only review needs words; the forge refuses an empty one.
        forM_ (take 1 pick) $ \v -> unless (v == CommentOnly && T.null (T.strip text)) $
          run submit "Submitting…" (actions.review v text) $ do
            Gtk.textBufferSetText buffer "" (-1)
            GI.set said [#label := "Review submitted.", #visible := True, #cssClasses := ["small", "muted"]]
  void $ GI.on submit #clicked send
  keys <- GI.new Gtk.ShortcutController []
  shortcut keys "<Primary>Return" send
  Gtk.widgetAddController summary keys

  checksBox <- section "Checks"
  forM_ m.checks $ \(passed, name, took) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    Icon.toned 14 (if passed then Success else Danger) (if passed then Icon.Check else Icon.Cross)
      >>= #append row
    nameLabel <- label name ["mono", "small"]
    GI.set nameLabel [#hexpand := True]
    #append row nameLabel
    label took ["muted", "small"] >>= #append row
    #append checksBox row
  -- Usta has no checks or reviewers here yet; empty sections are hidden.
  unless (null m.checks) $ #append side checksBox

  reviewersBox <- section "Reviewers"
  forM_ m.reviewers $ \(tone, name, state) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    Badge.dot tone >>= #append row
    label name ["small"] >>= #append row
    label ("· " <> state) ["muted", "small"] >>= #append row
    #append reviewersBox row
  unless (null m.reviewers) $ #append side reviewersBox

  bottom <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 8
                           , #vexpand := True
                           , #valign := Gtk.AlignEnd
                           ]
  mergeButton <- Button.new Button.defaults
    { Button.label = Just "Squash and merge", Button.variant = Button.Primary }
  GI.set mergeButton [#sensitive := m.approvalsNeeded == 0]
  void $ GI.on mergeButton #clicked $ run mergeButton "Merging…" actions.merge actions.refresh
  #append bottom mergeButton
  void $ if m.approvalsNeeded == 0
    then pure ()
    else do
      needs <- label ("Needs " <> T.pack (show m.approvalsNeeded) <> " approval") ["muted", "small"]
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
type Fetching :: Type
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
  Progress.caption [Progress.Plain "Fetching ", Progress.Code f.branchName, Progress.Plain " into the local mirror"]
    >>= #append banner
  progress <- Progress.bar (Progress.Percent f.percent)
  GI.set progress [#hexpand := False, #widthRequest := 320, #valign := Gtk.AlignCenter]
  #append banner progress
  label (T.pack (show (round f.percent :: Int)) <> "%") ["mono", "percent"] >>= #append banner
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
  label (T.pack (show f.fileCount) <> " files · list from the forge API") ["small", "muted", "list-note"]
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
  forM_ (zip [0 :: Int ..] f.ready) $ \(i, file) -> entry file.path "Ready" (i == 0)
  forM_ f.waiting $ \path -> entry path "Waiting" False
  forM_ [190, 150, 170, 130, 160] $ \w -> do
    placeholder <- Skeleton.bar w 10
    GI.set placeholder [#marginStart := 8, #marginTop := 14]
    #append side placeholder
  #append columns side

  diffs <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                          , #spacing := round gap
                          , #cssClasses := ["diffs"]
                          ]
  forM_ f.ready $ \file -> Diff.plain file >>= #append diffs
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

-- | A changed file as the diff view shows it. A binary file has no lines,
-- so it gets one that says so.
fromFileDiff :: D.FileDiff -> Diff.File
fromFileDiff f = Diff.File
  { path = case f.change of
      D.Renamed {from = old} -> old <> " → " <> f.path
      _ -> f.path
  , viewed = False
  , diffLines = if f.binary then [Hunk "Binary file: no lines to show"] else map line f.lines
  }
  where
    line l = case l of
      D.Hunk {header = h} -> Hunk h
      D.Context {old = o, new = n, text = t} -> Context o n t
      D.Insertion {new = n, text = t} -> Added n t
      D.Deletion {old = o, text = t} -> Removed o t

-- | The Files tab for a pull request's changes; @source@ says where they
-- came from. Review threads, checks and reviewers come with the review.
fromFileDiffs :: Clock -> Text -> [D.FileDiff] -> Model
fromFileDiffs c source changes = Model
  { files = map fromFileDiff changes
  , threads = Threads [] Nothing
  , me = ""
  , clock = c
  , source = source
  , range = ""
  , checks = []
  , reviewers = []
  , approvalsNeeded = 0
  , images =
      [ ((fromFileDiff f).path, (before f, after f))
      | f <- changes, f.binary, isImage f.path
      ]
  }
  where
    before f = case f.change of
      D.Added -> Nothing
      D.Renamed {from = old} -> Just old
      _ -> Just f.path
    after f = case f.change of
      D.Deleted -> Nothing
      _ -> Just f.path
    isImage path = any (`T.isSuffixOf` T.toLower path) [".png", ".jpg", ".jpeg", ".gif", ".webp", ".bmp", ".ico", ".svg"]

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: Model
sample = Model
  { files =
      [ Diff.File "usta.cabal" True
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
      , Diff.File "flake.nix" False
            [ Hunk "@@ -20,7 +20,7 @@ ustaFor = pkgs:"
            , Context 21 21 "          isLibrary = false;"
            , Context 22 22 "          isExecutable = true;"
            , Removed 23 "          executableHaskellDepends = with hsPkgs; [ base ];"
            , Added 23 "          executableHaskellDepends = with hsPkgs; [ base gi-gtk4 ];"
            , Context 24 24 "          license = pkgs.lib.licenses.asl20;"
            ]
      ]
  , threads = Threads [] Nothing
  , me = "abdivasiyev"
  , clock = Clock {now = UTCTime (fromGregorian 2026 11 1) 0, zone = utc}
  , source = "Diff computed locally"
  , range = "8427b36..e1c90fa"
  , checks = [(True, "nix build", "3m 24s")]
  , reviewers = [(Warning, "abdivasiyev", "awaiting")]
  , approvalsNeeded = 1
  , images = []
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
  , "box.u-files label.problem { color: var(--danger); }"
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
