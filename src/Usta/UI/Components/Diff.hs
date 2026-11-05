{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A unified diff of one file: a header with the path, counts and a
-- "Viewed" toggle, then numbered lines colored by kind. Any widget (a
-- review thread, say) can be attached below a line, and lines of the new
-- version can be chosen, by dragging down or Shift-clicking their
-- numbers, to comment on or link to.
module Usta.UI.Components.Diff
  ( Line (..)
  , File (..)
  , Commenting (..)
  , Keys (..)
  , Options (..)
  , defaultOptions
  , new
  , newWith
  , plain
  , stat
  , css
  ) where

import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Graphene as Graphene
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.FileIcon as FileIcon
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Highlight as Highlight
import Text.Read (readMaybe)

type Line :: Type
data Line
  = Hunk Text
    -- ^ The "@@ -12,6 +12,8 @@" header line.
  | Context Int Int Text
    -- ^ Unchanged: old number, new number, text.
  | Added Int Text
  | Removed Int Text

type File :: Type
data File = File
  { path :: Text
  , viewed :: Bool
  , diffLines :: [Line]
  }

-- | How a diff lets the user note on lines of the new version, or link to
-- them: a "+" shows by the line under the pointer, and chosen lines get a
-- bar with Comment and Copy link.
type Commenting :: Type
data Commenting = Commenting
  { start :: (Int, Int) -> [Text] -> Gtk.Box -> IO ()
    -- ^ The first and last line's numbers in the new version, their code,
    -- and the box under the last one, which the note is written in.
  , links :: (Int, Int) -> [(Text, Text)]
    -- ^ The links to those lines, each with its name: the first is Copy
    -- link's, the second the permalink's.
  , keys :: IORef (Maybe Keys)
    -- ^ What the keys do on the diff the pointer is in, or whose lines
    -- are chosen.
  , fullText :: IO (Either Text Text)
    -- ^ The file as the pull request leaves it, to show more unchanged
    -- lines around a change; off the main thread.
  }

-- | The keys' work on one diff.
type Keys :: Type
data Keys = Keys
  { comment :: IO ()
    -- ^ On the chosen lines, or the line under the pointer.
  , copy :: Int -> IO ()
    -- ^ Copies the link at that place in 'links'.
  , grow :: Int -> IO ()
    -- ^ Moves the chosen range's end down (1) or up (-1) a line; from the
    -- line under the pointer when none are chosen.
  , clear :: IO ()
  }

-- | How a diff is drawn, kept for every pull request.
type Options :: Type
data Options = Options
  { split :: Bool
    -- ^ The old and new version side by side.
  , ignoreSpace :: Bool
    -- ^ Lines changed only in their whitespace are shown unchanged.
  , changedWords :: Bool
    -- ^ In a changed line, the part that changed is marked.
  }
  deriving stock (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {split = False, ignoreSpace = False, changedWords = True}

-- | @attached@ pairs a line's index with a widget shown under it.
-- A viewed file shows only its header; @onViewed@ hears every toggle.
new :: File -> [(Int, Gtk.Widget)] -> Commenting -> (Bool -> IO ()) -> IO Gtk.Box
new = newWith defaultOptions

newWith :: Options -> File -> [(Int, Gtk.Widget)] -> Commenting -> (Bool -> IO ()) -> IO Gtk.Box
newWith opts file attached commenting onViewed = build opts file attached (Just commenting) (Just onViewed)

-- | A diff to read, without the "Viewed" toggle, e.g. for one commit.
plain :: File -> IO Gtk.Box
plain file = build defaultOptions file { viewed = False } [] Nothing Nothing

build :: Options -> File -> [(Int, Gtk.Widget)] -> Maybe Commenting -> Maybe (Bool -> IO ()) -> IO Gtk.Box
build opts original attached commenting onViewed = do
  -- Whitespace-only changes become unchanged lines; the removed half is
  -- kept, hidden, so the lines keep their places for threads.
  let (file, hidden) = if opts.ignoreSpace then whitespaceFolded original else (original, [])
  card <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #cssClasses := ["u-card", "u-diff"]
                         ]
  header <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 10
                           , #cssClasses := ["diff-header"]
                           ]
  FileIcon.file 16 file.path >>= #append header
  label file.path ["mono", "strong"] >>= #append header
  label (stat file) ["muted", "small"] >>= #append header
  unless (null hidden) $
    label ("· " <> T.pack (show (length hidden)) <> (if length hidden == 1 then " whitespace-only line hidden" else " whitespace-only lines hidden")) ["muted", "small"] >>= #append header
  #append card header

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  -- The bar beside chosen lines floats over the diff.
  bodyOverlay <- GI.new Gtk.Overlay [#child := body, #visible := not file.viewed]
  rows <- if opts.split then splitRows file attached commenting body else forM (zip3 [0 :: Int ..] file.diffLines (hunks file.diffLines)) $ \(index, line, hunk) -> do
    (row, code) <- lineRow line
    when (index `elem` hidden) $ GI.set row [#visible := False]
    slot <- case (commenting, newSide line) of
      (Just c, Just number) -> do
        overlay <- GI.new Gtk.Overlay [#child := row]
        plus <- GI.new Gtk.Button [ #label := "+", #halign := Gtk.AlignStart, #valign := Gtk.AlignCenter
                                  , #visible := False, #tooltipText := "Comment on this line (c)"
                                  , #cssClasses := ["comment-plus"] ]
        Gtk.overlayAddOverlay overlay plus
        under <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
        void $ GI.on plus #clicked (c.start (number, number) [textOf line] under)
        #append body overlay
        pure (Just (overlay, plus, under))
      _ -> Nothing <$ #append body row
    forM_ [w | (i, w) <- attached, i == index] $ \w -> do
      holder <- GI.new Gtk.Box [#cssClasses := ["diff-attached"]]
      #append holder w
      #append body holder
    forM_ slot $ \(_, _, under) -> #append body under
    pure Row {index, line, hunk, widget = row, code, slot, twin = Nothing}
  -- Choosing a range of lines, and more context, work on the unified diff.
  unless opts.split $ forM_ commenting $ \c -> do
    choosing file.path body bodyOverlay rows c
    expanding c body rows
  let wordMarks = if opts.changedWords then changedParts file.diffLines hidden else []
  Highlight.follow card $ \palette -> do
    let marked = highlighted palette file
    forM_ rows $ \r -> forM_ (lookup r.index (zip [0 ..] marked)) $ \m ->
      Gtk.labelSetMarkup r.code (maybe m id (lookup r.index wordMarks))
    -- A split row's second label, for a line both sides show.
    forM_ rows $ \r -> forM_ r.twin $ \t -> forM_ (lookup r.index (zip [0 ..] marked)) (Gtk.labelSetMarkup t)
  #append card bodyOverlay
  setCollapsed card file.viewed

  forM_ onViewed $ \report -> do
    check <- GI.new Gtk.CheckButton [ #label := "Viewed"
                                    , #active := file.viewed
                                    , #hexpand := True
                                    , #halign := Gtk.AlignEnd
                                    ]
    #append header check
    void $ GI.on check #toggled $ do
      isViewed <- GI.get check #active
      GI.set bodyOverlay [#visible := not isViewed]
      setCollapsed card isViewed
      report isViewed
  pure card
  where
    setCollapsed card collapsed
      | collapsed = Gtk.widgetAddCssClass card "collapsed"
      | otherwise = Gtk.widgetRemoveCssClass card "collapsed"

-- | A drawn line, and where it is in the diff.
type Row :: Type
data Row = Row
  { index :: Int
  , line :: Line
  , hunk :: Int
    -- ^ Which hunk it is in, counting from 0.
  , widget :: Gtk.Box
  , code :: Gtk.Label
  , slot :: Maybe (Gtk.Overlay, Gtk.Button, Gtk.Box)
    -- ^ For a line of the new version: its overlay, its "+" and the box
    -- under it.
  , twin :: Maybe Gtk.Label
    -- ^ In a split diff, the other side's label for an unchanged line.
  }

-- | The lines with each pair of a removed and an added line that differ
-- only in whitespace turned into one unchanged line; the removed one's
-- index is in the second list, to hide.
whitespaceFolded :: File -> (File, [Int])
whitespaceFolded f = (f {diffLines = map snd changed}, hiddenOnes)
  where
    pairs = runPairs f.diffLines
    same = [(r, a) | (r, a) <- pairs, squash (textOf (f.diffLines !! r)) == squash (textOf (f.diffLines !! a))]
    hiddenOnes = map fst same
    changed = [ (i, case lookup i [(a, r) | (r, a) <- same] of
                     Just r | Removed old _ <- f.diffLines !! r, Added new' t <- l -> Context old new' t
                     _ -> l)
              | (i, l) <- zip [0 ..] f.diffLines ]
    squash = T.filter (not . (`elem` (" \t" :: String)))

-- | Each removed line paired with the added line at the same place in the
-- same run of changes, by index.
runPairs :: [Line] -> [(Int, Int)]
runPairs ls = go (zip [0 ..] ls)
  where
    go [] = []
    go xs =
      let (removed, rest) = span (isRemoved . snd) xs
          (added, rest') = span (isAdded . snd) rest
      in if null removed && null added then go (drop 1 xs) else zip (map fst removed) (map fst added) <> go rest'
    isRemoved l = case l of Removed _ _ -> True; _ -> False
    isAdded l = case l of Added _ _ -> True; _ -> False

-- | For each paired removed and added line, its text with the part that
-- changed marked, by index.
changedParts :: [Line] -> [Int] -> [(Int, Text)]
changedParts ls hidden = concat
  [ [(r, marked "#E5484D" old (prefix, suffix)), (a, marked "#30A46C" new' (prefix, suffix'))]
  | (r, a) <- runPairs ls, r `notElem` hidden
  , let old = textOf (ls !! r)
        new' = textOf (ls !! a)
        prefix = common old new'
        suffix = common (T.reverse (T.drop prefix old)) (T.reverse (T.drop prefix new'))
        suffix' = suffix
  , prefix + suffix < max (T.length old) (T.length new')
  ]
  where
    marked color t (p, q) =
      let (front, rest) = T.splitAt p t
          (middle, back) = T.splitAt (T.length rest - q) rest
      in Highlight.plain front <> (if T.null middle then "" else "<span background='" <> color <> "' bgalpha='35%'>" <> Highlight.plain middle <> "</span>") <> Highlight.plain back
    common x y = maybe 0 (\(c, _, _) -> T.length c) (T.commonPrefixes x y)

-- | The rows of a split diff: unchanged lines on both sides, and each run
-- of changes with its removed lines on the left beside its added ones on
-- the right. A line of the new version can be commented on with its "+".
splitRows :: File -> [(Int, Gtk.Widget)] -> Maybe Commenting -> Gtk.Box -> IO [Row]
splitRows file attached commenting body = go (zip3 [0 :: Int ..] file.diffLines (hunks file.diffLines))
  where
    go [] = pure []
    go xs@((i, l, h) : rest) = case l of
      Hunk _ -> do
        (row, code) <- lineRow l
        #append body row
        (Row {index = i, line = l, hunk = h, widget = row, code, slot = Nothing, twin = Nothing} :) <$> go rest
      Context _ _ _ -> do
        (row, left, right, slot) <- pairRow (Just (i, l)) (Just (i, l))
        attach i
        (Row {index = i, line = l, hunk = h, widget = row, code = left, slot, twin = Just right} :) <$> go rest
      _ -> do
        let (removed, after) = span (isRemoved . snd3) xs
            (added, rest') = span (isAdded . snd3) after
            n = max (length removed) (length added)
            pick ys k = if k < length ys then Just (let (j, line', _) = ys !! k in (j, line')) else Nothing
        made <- mapM (\k -> do
          (row, left, right, slot) <- pairRow (pick removed k) (pick added k)
          forM_ (pick added k) (attach . fst)
          pure ( [Row {index = j, line = line', hunk = h, widget = row, code = left, slot = Nothing, twin = Nothing} | Just (j, line') <- [pick removed k]]
              <> [Row {index = j, line = line', hunk = h, widget = row, code = right, slot, twin = Nothing} | Just (j, line') <- [pick added k]] )) [0 .. n - 1]
        if n == 0 then go (drop 1 xs) else (concat made <>) <$> go rest'
    snd3 (_, b, _) = b
    isRemoved l = case l of Removed _ _ -> True; _ -> False
    isAdded l = case l of Added _ _ -> True; _ -> False
    attach i = forM_ [w | (j, w) <- attached, j == i] $ \w -> do
      holder <- GI.new Gtk.Box [#cssClasses := ["diff-attached"]]
      #append holder w
      #append body holder
    -- One row: the left half for the old version, the right for the new.
    pairRow left right = do
      row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #homogeneous := True, #cssClasses := ["split-row"]]
      (l, lCode) <- half (fmap snd left) True
      (r, rCode) <- half (fmap snd right) False
      #append row l
      slot <- case (commenting, right >>= newSide . snd, right) of
        (Just c, Just number, Just (_, line')) -> do
          overlay <- GI.new Gtk.Overlay [#child := r]
          plus <- GI.new Gtk.Button [ #label := "+", #halign := Gtk.AlignStart, #valign := Gtk.AlignCenter
                                    , #visible := False, #tooltipText := "Comment on this line (c)", #cssClasses := ["comment-plus"] ]
          Gtk.overlayAddOverlay overlay plus
          under <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
          void $ GI.on plus #clicked (c.start (number, number) [textOf line'] under)
          motion <- Gtk.eventControllerMotionNew
          void $ GI.on motion #enter $ \_ _ -> GI.set plus [#visible := True]
          void $ GI.on motion #leave $ GI.set plus [#visible := False]
          Gtk.widgetAddController overlay motion
          #append row overlay
          pure (Just (overlay, plus, under))
        _ -> Nothing <$ #append row r
      #append body row
      forM_ slot $ \(_, _, under) -> #append body under
      pure (row, lCode, rCode, slot)
    half line' isOld = do
      box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["diff-line", kindOf line' isOld]]
      let (n, sign) = case line' of
            Just (Context a b _) -> (T.pack (show (if isOld then a else b)), "")
            Just (Removed a _) -> (T.pack (show a), "−")
            Just (Added b _) -> (T.pack (show b), "+")
            _ -> ("", "")
      gutter n >>= #append box
      label sign ["sign"] >>= #append box
      code <- label "" ["code"]
      GI.set code [#hexpand := True, #selectable := True, #wrap := True, #wrapMode := Pango.WrapModeWordChar]
      #append box code
      pure (box, code)
    kindOf line' isOld = case line' of
      Just (Context _ _ _) -> "context"
      Just (Removed _ _) -> "removed"
      Just (Added _ _) -> "added"
      Nothing -> if isOld then "empty" else "empty"
      _ -> "context"

-- | Each hunk's header row gets "Expand 20 lines above": the unchanged
-- lines of the new version before the hunk, back to the one above, from
-- the whole file, read the first time.
expanding :: Commenting -> Gtk.Box -> [Row] -> IO ()
expanding c body rows = do
  whole <- newIORef Nothing
  let starts = [(r, new', old - new') | r <- rows, Hunk header <- [r.line], Just (old, new') <- [startsOf header]]
      before r = last (0 : [n | x <- rows, x.index < r.index, Just n <- [newSide x.line]])
  forM_ starts $ \(r, first, shift) -> do
    shown <- newIORef first
    button <- Button.new Button.defaults {Button.label = Just "↕ Expand 20 lines above", Button.variant = Button.Flat, Button.small = True}
    GI.set button [#halign := Gtk.AlignStart, #visible := first - 1 > before r]
    Gtk.widgetAddCssClass button "expand"
    above <- Gtk.widgetGetPrevSibling r.widget
    Gtk.boxInsertChildAfter body button above
    void $ GI.on button #clicked $ do
      top <- readIORef shown
      let from = max (before r + 1) (top - 20)
      Button.busy button ([] :: [Gtk.Button]) "Loading…" (cached whole) $ \case
        Left _ -> GI.set button [#visible := False]
        Right ls -> do
          -- Newest first, so each goes in right above the hunk.
          forM_ (reverse [from .. top - 1]) $ \n -> forM_ (lookup n (zip [1 ..] ls)) $ \t -> do
            (row, code) <- lineRow (Context (n + shift) n t)
            Gtk.labelSetText code t
            Gtk.boxInsertChildAfter body row (Just button)
          writeIORef shown from
          GI.set button [#visible := from > before r + 1]
  where
    cached ref = readIORef ref >>= \case
      Just ls -> pure (Right ls)
      Nothing -> c.fullText >>= \case
        Left e -> pure (Left e)
        Right t -> let ls = T.lines t in writeIORef ref (Just ls) >> pure (Right ls)

-- | Where a hunk starts in the old and new version, from its header,
-- "@@ -12,6 +12,8 @@".
startsOf :: Text -> Maybe (Int, Int)
startsOf header = case T.words header of
  "@@" : old : new' : _ -> (,) <$> number old <*> number new'
  _ -> Nothing
  where
    number w = readMaybe (T.unpack (T.takeWhile (/= ',') (T.drop 1 w)))

-- | Each line's hunk, counting from 0.
hunks :: [Line] -> [Int]
hunks = drop 1 . scanl (\n l -> case l of Hunk _ -> n + 1; _ -> n) (-1)

-- | Choosing lines: a press on the numbers chooses that line, Shift
-- extends from the first chosen one, and dragging down the numbers grows
-- the range. A range stays in one hunk and holds the new version's lines
-- in it. The bar beside it comments on it or copies a link.
choosing :: Text -> Gtk.Box -> Gtk.Overlay -> [Row] -> Commenting -> IO ()
choosing path body overlay rows c = do
  picked <- newIORef Nothing
  hoveredRow <- newIORef Nothing
  bar <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 2, #visible := False
                        , #halign := Gtk.AlignEnd, #valign := Gtk.AlignStart, #marginEnd := 12
                        , #cssClasses := ["range-bar"] ]
  which <- label "" ["muted", "small", "which"]
  commentButton <- Button.new Button.defaults {Button.label = Just "+ Comment", Button.variant = Button.Primary, Button.small = True, Button.tooltip = Just "Comment on these lines (c)"}
  copyButton <- Button.new Button.defaults {Button.label = Just "Copy link", Button.variant = Button.Flat, Button.small = True, Button.tooltip = Just "Copy the link to these lines (y)"}
  menuItems <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["menu"]]
  menu <- GI.new Gtk.Popover [#child := menuItems, #hasArrow := False, #cssClasses := ["u-range-links"]]
  more <- Button.menu Button.defaults
    {Button.icon = Just "pan-down-symbolic", Button.variant = Button.Flat, Button.small = True, Button.tooltip = Just "More links"} menu
  mapM_ (#append bar) =<< sequence [Gtk.toWidget which, Gtk.toWidget commentButton, Gtk.toWidget copyButton, Gtk.toWidget more]
  Gtk.overlayAddOverlay overlay bar

  let newRows = [r | r <- rows, isJust r.slot]
      numberOf r = maybe 0 id (newSide r.line)
      chosen = readIORef picked >>= \case
        Nothing -> pure []
        Just (a, b) -> pure [r | r <- newRows, r.index >= min a b, r.index <= max a b]
      ends rs = case rs of
        [] -> Nothing
        first : _ -> Just (numberOf first, numberOf (last rs))
      draw = do
        rs <- chosen
        forM_ newRows $ \r -> Gtk.widgetRemoveCssClass r.widget "picked"
        forM_ rs $ \r -> Gtk.widgetAddCssClass r.widget "picked"
        case (rs, ends rs) of
          (first : _, Just (a, b)) -> do
            GI.set which [#label := if a == b then "Line " <> number a else "Lines " <> number a <> "–" <> number b]
            fillMenu (a, b)
            -- Beside the first chosen line.
            origin <- Graphene.newZeroPoint
            (found, at) <- Gtk.widgetComputePoint first.widget body origin
            y <- Graphene.getPointY at
            when found $ GI.set bar [#marginTop := max 0 (round y - 4)]
            GI.set bar [#visible := True]
          _ -> GI.set bar [#visible := False]
      -- The forge's links, then the lines as a Markdown quote.
      allLinks rs range@(a, b) = c.links range <>
        [("Copy as Markdown quote", "> " <> path <> (if a == b then " line " <> number a else " lines " <> number a <> "–" <> number b)
            <> "\n> ```\n" <> T.concat ["> " <> textOf r.line <> "\n" | r <- rs] <> "> ```\n")]
      fillMenu range = do
        rs' <- chosen
        let clearMenu = Gtk.widgetGetFirstChild menuItems >>= mapM_ (\w -> Gtk.boxRemove menuItems w >> clearMenu)
        clearMenu
        forM_ (zip [0 :: Int ..] (allLinks rs' range)) $ \(i, (name, link)) -> do
          item <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2]
          label name [] >>= #append item
          shown <- label link ["muted", "small", "mono"]
          GI.set shown [#ellipsize := Pango.EllipsizeModeMiddle, #maxWidthChars := 48]
          #append item shown
          b <- GI.new Gtk.Button [#child := item, #cssClasses := ["item"]]
          void $ GI.on b #clicked (Gtk.popoverPopdown menu >> copyAt i)
          #append menuItems b
      copyAt i = do
        rs <- chosen
        forM_ (ends rs) $ \range -> forM_ (lookup i (zip [0 ..] (allLinks rs range))) $ \(_, link) -> do
          clipboard <- Gtk.widgetGetClipboard body
          provider <- GI.toGValue (Just link) >>= Gdk.contentProviderNewForValue
          void (Gdk.clipboardSetContent clipboard (Just provider))
          -- "Link copied" for a moment, in place of which lines.
          before <- GI.get which #label
          GI.set which [#label := "Link copied"]
          void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 1500 (GI.set which [#label := before] >> pure False)
      commentOn = do
        rs <- chosen
        case (rs, ends rs) of
          (_ : _, Just range) | Just (_, _, under) <- (last rs).slot -> do
            writeIORef picked Nothing
            draw
            c.start range (map (textOf . (.line)) rs) under
          _ -> readIORef hoveredRow >>= mapM_ (\r -> forM_ r.slot $ \(_, _, under) ->
            c.start (numberOf r, numberOf r) [textOf r.line] under)
      -- Within the anchor's hunk, on a line of the new version.
      extendTo r = readIORef picked >>= \case
        Just (a, _) | Just anchor <- lookup a [(x.index, x) | x <- rows], anchor.hunk == r.hunk ->
          writeIORef picked (Just (a, r.index)) >> draw
        _ -> writeIORef picked (Just (r.index, r.index)) >> draw
      -- With none chosen, the line under the pointer is chosen first.
      grow by = readIORef picked >>= \case
        Just (_, b) -> case [r | r <- (if by > 0 then id else reverse) newRows, if by > 0 then r.index > b else r.index < b] of
          r : _ -> extendTo r
          [] -> pure ()
        Nothing -> readIORef hoveredRow >>= mapM_ (\r -> writeIORef picked (Just (r.index, r.index)) >> draw)
      keys = Keys {comment = commentOn, copy = copyAt, grow, clear = writeIORef picked Nothing >> draw}
      rowAt y = do
        found <- filterM' (\r -> do
          origin <- Graphene.newZeroPoint
          (ok, at) <- Gtk.widgetComputePoint r.widget body origin
          top <- Graphene.getPointY at
          h <- fromIntegral <$> Gtk.widgetGetHeight r.widget
          pure (ok && realToFrac y >= top && realToFrac y < top + h)) newRows
        pure (case found of r : _ -> Just r; [] -> Nothing)

  void $ GI.on commentButton #clicked commentOn
  void $ GI.on copyButton #clicked (copyAt 0)

  -- Pressing and dragging on the line numbers; the text keeps its own
  -- selection. The handlers reach the gesture as ?self: once added, the
  -- widget owns it.
  drag <- Gtk.gestureDragNew
  startY <- newIORef 0
  void $ GI.on drag #dragBegin $ \x y ->
    if x > numbersWidth
      then void (Gtk.gestureSetState ?self Gtk.EventSequenceStateDenied)
      else do
        writeIORef startY y
        state <- Gtk.eventControllerGetCurrentEventState ?self
        rowAt y >>= mapM_ (\r ->
          if Gdk.ModifierTypeShiftMask `elem` state then extendTo r
          else writeIORef picked (Just (r.index, r.index)) >> draw)
        writeIORef c.keys (Just keys)
  void $ GI.on drag #dragUpdate $ \_ dy -> do
    y0 <- readIORef startY
    rowAt (y0 + dy) >>= mapM_ extendTo
  Gtk.widgetAddController body drag

  -- The "+" and the keys follow the pointer.
  forM_ newRows $ \r -> forM_ r.slot $ \(o, plus, _) -> do
    motion <- Gtk.eventControllerMotionNew
    void $ GI.on motion #enter $ \_ _ -> do
      GI.set plus [#visible := True]
      writeIORef hoveredRow (Just r)
      writeIORef c.keys (Just keys)
    void $ GI.on motion #leave $ GI.set plus [#visible := False] >> writeIORef hoveredRow Nothing
    Gtk.widgetAddController o motion
  where
    number = T.pack . show
    filterM' p = foldr (\x acc -> p x >>= \keep -> (if keep then (x :) else id) <$> acc) (pure [])

-- | Where the line numbers and sign end: two gutters and the sign column.
numbersWidth :: Double
numbersWidth = 108

-- | "+12 −3".
stat :: File -> Text
stat file = "+" <> count isAdded <> " −" <> count isRemoved
  where
    count p = T.pack (show (length (filter p file.diffLines)))
    isAdded (Added _ _) = True
    isAdded _ = False
    isRemoved (Removed _ _) = True
    isRemoved _ = False

-- | Each line's code as Pango markup, highlighted by the file's language.
-- The lines are tokenized together, so a comment or string that spans
-- lines keeps its color; hunk headers stay plain.
highlighted :: Highlight.Palette -> File -> [Text]
highlighted palette file
  | length marked == length codes = fill file.diffLines marked
  | otherwise = map (Highlight.plain . textOf) file.diffLines
  where
    codes = [t | line <- file.diffLines, Just t <- [codeOf line]]
    marked = Highlight.markup palette (Highlight.languageFor (T.unpack file.path)) (T.intercalate "\n" codes <> "\n")
    fill (line : rest) ms = case (codeOf line, ms) of
      (Just _, m : ms') -> m : fill rest ms'
      _ -> Highlight.plain (textOf line) : fill rest ms
    fill [] _ = []
    codeOf line = case line of
      Hunk _ -> Nothing
      Context _ _ t -> Just t
      Added _ t -> Just t
      Removed _ t -> Just t

-- | A line's text, without its sign.
textOf :: Line -> Text
textOf line = case line of
  Hunk t -> t
  Context _ _ t -> t
  Added _ t -> t
  Removed _ t -> t

-- | A line's number in the new version, for a line that is there.
newSide :: Line -> Maybe Int
newSide line = case line of
  Context _ b _ -> Just b
  Added b _ -> Just b
  _ -> Nothing

-- | The row, and the label its code goes in as markup.
lineRow :: Line -> IO (Gtk.Box, Gtk.Label)
lineRow line = do
  row <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #cssClasses := ["diff-line", kindClass]
                        ]
  gutter old >>= #append row
  gutter new' >>= #append row
  label sign ["sign"] >>= #append row
  -- A long line wraps in place of scrolling the diff sideways.
  text <- label "" ["code"]
  GI.set text [#hexpand := True, #selectable := True, #wrap := True, #wrapMode := Pango.WrapModeWordChar]
  #append row text
  pure (row, text)
  where
    (kindClass, old, new', sign) = case line of
      Hunk _ -> ("hunk", "", "", "")
      Context a b _ -> ("context", number a, number b, "")
      Added b _ -> ("added", "", number b, "+")
      Removed a _ -> ("removed", number a, "", "−")
    number = T.pack . show

gutter :: Text -> IO Gtk.Label
gutter n = do
  l <- GI.new Gtk.Label [ #label := n
                        , #xalign := 1
                        , #valign := Gtk.AlignStart
                        , #widthRequest := (44 :: Int32)
                        , #cssClasses := ["gutter"]
                        ]
  pure l

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #valign := Gtk.AlignStart, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-diff { border-radius: 8px; }"
  , "box.u-diff box.diff-header {"
  , "  padding: 8px 12px; background: var(--window); border-bottom: 1px solid var(--border);"
  , "  border-radius: 8px 8px 0 0;"
  , "}"
  , "box.u-diff label.strong { font-weight: 500; }"
  , "box.u-diff label.small { font-size: 12px; }"
  , "box.u-diff box.diff-line { font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , "box.u-diff label.gutter { padding: 2px 8px 2px 0; color: var(--text-muted); }"
  , "box.u-diff label.sign { min-width: 20px; padding: 2px 0; }"
  , "box.u-diff label.code { padding: 2px 16px 2px 0; }"
  , "box.u-diff box.diff-line.hunk { background: var(--selected); color: var(--text-soft); }"
  , "box.u-diff box.diff-line.hunk label.gutter { background: var(--nav-active); }"
  , "box.u-diff box.diff-line.context { background: var(--surface); }"
  , "box.u-diff box.diff-line.context label.gutter { background: var(--window); }"
  , "box.u-diff box.diff-line.added { background: var(--success-bg); color: var(--success); }"
  , "box.u-diff box.diff-line.removed { background: var(--danger-bg); color: var(--danger); }"
  , "box.u-diff box.diff-attached { padding: 8px 12px 8px 108px; background: var(--surface); }"
  -- The holder already indents what it holds.
  , "box.u-diff box.diff-attached > box.u-thread { margin: 0; }"
  , "box.u-diff box.split-row box.diff-line.empty { background: var(--window); }"
  , "box.u-diff box.split-row > box:first-child, box.u-diff box.split-row > overlay:first-child { border-right: 1px solid var(--border-subtle); }"
  , "box.u-diff box.diff-line.picked { background: var(--accent-bg); box-shadow: inset 3px 0 0 var(--accent); }"
  , "box.u-diff box.range-bar { padding: 3px; border: 1px solid var(--input-border); border-radius: 8px; background: var(--surface); box-shadow: 0 6px 18px rgba(27,29,33,0.14); }"
  , "box.u-diff box.range-bar label.which { padding: 0 8px; }"
  , "popover.u-range-links > contents { padding: 4px; border-radius: 10px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: 0 8px 24px rgba(27,29,33,0.14); }"
  , "popover.u-range-links button.item { padding: 6px 10px; border: none; box-shadow: none; background: transparent; background-image: none; }"
  , "popover.u-range-links button.item:hover { background: var(--border-subtle); }"
  , "box.u-diff button.expand { margin: 2px 0 2px 100px; font-size: 12px; color: var(--accent); }"
  , "box.u-diff button.comment-plus { min-width: 18px; min-height: 18px; margin-left: 14px; padding: 0; border-radius: 4px; background: var(--accent); color: var(--on-accent); font-weight: 700; font-size: 12px; }"
  , "box.u-diff > box:last-child { border-radius: 0 0 8px 8px; }"
  , "box.u-diff.collapsed box.diff-header { border-bottom: none; border-radius: 8px; }"
  ]
