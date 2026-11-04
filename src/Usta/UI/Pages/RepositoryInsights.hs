{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A repository's Insights, drawn from its local mirror. Activity: commits
-- and lines added and removed per week, and the contributors, for a month,
-- three months, a year or all time. Commit graph: every branch's history as
-- lanes, newest first, with the chosen commit beside it.
module Usta.UI.Pages.RepositoryInsights
  ( Actions (..)
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import qualified Data.GI.Base.GValue as GValue
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (findIndex, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime, nominalDay)
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.Graph as Graph
import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Button as Button
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Diff as Diff
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.UI.Format (Clock (..), ago, date, short)
import Usta.Types.Insights (Change (..), Node (..))

type Actions :: Type
data Actions = Actions
  { branch :: Text
    -- ^ The branch the picker shows.
  , changes :: Maybe UTCTime -> IO (Either Text [Change])
    -- ^ Each commit's size on the branch since a time; off the main thread.
  , graph :: [Text] -> Bool -> Int -> IO (Either Text [Node])
    -- ^ A page of the graph, from 1: of the given branches, or all; first
    -- parents only if asked. Off the main thread.
  , files :: Text -> IO (Either Text [Diff.File])
    -- ^ A commit's changes against its first parent, by hash.
  }

new :: Clock -> Actions -> IO Gtk.Widget
new c a = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12, #cssClasses := ["u-insights"]]
  (views, viewButtons) <- Segmented.new ["Activity", "Commit graph"] 0
  stack <- GI.new Gtk.Stack [#vexpand := True, #hexpand := True]
  activityPage <- activity c a
  void $ Gtk.stackAddNamed stack activityPage (Just "activity")
  graphBuilt <- newIORef False
  graphSlot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #vexpand := True]
  void $ Gtk.stackAddNamed stack graphSlot (Just "graph")
  let showView i = do
        when (i == 1) $ do
          built <- readIORef graphBuilt
          unless built $ writeIORef graphBuilt True >> commitGraph c a >>= #append graphSlot
        Gtk.stackSetVisibleChildName stack (if i == 1 then "graph" else "activity")
  forM_ (zip [0 :: Int ..] viewButtons) $ \(i, b) -> void $ GI.on b #toggled $ do
    active <- GI.get b #active
    when active (showView i)
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  #append top views
  #append box top
  #append box stack
  -- g goes between Activity and the graph.
  keys <- GI.new Gtk.ShortcutController []
  shortcut keys "g" $ do
    onGraph <- (== Just "graph") <$> Gtk.stackGetVisibleChildName stack
    forM_ (zip [0 :: Int ..] viewButtons) $ \(i, b) -> when (i == (if onGraph then 0 else 1)) (GI.set b [#active := True])
  Gtk.widgetAddController box keys
  Gtk.toWidget box

-- * Activity

type Range :: Type
data Range = Month | Quarter | Year | Always
  deriving stock (Eq, Enum, Bounded)

rangeName :: Range -> Text
rangeName r = case r of
  Month -> "Month"
  Quarter -> "3 months"
  Year -> "Year"
  Always -> "All time"

since :: Clock -> Range -> Maybe UTCTime
since c r = case r of
  Month -> Just (addUTCTime (-30 * nominalDay) c.now)
  Quarter -> Just (addUTCTime (-91 * nominalDay) c.now)
  Year -> Just (addUTCTime (-365 * nominalDay) c.now)
  Always -> Nothing

activity :: Clock -> Actions -> IO Gtk.Widget
activity c a = do
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 14]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  (ranges, rangeButtons) <- Segmented.new (map rangeName [minBound ..]) 2
  #append top ranges
  summary <- label "Reading the history…" ["muted", "small"]
  GI.set summary [#valign := Gtk.AlignCenter]
  #append top summary
  #append page top
  charts <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 14, #homogeneous := True]
  (commitsCard, commitsArea) <- chartCard "Commit activity" "per week"
  (codeCard, codeArea) <- chartCard "Code frequency" "lines added and removed per week"
  mapM_ (#append charts) [commitsCard, codeCard]
  #append page charts
  people <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-card", "contributors"]]
  peopleTop <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["card-top"]]
  peopleTitle <- label "Contributors" ["title"]
  GI.set peopleTitle [#hexpand := True]
  #append peopleTop peopleTitle
  (orders, orderButtons) <- Segmented.new ["Commits", "Additions", "Deletions"] 0
  #append peopleTop orders
  #append people peopleTop
  peopleList <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append people peopleList
  #append page people
  scroll <- GI.new Gtk.ScrolledWindow [#child := page, #hscrollbarPolicy := Gtk.PolicyTypeNever, #vexpand := True]

  current <- newIORef ([], Year)
  order <- newIORef (0 :: Int)
  let weeksOf range cs = case cs of
        [] -> []
        _ ->
          let start = maybe (minimum (map (.at) cs)) id (since c range)
              n = max 1 (ceiling (diffUTCTime c.now start / (7 * nominalDay)) :: Int)
              week t = min (n - 1) (max 0 (floor (diffUTCTime t start / (7 * nominalDay))))
              counts = Map.fromListWith add [(week x.at, (1 :: Int, x.added, x.removed)) | x <- cs]
              add (p, q, r) (p', q', r') = (p + p', q + q', r + r')
          in [Map.findWithDefault (0, 0, 0) i counts | i <- [0 .. n - 1]]
      drawPeople = do
        (cs, range) <- readIORef current
        o <- readIORef order
        let clear = Gtk.widgetGetFirstChild peopleList >>= mapM_ (\w -> Gtk.boxRemove peopleList w >> clear)
        clear
        let byAuthor = Map.fromListWith (<>) [(x.author, [x]) | x <- cs]
            totals = [ (name, length xs, sum (map (.added) xs), sum (map (.removed) xs), xs)
                     | (name, xs) <- Map.toList byAuthor ]
            ranked = sortOn (\(_, n, ad, de, _) -> Down (case o of 1 -> ad; 2 -> de; _ -> n)) totals
        GI.set peopleTitle [#label := "Contributors  " <> T.pack (show (length ranked))]
        forM_ (zip [1 :: Int ..] ranked) $ \(i, (name, n, ad, de, xs)) -> do
          row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 14, #cssClasses := ["person"]]
          label ("#" <> T.pack (show i)) ["muted", "rank"] >>= #append row
          Avatar.new 24 (T.toUpper (T.take 2 name)) >>= #append row
          who <- label name ["strong"]
          GI.set who [#widthRequest := 160, #ellipsize := Pango.EllipsizeModeEnd]
          #append row who
          counts <- label (T.pack (show n) <> (if n == 1 then " commit" else " commits")) []
          GI.set counts [#widthRequest := 110]
          #append row counts
          lines' <- GI.new Gtk.Label [#useMarkup := True, #xalign := 0, #widthRequest := 150, #label :=
            "<span foreground='#1F7A3A'>+" <> T.pack (show ad) <> "</span> <span foreground='#B42318'>−" <> T.pack (show de) <> "</span>"]
          #append row lines'
          let first = minimum (map (.at) xs)
              lastOne = maximum (map (.at) xs)
          span' <- label (date c first <> " → " <> (if diffUTCTime c.now lastOne < nominalDay then "today" else date c lastOne)) ["muted"]
          GI.set span' [#widthRequest := 170]
          #append row span'
          spark <- GI.new Gtk.DrawingArea [#contentWidth := 220, #contentHeight := 26, #valign := Gtk.AlignCenter]
          let points = [fromIntegral k | (k, _, _) <- weeksOf range xs] :: [Double]
          Gtk.drawingAreaSetDrawFunc spark $ Just $ \_ context w h -> renderWithContext (sparkline points (fromIntegral w) (fromIntegral h)) context
          #append row spark
          #append peopleList row
      redraw = do
        (cs, range) <- readIORef current
        let weeks = weeksOf range cs
            people' = Map.size (Map.fromList [(x.author, ()) | x <- cs])
        GI.set summary [#label := T.pack (show (length cs)) <> " commits by " <> T.pack (show people') <> (if people' == 1 then " person" else " people")
          <> " on " <> a.branch <> " · from the local mirror"]
        Gtk.drawingAreaSetDrawFunc commitsArea $ Just $ \_ context w h ->
          renderWithContext (bars [fromIntegral n | (n, _, _) <- weeks] (fromIntegral w) (fromIntegral h)) context
        Gtk.drawingAreaSetDrawFunc codeArea $ Just $ \_ context w h ->
          renderWithContext (frequency [(fromIntegral ad, fromIntegral de) | (_, ad, de) <- weeks] (fromIntegral w) (fromIntegral h)) context
        mapM_ Gtk.widgetQueueDraw [commitsArea, codeArea]
        drawPeople
      load range = void $ forkIO $ do
        result <- try (a.changes (since c range))
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          case result of
            Right (Right cs) -> writeIORef current (cs, range) >> redraw
            Right (Left problem) -> GI.set summary [#label := problem]
            Left (e :: SomeException) -> GI.set summary [#label := T.pack (show e)]
          pure False
  forM_ (zip [minBound ..] rangeButtons) $ \(range, b) -> void $ GI.on b #toggled $ do
    active <- GI.get b #active
    when active $ GI.set summary [#label := "Reading the history…"] >> load range
  forM_ (zip [0 ..] orderButtons) $ \(i, b) -> void $ GI.on b #toggled $ do
    active <- GI.get b #active
    when active (writeIORef order i >> drawPeople)
  load Year
  Gtk.toWidget scroll

chartCard :: Text -> Text -> IO (Gtk.Box, Gtk.DrawingArea)
chartCard title note = do
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #cssClasses := ["u-card", "chart"]]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  label title ["title"] >>= #append top
  label note ["muted", "small"] >>= #append top
  #append card top
  area <- GI.new Gtk.DrawingArea [#contentHeight := 170, #hexpand := True]
  #append card area
  pure (card, area)

-- | A bar a week, scaled to the tallest.
bars :: [Double] -> Double -> Double -> Cairo.Render ()
bars values w h = unless (null values) $ do
  let top = max 1 (maximum values)
      step = w / fromIntegral (length values)
      bar = max 1 (step * 0.7)
  Cairo.setSourceRGB 0.18 0.37 0.82
  forM_ (zip [0 :: Int ..] values) $ \(i, v) -> do
    let bh = (h - 4) * v / top
    Cairo.rectangle (fromIntegral i * step) (h - bh) bar bh
  Cairo.fill

-- | Lines added above the middle, in green, and removed below it, in red.
frequency :: [(Double, Double)] -> Double -> Double -> Cairo.Render ()
frequency values w h = unless (null values) $ do
  let top = max 1 (maximum (concat [[x, y] | (x, y) <- values]))
      step = w / fromIntegral (length values)
      bar = max 1 (step * 0.7)
      mid = h / 2
  Cairo.setSourceRGB 0.12 0.48 0.23
  forM_ (zip [0 :: Int ..] values) $ \(i, (ad, _)) -> Cairo.rectangle (fromIntegral i * step) (mid - (mid - 2) * ad / top) bar ((mid - 2) * ad / top)
  Cairo.fill
  Cairo.setSourceRGB 0.71 0.14 0.09
  forM_ (zip [0 :: Int ..] values) $ \(i, (_, de)) -> Cairo.rectangle (fromIntegral i * step) mid bar ((mid - 2) * de / top)
  Cairo.fill
  Cairo.setSourceRGBA 0.5 0.5 0.5 0.5
  Cairo.setLineWidth 1
  Cairo.moveTo 0 mid >> Cairo.lineTo w mid >> Cairo.stroke

sparkline :: [Double] -> Double -> Double -> Cairo.Render ()
sparkline values w h = unless (length values < 2) $ do
  let top = max 1 (maximum values)
      step = w / fromIntegral (length values - 1)
      y v = h - 2 - (h - 4) * v / top
  Cairo.setSourceRGB 0.18 0.37 0.82
  Cairo.setLineWidth 1.5
  forM_ (zip [0 :: Int ..] values) $ \(i, v) ->
    (if i == 0 then Cairo.moveTo else Cairo.lineTo) (fromIntegral i * step) (y v)
  Cairo.stroke

-- * Commit graph

commitGraph :: Clock -> Actions -> IO Gtk.Widget
commitGraph c a = do
  outer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 14, #vexpand := True]
  left <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #hexpand := True]
  bar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  scope <- Gtk.dropDownNewFromStrings ["All branches", "Only " <> a.branch]
  #append bar scope
  firstOnly <- GI.new Gtk.CheckButton [#label := "First parent only"]
  tagsOn <- GI.new Gtk.CheckButton [#label := "Tags", #active := True]
  mapM_ (#append bar) [firstOnly, tagsOn]
  find <- GI.new Gtk.SearchEntry [#placeholderText := "Find a commit", #hexpand := True, #halign := Gtk.AlignEnd, #widthRequest := 260]
  #append bar find
  #append left bar
  summary <- label "Reading the graph…" ["muted", "small"]
  #append left summary
  list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle, #cssClasses := ["u-card", "graph"]]
  scroll <- GI.new Gtk.ScrolledWindow [#child := list, #hscrollbarPolicy := Gtk.PolicyTypeNever, #vexpand := True]
  #append left scroll
  #append outer left
  side <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #widthRequest := 340, #cssClasses := ["u-card", "chosen"]]
  label "Choose a commit." ["muted"] >>= #append side
  sideScroll <- GI.new Gtk.ScrolledWindow [#child := side, #hscrollbarPolicy := Gtk.PolicyTypeNever, #widthRequest := 360, #propagateNaturalWidth := False]
  #append outer sideScroll

  -- The diff of a commit, in place of the graph, with Back.
  stack <- GI.new Gtk.Stack [#vexpand := True]
  void $ Gtk.stackAddNamed stack outer (Just "graph")
  diffPage <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12]
  diffScroll <- GI.new Gtk.ScrolledWindow [#child := diffPage, #hscrollbarPolicy := Gtk.PolicyTypeNever, #vexpand := True]
  void $ Gtk.stackAddNamed stack diffScroll (Just "diff")

  nodes <- newIORef []
  pages <- newIORef (0 :: Int)
  finished <- newIORef False
  loading <- newIORef False
  chosen <- newIORef Nothing
  let clearBox b = Gtk.widgetGetFirstChild b >>= mapM_ (\w -> Gtk.boxRemove b w >> clearBox b)
      settings = do
        i <- Gtk.dropDownGetSelected scope
        fp <- GI.get firstOnly #active
        pure ([a.branch | i == 1], fp)
      redraw = do
        ns <- readIORef nodes
        showTags <- GI.get tagsOn #active
        Gtk.listBoxRemoveAll list
        let laid = Graph.rows ns
            lanes = maximum (1 : map (.width) laid)
        forM_ (zip ns laid) $ \(n, r) -> graphRow c showTags lanes n r >>= #append list
        done <- readIORef finished
        GI.set summary [#label := T.pack (show (length ns)) <> " commits" <> (if done then "" else " so far, more as you scroll") <> " · from the local mirror"]
      loadMore = do
        busy <- readIORef loading
        done <- readIORef finished
        unless (busy || done) $ do
          writeIORef loading True
          n <- (+ 1) <$> readIORef pages
          (scope', fp) <- settings
          void $ forkIO $ do
            result <- try (a.graph scope' fp n)
            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
              writeIORef loading False
              case result of
                Right (Right page) -> do
                  modifyIORef' nodes (<> page)
                  writeIORef pages n
                  when (length page < 100) (writeIORef finished True)
                  redraw
                Right (Left problem) -> GI.set summary [#label := problem] >> writeIORef finished True
                Left (e :: SomeException) -> GI.set summary [#label := T.pack (show e)] >> writeIORef finished True
              pure False
      restart = do
        writeIORef nodes [] >> writeIORef pages 0 >> writeIORef finished False
        Gtk.listBoxRemoveAll list
        GI.set summary [#label := "Reading the graph…"]
        loadMore
      select i = do
        ns <- readIORef nodes
        when (i >= 0 && i < length ns) $ do
          row <- Gtk.listBoxGetRowAtIndex list (fromIntegral i)
          forM_ row $ \r -> Gtk.listBoxSelectRow list (Just r) >> void (Gtk.widgetGrabFocus r)
      openDiff n = do
        clearBox diffPage
        back <- Button.new Button.defaults {Button.label = Just "← Commit graph", Button.variant = Button.Flat, Button.small = True}
        GI.set back [#halign := Gtk.AlignStart]
        void $ GI.on back #clicked (Gtk.stackSetVisibleChildName stack "graph")
        #append diffPage back
        title <- label n.subject ["subject"]
        GI.set title [#wrap := True, #selectable := True]
        #append diffPage title
        fileBox <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12]
        label "Loading the changes…" ["muted"] >>= #append fileBox
        #append diffPage fileBox
        Gtk.stackSetVisibleChildName stack "diff"
        void $ forkIO $ do
          result <- try (a.files n.sha)
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            clearBox fileBox
            case result of
              Right (Right fs) -> forM_ fs (\f -> Diff.plain f >>= #append fileBox)
              Right (Left problem) -> label problem ["muted"] >>= #append fileBox
              Left (e :: SomeException) -> label (T.pack (show e)) ["muted"] >>= #append fileBox
            pure False
      showChosen n = do
        writeIORef chosen (Just n)
        clearBox side
        head' <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
        when (length n.parents > 1) $ label "Merge" ["badge", "merge"] >>= #append head'
        hash <- label (T.take 7 n.sha) ["mono", "hash"]
        GI.set hash [#hexpand := True]
        #append head' hash
        #append side head'
        title <- label n.subject ["subject"]
        GI.set title [#wrap := True, #selectable := True]
        #append side title
        label (n.author <> " · " <> ago c n.at) ["muted"] >>= #append side
        unless (null n.parents) $ label ("Parents  " <> T.intercalate " · " (map (T.take 7) n.parents)) ["mono", "small"] >>= #append side
        unless (null n.refs) $ label ("On  " <> T.intercalate ", " n.refs) ["small"] >>= \l -> GI.set l [#wrap := True] >> #append side l
        changed <- label "Changed" ["section"]
        #append side changed
        fileList <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
        label "Loading…" ["muted", "small"] >>= #append fileList
        #append side fileList
        buttons <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
        openButton <- Button.new Button.defaults {Button.label = Just "Open diff", Button.variant = Button.Primary, Button.small = True}
        copy <- Button.new Button.defaults {Button.label = Just "Copy hash", Button.small = True}
        mapM_ (#append buttons) [openButton, copy]
        #append side buttons
        void $ GI.on openButton #clicked (openDiff n)
        void $ GI.on copy #clicked (copyHash n)
        void $ forkIO $ do
          result <- try (a.files n.sha)
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            current <- readIORef chosen
            when (((.sha) <$> current) == Just n.sha) $ do
              clearBox fileList
              case result of
                Right (Right fs) -> do
                  GI.set changed [#label := T.pack (show (length fs)) <> (if length fs == 1 then " file" else " files") <> " against the first parent"]
                  forM_ fs $ \f -> do
                    line <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
                    p <- label f.path ["mono", "small"]
                    GI.set p [#hexpand := True, #ellipsize := Pango.EllipsizeModeMiddle]
                    #append line p
                    let ad = length [() | Diff.Added _ _ <- f.diffLines]
                        de = length [() | Diff.Removed _ _ <- f.diffLines]
                    GI.new Gtk.Label [#useMarkup := True, #label := "<span foreground='#1F7A3A'>+" <> T.pack (show ad) <> "</span> <span foreground='#B42318'>−" <> T.pack (show de) <> "</span>", #cssClasses := ["small"]] >>= #append line
                    #append fileList line
                Right (Left problem) -> label problem ["muted", "small"] >>= #append fileList
                Left (e :: SomeException) -> label (T.pack (show e)) ["muted", "small"] >>= #append fileList
            pure False
      copyHash n = do
        clipboard <- Gtk.widgetGetClipboard outer
        value <- GValue.toGValue (Just n.sha)
        provider <- Gdk.contentProviderNewForValue value
        void (Gdk.clipboardSetContent clipboard (Just provider))
      selectedIndex = Gtk.listBoxGetSelectedRow list >>= maybe (pure (-1)) (fmap fromIntegral . Gtk.listBoxRowGetIndex)
      jumpTo p = do
        ns <- readIORef nodes
        forM_ (findIndex (\x -> p x) ns) select
  void $ GI.on list #rowSelected $ \row -> forM_ row $ \r -> do
    i <- fromIntegral <$> Gtk.listBoxRowGetIndex r
    ns <- readIORef nodes
    when (i >= 0 && i < length ns) $ showChosen (ns !! i)
  void $ GI.on list #rowActivated $ \_ -> readIORef chosen >>= mapM_ openDiff
  void $ GI.on scroll #edgeReached $ \pos -> when (pos == Gtk.PositionTypeBottom) loadMore
  void $ GI.on scope (GI.PropertyNotify #selected) (const restart)
  void $ GI.on firstOnly #toggled restart
  void $ GI.on tagsOn #toggled redraw
  void $ GI.on find #activate $ do
    q <- T.toLower <$> Gtk.editableGetText find
    unless (T.null q) $ jumpTo (\n -> q `T.isInfixOf` T.toLower (n.subject <> " " <> n.author) || q `T.isPrefixOf` n.sha)

  -- j k move, Enter opens, [ ] go to the parent or child, y copies, / finds.
  keys <- GI.new Gtk.ShortcutController []
  shortcut keys "j" (selectedIndex >>= select . (+ 1))
  shortcut keys "k" (selectedIndex >>= select . subtract 1)
  shortcut keys "bracketleft" $ readIORef chosen >>= mapM_ (\n -> forM_ (take 1 n.parents) $ \p -> jumpTo ((== p) . (.sha)))
  shortcut keys "bracketright" $ readIORef chosen >>= mapM_ (\n -> jumpTo (\x -> n.sha `elem` x.parents))
  shortcut keys "y" (readIORef chosen >>= mapM_ copyHash)
  shortcut keys "slash" (void (Gtk.widgetGrabFocus find))
  shortcut keys "Escape" (Gtk.stackSetVisibleChildName stack "graph")
  Gtk.widgetAddController stack keys
  loadMore
  Gtk.toWidget stack

-- | One commit's row: its lanes, refs, subject, author, when and hash.
graphRow :: Clock -> Bool -> Int -> Node -> Graph.Row -> IO Gtk.Box
graphRow c showTags lanes n r = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["commit"]]
  area <- GI.new Gtk.DrawingArea [#contentWidth := fromIntegral (lanes * laneWidth + 8), #contentHeight := fromIntegral rowHeight]
  Gtk.drawingAreaSetDrawFunc area $ Just $ \_ context _ h -> renderWithContext (lanesOf r (fromIntegral h)) context
  #append row area
  forM_ [ref | ref <- n.refs, showTags || not ("tag: " `T.isPrefixOf` ref)] $ \ref -> do
    let isTag = "tag: " `T.isPrefixOf` ref
    chip <- label (if isTag then T.drop 5 ref else ref) ["mono", "chip", if isTag then "tag" else "ref"]
    GI.set chip [#valign := Gtk.AlignCenter]
    #append row chip
  subject <- label n.subject [if r.isMerge then "merge-subject" else "subject"]
  GI.set subject [#hexpand := True, #ellipsize := Pango.EllipsizeModeEnd]
  #append row subject
  Avatar.new 22 (T.toUpper (T.take 2 n.author)) >>= \av -> GI.set av [#valign := Gtk.AlignCenter] >> #append row av
  when' <- label (short c n.at) ["muted", "small"]
  GI.set when' [#widthRequest := 56]
  #append row when'
  label (T.take 7 n.sha) ["mono", "hash"] >>= #append row
  pure row

laneWidth, rowHeight :: Int
laneWidth = 16
rowHeight = 40

-- | The lines through a row and the commit's dot; a merge's dot is hollow.
lanesOf :: Graph.Row -> Double -> Cairo.Render ()
lanesOf r h = do
  let x col = 10 + fromIntegral (col * laneWidth)
      mid = h / 2
      line (a, b) y0 y1 = do
        color a b
        Cairo.moveTo (x a) y0
        if a == b then Cairo.lineTo (x b) y1 else Cairo.curveTo (x a) ((y0 + y1) / 2) (x b) ((y0 + y1) / 2) (x b) y1
        Cairo.stroke
      color a b = let (cr, cg, cb) = laneColor (max a b) in Cairo.setSourceRGB cr cg cb
  Cairo.setLineWidth 2
  forM_ r.above $ \e -> line e 0 mid
  forM_ r.below $ \e -> line e mid h
  let (cr, cg, cb) = laneColor r.column
  Cairo.setSourceRGB cr cg cb
  Cairo.arc (x r.column) mid (if r.isMerge then 5 else 4) 0 (2 * pi)
  if r.isMerge
    then Cairo.setSourceRGB 1 1 1 >> Cairo.fillPreserve >> Cairo.setSourceRGB cr cg cb >> Cairo.stroke
    else Cairo.fill

laneColor :: Int -> (Double, Double, Double)
laneColor i = colors !! (i `mod` length colors)
  where
    colors = [(0.18, 0.37, 0.82), (0.12, 0.48, 0.23), (0.76, 0.25, 0.05), (0.43, 0.16, 0.85), (0.54, 0.35, 0.0), (0.07, 0.5, 0.55)]

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-insights { padding: 14px 20px; }"
  , "box.u-insights box.chart { padding: 14px; }"
  , "box.u-insights label.title { font-weight: 600; }"
  , "box.u-insights label.small { font-size: 12px; }"
  , "box.u-insights box.contributors box.card-top { padding: 12px 14px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-insights box.person { padding: 10px 14px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-insights label.rank { min-width: 26px; }"
  , "box.u-insights label.strong { font-weight: 600; }"
  , "box.u-insights list.graph { background: transparent; border-radius: 8px; }"
  , "box.u-insights list.graph row { padding: 0 12px; border-bottom: 1px solid var(--border-subtle); background: var(--surface); }"
  , "box.u-insights list.graph row:selected { background: var(--accent-bg); box-shadow: inset 3px 0 0 var(--accent); }"
  , "box.u-insights label.merge-subject { color: var(--text-muted); }"
  , "box.u-insights label.hash { color: var(--accent); font-size: 12px; }"
  , "box.u-insights label.chip { padding: 1px 8px; border-radius: 10px; font-size: 12px; }"
  , "box.u-insights label.chip.ref { background: var(--accent-bg); color: var(--accent); }"
  , "box.u-insights label.chip.tag { background: var(--warning-bg); color: var(--warning); }"
  , "box.u-insights box.chosen { padding: 14px; }"
  , "box.u-insights box.chosen label.subject { font-size: 16px; font-weight: 600; }"
  , "box.u-insights label.badge.merge { padding: 1px 8px; border-radius: 10px; background: var(--merged-bg); color: var(--merged); font-size: 12px; }"
  , "box.u-insights label.section { margin-top: 8px; font-size: 11px; font-weight: 600; color: var(--text-muted); }"
  ]
