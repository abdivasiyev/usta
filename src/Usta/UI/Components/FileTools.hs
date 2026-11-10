{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The Files tab's tools for one file: its blame, each line with the
commit that last changed it; its history, each commit showing what it
did to the file; and the bar that finds text in the code.
-}
module Usta.UI.Components.FileTools
    ( blameView
    , historyView
    , findBar
    , css
    )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.Function (on)
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (groupBy)
import Data.Text (Text)
import Data.Time (diffUTCTime, nominalDay)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Usta.Types.Commit (Commit (..))
import Usta.Types.Insights (Blamed (..))
import Usta.Types.Parse (unparse)
import Usta.UI.Components.CommandPalette (shortcut)
import Usta.UI.Format (Clock (..), ago)

import Usta.UI.Components.Button qualified as Button
import Usta.UI.Components.Diff qualified as Diff
import Usta.UI.Scroll qualified as Scroll

-- | The blame, loaded when first shown; @load@ runs off the main thread.
blameView :: Clock -> IO (Either Text [Blamed]) -> IO Gtk.Widget
blameView c load = loading load $ \lines' -> do
    -- A grid, so the commit, number and code columns line up.
    grid <- GI.new Gtk.Grid [#cssClasses := ["u-blame"]]
    let
        numbered = zip [1 :: Int ..] (take 4000 lines')
        place :: Gtk.IsWidget w => w -> Int32 -> Int -> IO ()
        place w col row = Gtk.toWidget w >>= \x -> Gtk.gridAttach grid x col (fromIntegral row) 1 1
    forM_ (groupBy ((==) `on` ((.sha) . snd)) numbered) $ \chunk ->
        forM_ (zip [0 :: Int ..] chunk) $ \(k, (n, b)) -> do
            let
                first = ["first" | k == 0]
            heat <- GI.new Gtk.Box [#widthRequest := 3, #cssClasses := ["heat", heatOf c b] <> first]
            place heat 0 n
            who <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #cssClasses := ["who"] <> first, #tooltipText := b.summary]
            when (k == 0) $ do
                label (T.take 7 b.sha) ["mono", "hash"] >>= #append who
                label b.author ["strong"] >>= #append who
                label (ago c b.at) ["muted"] >>= #append who
                s <- label b.summary ["muted"]
                GI.set s [#ellipsize := Pango.EllipsizeModeEnd, #maxWidthChars := 28]
                #append who s
            place who 1 n
            num <- label (T.pack (show n)) (["mono", "number"] <> first)
            GI.set num [#xalign := 1, #marginStart := 4, #marginEnd := 12]
            place num 2 n
            code <- label (if T.null b.line then " " else b.line) (["mono", "code"] <> first)
            GI.set code [#selectable := True, #hexpand := True]
            place code 3 n
    scroll <- GI.new Gtk.ScrolledWindow [#child := grid, #vscrollbarPolicy := Gtk.PolicyTypeNever, #propagateNaturalHeight := True]
    Scroll.sideways scroll
    Gtk.toWidget scroll
    where
        -- Newer lines are warmer, as forges draw them.
        heatOf clock b
            | age < 7 = "week"
            | age < 30 = "month"
            | age < 365 = "year"
            | otherwise = "old"
            where
                age = diffUTCTime clock.now b.at / nominalDay

{- | The commits that changed the file; the chosen one shows its change
to it below the list. @changeOf@ runs off the main thread.
-}
historyView :: Clock -> IO (Either Text [Commit]) -> (Commit -> IO (Either Text [Diff.File])) -> IO Gtk.Widget
historyView c load changeOf = loading load $ \commits -> do
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #cssClasses := ["u-file-history"]]
    list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle, #cssClasses := ["commits"]]
    forM_ commits $ \x -> do
        row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
        body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #hexpand := True]
        s <- label x.subject ["strong"]
        GI.set s [#ellipsize := Pango.EllipsizeModeEnd]
        #append body s
        label (x.author <> " · " <> ago c x.authoredAt) ["muted", "small"] >>= #append body
        #append row body
        label (T.take 7 (unparse x.sha)) ["mono", "hash"] >>= #append row
        #append list row
    when (null commits) $ label "No commit changed this file on the branch." ["muted"] >>= #append box
    #append box list
    shown <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
    #append box shown
    void $ GI.on list #rowSelected $ \row -> forM_ row $ \r -> do
        i <- fromIntegral <$> Gtk.listBoxRowGetIndex r
        when (i < length commits) $ do
            clear shown
            label "Loading the change…" ["muted"] >>= #append shown
            void $ forkIO $ do
                result <- try (changeOf (commits !! i))
                void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    clear shown
                    case result of
                        Right (Right fs) | not (null fs) -> forM_ fs (\f -> Diff.plain f >>= #append shown)
                        Right (Right _) -> label "The change is not in this commit's diff." ["muted"] >>= #append shown
                        Right (Left problem) -> label problem ["muted"] >>= #append shown
                        Left (e :: SomeException) -> label (T.pack (show e)) ["muted"] >>= #append shown
                    pure False
    Gtk.toWidget box

{- | A bar that finds text in @view@: the matches counted, ↑ and ↓ or Enter
go between them, Esc or × closes it. 'snd' shows it with the cursor in.
-}
findBar :: Gtk.TextView -> IO (Gtk.Box, IO ())
findBar view = do
    bar <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationHorizontal
            , #spacing := 6
            , #visible := False
            , #halign := Gtk.AlignEnd
            , #valign := Gtk.AlignStart
            , #marginTop := 8
            , #marginEnd := 12
            , #cssClasses := ["u-find-bar"]
            ]
    entry <- GI.new Gtk.SearchEntry [#placeholderText := "Find in file", #widthRequest := 200]
    count <- label "" ["muted", "small"]
    up <- Button.new Button.defaults {Button.label = Just "↑", Button.variant = Button.Flat, Button.small = True, Button.tooltip = Just "Previous (Shift Enter)"}
    down <- Button.new Button.defaults {Button.label = Just "↓", Button.variant = Button.Flat, Button.small = True, Button.tooltip = Just "Next (Enter)"}
    close <- Button.new Button.defaults {Button.label = Just "×", Button.variant = Button.Flat, Button.small = True, Button.tooltip = Just "Close (Esc)"}
    mapM_ (#append bar) =<< sequence [Gtk.toWidget entry, Gtk.toWidget count, Gtk.toWidget up, Gtk.toWidget down, Gtk.toWidget close]
    buffer <- Gtk.textViewGetBuffer view
    at <- newIORef (0 :: Int)
    let
        matches q = do
            start <- Gtk.textBufferGetStartIter buffer
            let
                go it acc = do
                    found <- Gtk.textIterForwardSearch it q [Gtk.TextSearchFlagsCaseInsensitive] Nothing
                    case found of
                        (True, s, e) -> do
                            next <- Gtk.textIterCopy e
                            go next ((s, e) : acc)
                        _ -> pure (reverse acc)
            if T.null q then pure [] else go start []
        jump by = do
            q <- Gtk.editableGetText entry
            ms <- matches q
            if null ms
                then GI.set count [#label := if T.null q then "" else "No match"]
                else do
                    i <- readIORef at
                    let
                        j = (i + by) `mod` length ms
                        (s, e) = ms !! j
                    writeIORef at j
                    Gtk.textBufferSelectRange buffer s e
                    void $ Gtk.textViewScrollToIter view s 0.1 False 0 0
                    GI.set count [#label := T.pack (show (j + 1)) <> " of " <> T.pack (show (length ms))]
        hide = GI.set bar [#visible := False]
    void $ GI.on entry #searchChanged (writeIORef at 0 >> jump 0)
    void $ GI.on entry #activate (jump 1)
    void $ GI.on up #clicked (jump (-1))
    void $ GI.on down #clicked (jump 1)
    void $ GI.on close #clicked hide
    keys <- GI.new Gtk.ShortcutController []
    shortcut keys "Escape" hide
    shortcut keys "<Shift>Return" (jump (-1))
    Gtk.widgetAddController bar keys
    pure (bar, GI.set bar [#visible := True] >> void (Gtk.widgetGrabFocus entry))

-- | A slot that loads, then shows what @load@ gave.
loading :: IO (Either Text a) -> (a -> IO Gtk.Widget) -> IO Gtk.Widget
loading load render = do
    slot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
    label "Loading…" ["muted", "pad"] >>= #append slot
    void $ forkIO $ do
        result <- try load
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            clear slot
            case result of
                Right (Right a) -> render a >>= #append slot
                Right (Left problem) -> label problem ["muted", "pad"] >>= \l -> GI.set l [#wrap := True] >> #append slot l
                Left (e :: SomeException) -> label (T.pack (show e)) ["muted", "pad"] >>= #append slot
            pure False
    Gtk.toWidget slot

clear :: Gtk.Box -> IO ()
clear box = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear box)

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css =
    mconcat
        [ "grid.u-blame { background: var(--surface); font-size: 13px; }"
        , "grid.u-blame .first { border-top: 1px solid var(--border-subtle); }"
        , "grid.u-blame box.who { padding: 0 12px 0 8px; }"
        , "grid.u-blame label { line-height: 1.4; }"
        , "grid.u-blame label.mono { font-family: 'JetBrains Mono', monospace; }"
        , "grid.u-blame label.number { padding: 0 10px 0 4px; color: var(--text-muted); }"
        , "grid.u-blame label.code { padding-right: 16px; }"
        , "grid.u-blame label.hash, box.u-file-history label.hash { color: var(--accent); font-family: 'JetBrains Mono', monospace; font-size: 12px; }"
        , "grid.u-blame label.strong, box.u-file-history label.strong { font-weight: 600; }"
        , "grid.u-blame box.heat.week { background: #E5763A; }"
        , "grid.u-blame box.heat.month { background: #F2C25C; }"
        , "grid.u-blame box.heat.year { background: #9FB7EC; }"
        , "grid.u-blame box.heat.old { background: var(--border); }"
        , "box.u-file-history { padding: 12px; }"
        , "box.u-file-history list.commits { background: transparent; }"
        , "box.u-file-history list.commits row { padding: 8px 10px; border-bottom: 1px solid var(--border-subtle); }"
        , "box.u-file-history list.commits row:selected { background: var(--accent-bg); }"
        , "box.u-file-history label.small { font-size: 12px; }"
        , "box.u-find-bar { padding: 4px 6px; border: 1px solid var(--input-border); border-radius: 8px; background: var(--surface); box-shadow: 0 4px 12px rgba(27,29,33,0.12); }"
        , "label.pad { padding: 16px; }"
        ]
