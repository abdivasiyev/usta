{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The branch picker as the design draws it: a button with the branch
shown, opening Branches and Tags, a box to find one, the default branch
first, then the recently pushed and all the others, each with how far
it is from the default one when that is known.
-}
module Usta.UI.Components.BranchPicker
    ( Picker (..)
    , new
    , css
    )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Text (Text)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Usta.Types.Branch (Branch (..), Tag (..))
import Usta.UI.Format (Clock, ago)

import Usta.UI.Components.Icon qualified as Icon
import Usta.UI.Components.Segmented qualified as Segmented

type Picker :: Type
data Picker = Picker
    { shown :: Text
    -- ^ The branch or tag the page shows.
    , defaultBranch :: Text
    , clock :: Clock
    , branches :: IO (Either Text [Branch])
    , tags :: Maybe (IO (Either Text [Tag]))
    {- ^ Both run off the main thread, the first time the picker opens;
    without tags the picker has no Tags tab.
    -}
    , choose :: Text -> IO ()
    , note :: Text
    -- ^ Under the list, e.g. what follows the branch.
    }

new :: Picker -> IO Gtk.MenuButton
new p = do
    caption <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    Icon.new 14 Icon.Branch >>= #append caption
    GI.new Gtk.Label [#label := p.shown, #cssClasses := ["mono"]] >>= #append caption
    content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6, #widthRequest := 340, #cssClasses := ["content"]]
    popover <- GI.new Gtk.Popover [#child := content, #hasArrow := False, #cssClasses := ["u-branch-picker"]]
    button <- GI.new Gtk.MenuButton [#child := caption, #popover := popover, #tooltipText := "Choose a branch (b)", #cssClasses := ["u-branch-button"]]

    (kinds, kindButtons) <- Segmented.new ["Branches", "Tags"] 0
    GI.set kinds [#halign := Gtk.AlignStart, #visible := isJust p.tags]
    #append content kinds
    find <- GI.new Gtk.SearchEntry [#placeholderText := "Find a branch"]
    #append content find
    list <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2]
    scroll <- GI.new Gtk.ScrolledWindow [#child := list, #hscrollbarPolicy := Gtk.PolicyTypeNever, #propagateNaturalHeight := True, #maxContentHeight := 380]
    #append content scroll
    footer <- GI.new Gtk.Label [#label := p.note, #xalign := 0, #wrap := True, #cssClasses := ["footnote"]]
    #append content footer

    loaded <- newIORef Nothing
    showingTags <- newIORef False
    let
        clear = Gtk.widgetGetFirstChild list >>= mapM_ (\w -> Gtk.boxRemove list w >> clear)
        heading text = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := ["heading"]] >>= #append list
        say text = GI.new Gtk.Label [#label := text, #xalign := 0, #wrap := True, #cssClasses := ["muted"]] >>= #append list
        row name meta isDefault = do
            line <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
            GI.new Gtk.Label [#label := if name == p.shown then "✓" else "", #widthRequest := 14, #cssClasses := ["tick"]] >>= #append line
            n <- GI.new Gtk.Label [#label := name, #xalign := 0, #hexpand := True, #ellipsize := Pango.EllipsizeModeMiddle, #cssClasses := ["mono"]]
            #append line n
            when isDefault $ GI.new Gtk.Label [#label := "default", #cssClasses := ["pill"]] >>= #append line
            GI.new Gtk.Label [#label := meta, #cssClasses := ["muted", "small"]] >>= #append line
            b <- GI.new Gtk.Button [#child := line, #cssClasses := ["item"] <> ["current" | name == p.shown]]
            void $ GI.on b #clicked (Gtk.popoverPopdown popover >> p.choose name)
            #append list b
        metaOf br =
            T.intercalate
                " · "
                ( catMaybes
                    [ (\n -> T.pack (show n) <> " ahead") <$> nonZero br.ahead
                    , (\n -> T.pack (show n) <> " behind") <$> nonZero br.behind
                    ]
                )
                `orElse` maybe "" (ago p.clock) br.pushed
        draw = do
            clear
            q <- T.toLower <$> Gtk.editableGetText find
            tagsOn <- readIORef showingTags
            GI.set find [#placeholderText := if tagsOn then "Find a tag" else "Find a branch"]
            readIORef loaded >>= \case
                Nothing -> say "Loading…"
                Just (bs, ts) ->
                    if tagsOn
                        then case ts of
                            Left problem -> say problem
                            Right [] -> say "No tags yet."
                            Right all' -> forM_ [t | t <- all', q `T.isInfixOf` T.toLower t.name] $ \t ->
                                row t.name (maybe "" (ago p.clock) t.at) False
                        else case bs of
                            Left problem -> say problem
                            Right all' -> do
                                let
                                    matching = [b | b <- all', q `T.isInfixOf` T.toLower b.name]
                                    others = [b | b <- matching, b.name /= p.defaultBranch]
                                    (recent, rest) = splitAt 3 others
                                forM_ [b | b <- matching, b.name == p.defaultBranch] $ \b -> row b.name (maybe "" (ago p.clock) b.pushed) True
                                when (T.null q && not (null recent)) $ heading "Recently pushed"
                                forM_ (if T.null q then recent else []) $ \b -> row b.name (metaOf b) False
                                unless' (null rest && not (T.null q && null recent))
                                    $ heading (if T.null q then "All branches · " <> T.pack (show (length all')) else "Matching · " <> T.pack (show (length others)))
                                forM_ (if T.null q then rest else others) $ \b -> row b.name (metaOf b) False
                                when (null matching) $ say "No branch matches."
    void $ GI.on find #searchChanged draw
    forM_ (zip [False, True] kindButtons) $ \(isTags, kb) -> void $ GI.on kb #toggled $ do
        active <- GI.get kb #active
        when active (writeIORef showingTags isTags >> draw)
    -- Loads once, the first time it opens.
    void $ GI.on popover #show $ do
        draw
        void (Gtk.widgetGrabFocus find)
        already <- readIORef loaded
        when (isNothing already) $ void $ forkIO $ do
            bs <- try p.branches
            ts <- try (maybe (pure (Right [])) id p.tags)
            let
                flat r = either (\(e :: SomeException) -> Left (T.pack (show e))) id r
            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                writeIORef loaded (Just (flat bs, flat ts))
                draw
                pure False
    pure button
    where
        nonZero = \case
            Just n | n > 0 -> Just n
            _ -> Nothing
        orElse a b = if T.null a then b else a
        unless' c act = if c then pure () else act

css :: Text
css =
    mconcat
        [ "menubutton.u-branch-button > button { min-height: 28px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 6px; background: var(--surface); box-shadow: none; background-image: none; }"
        , "popover.u-branch-picker > contents { padding: 8px; border-radius: 12px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: 0 12px 32px rgba(27,29,33,0.16); }"
        , "popover.u-branch-picker button.item { padding: 6px 8px; border: none; border-radius: 6px; box-shadow: none; background: transparent; background-image: none; }"
        , "popover.u-branch-picker button.item:hover { background: var(--border-subtle); }"
        , "popover.u-branch-picker button.item.current { background: var(--accent-bg); }"
        , "popover.u-branch-picker label.tick { color: var(--accent); }"
        , "popover.u-branch-picker label.pill { padding: 1px 8px; border-radius: 10px; background: var(--border-subtle); font-size: 11px; }"
        , "popover.u-branch-picker label.heading { padding: 8px 8px 2px; font-size: 11px; font-weight: 600; color: var(--text-muted); }"
        , "popover.u-branch-picker label.small { font-size: 12px; }"
        , -- Branches and Tags as page tabs: popovers recolor a dark chosen tab's
          -- label to the same dark, so the chosen one here is raised and light.
          "popover.u-branch-picker box.u-segmented { background: var(--sidebar); border-radius: 8px; padding: 3px; margin-bottom: 6px; }"
        , "popover.u-branch-picker box.u-segmented button { min-height: 28px; padding: 0 12px; border: none; border-radius: 6px; outline: none; box-shadow: none; background: transparent; background-image: none; color: var(--text-soft); }"
        , "popover.u-branch-picker box.u-segmented button:hover { background: var(--border-subtle); }"
        , "popover.u-branch-picker box.u-segmented button:checked { background: var(--surface); color: var(--text); font-weight: 600; box-shadow: 0 1px 2px rgba(27,29,33,0.12); }"
        , "popover.u-branch-picker box.u-segmented button label { color: inherit; }"
        , "popover.u-branch-picker label.footnote { padding: 6px 8px 0; border-top: 1px solid var(--border-subtle); font-size: 12px; color: var(--text-muted); }"
        ]
