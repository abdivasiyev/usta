{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A repository's Commits tab: the chosen branch's history by day, newest
first, 50 at a time, with a box that filters by message or author. A
commit opens its diff in place; Back returns to the list where it was.
-}
module Usta.UI.Pages.RepositoryCommits
    ( Actions (..)
    , new
    , css
    )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import Data.Function (on)
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (groupBy)
import Data.Text (Text)
import Data.Time (addDays, localDay, utcToLocalTime)

import Data.GI.Base qualified as GI
import Data.GI.Base.GValue qualified as GValue
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Usta.Types.Commit (Commit (..))
import Usta.Types.Parse (unparse)
import Usta.UI.Components.CommandPalette (shortcut)
import Usta.UI.Format (Clock (..), ago, date)

import Usta.UI.Components.Avatar qualified as Avatar
import Usta.UI.Components.Button qualified as Button
import Usta.UI.Components.Diff qualified as Diff

type Actions :: Type
data Actions = Actions
    { branch :: Text
    , page :: Int -> IO (Either Text [Commit])
    -- ^ The history's page, from 1, 50 commits each; off the main thread.
    , changes :: Commit -> IO (Either Text [Diff.File])
    -- ^ A commit's changes against its first parent; off the main thread.
    , place :: Maybe Avatar.Place
    -- ^ Where the authors' pictures come from.
    }

new :: Clock -> Actions -> IO Gtk.Widget
new c a = do
    stack <- GI.new Gtk.Stack [#vexpand := True, #hexpand := True, #cssClasses := ["u-repo-commits"]]
    page' <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8, #cssClasses := ["history"]]
    top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
    said <- label ("History of " <> a.branch) ["muted", "small"]
    GI.set said [#hexpand := True]
    #append top said
    filterBox <- GI.new Gtk.SearchEntry [#placeholderText := "Filter by message or author", #widthRequest := 260]
    #append top filterBox
    #append page' top
    groups <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
    #append page' groups
    -- The next page comes as the list is scrolled to its end; Try again
    -- shows only when a page failed.
    more <- Button.new Button.defaults {Button.label = Just "Try again", Button.variant = Button.Secondary, Button.small = True}
    GI.set more [#halign := Gtk.AlignCenter, #visible := False]
    #append page' more
    loadingMore <- GI.new Gtk.Label [#label := "Loading more…", #visible := False, #cssClasses := ["muted", "small"]]
    #append page' loadingMore
    listScroll <- GI.new Gtk.ScrolledWindow [#child := page', #hscrollbarPolicy := Gtk.PolicyTypeNever, #vexpand := True]
    void $ Gtk.stackAddNamed stack listScroll (Just "list")
    detail <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12, #cssClasses := ["detail"]]
    detailScroll <- GI.new Gtk.ScrolledWindow [#child := detail, #hscrollbarPolicy := Gtk.PolicyTypeNever, #vexpand := True]
    void $ Gtk.stackAddNamed stack detailScroll (Just "commit")

    loaded <- newIORef []
    nextPage <- newIORef (1 :: Int)
    -- Whether a page is on its way, and whether there is one after.
    busy <- newIORef False
    hasMore <- newIORef True
    selected <- newIORef Nothing
    let
        clear box = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear box)
        dayOf t = localDay (utcToLocalTime c.zone t)
        today = dayOf c.now
        dayName t
            | dayOf t == today = "Today"
            | dayOf t == addDays (-1) today = "Yesterday"
            | otherwise = date c t
        draw = do
            clear groups
            q <- T.toLower <$> Gtk.editableGetText filterBox
            commits <- readIORef loaded
            let
                shown = [x | x <- commits, T.null q || q `T.isInfixOf` T.toLower (x.subject <> " " <> x.author)]
            when (null shown && not (null commits)) $ label "No commit matches." ["muted"] >>= #append groups
            forM_ (groupBy ((==) `on` (dayOf . (.authoredAt))) shown) $ \day -> do
                forM_ (take 1 day) $ \first -> label (dayName first.authoredAt) ["day"] >>= #append groups
                -- One click chooses a commit; a double click or Enter opens it.
                list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle, #activateOnSingleClick := False, #cssClasses := ["u-card", "commits"]]
                forM_ day $ \x -> commitRow c a.place x >>= #append list
                void $ GI.on list #rowActivated $ \row -> do
                    i <- Gtk.listBoxRowGetIndex row
                    when (i >= 0 && fromIntegral i < length day) $ open (day !! fromIntegral i)
                void $ GI.on list #rowSelected $ \row -> forM_ row $ \r -> do
                    i <- Gtk.listBoxRowGetIndex r
                    when (i >= 0 && fromIntegral i < length day) $ writeIORef selected (Just (day !! fromIntegral i))
                #append groups list
        load = do
            n <- readIORef nextPage
            writeIORef busy True
            GI.set more [#visible := False]
            GI.set loadingMore [#visible := n > 1]
            void $ forkIO $ do
                result <- try (a.page n)
                void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    writeIORef busy False
                    GI.set loadingMore [#visible := False]
                    case result of
                        Right (Right commits) -> do
                            modifyIORef' loaded (<> commits)
                            writeIORef nextPage (n + 1)
                            writeIORef hasMore (length commits == 50)
                            draw
                            -- A page that does not fill the view has no end to scroll to.
                            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (fill >> pure False)
                        Right (Left problem) -> failed problem
                        Left (e :: SomeException) -> failed (T.pack (show e))
                    pure False
        failed problem = do
            GI.set more [#visible := True]
            label problem ["muted"] >>= #append groups
        next = do
            waiting <- readIORef busy
            more' <- readIORef hasMore
            when (more' && not waiting) load
        fill = do
            adjustment <- Gtk.scrolledWindowGetVadjustment listScroll
            upper <- Gtk.adjustmentGetUpper adjustment
            shown <- Gtk.adjustmentGetPageSize adjustment
            when (upper <= shown + 1) next
        open x = do
            clear detail
            back <- Button.new Button.defaults {Button.label = Just "← Commits", Button.variant = Button.Flat, Button.small = True}
            GI.set back [#halign := Gtk.AlignStart]
            void $ GI.on back #clicked (Gtk.stackSetVisibleChildName stack "list")
            #append detail back
            head' <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #cssClasses := ["u-card", "commit-head"]]
            subject <- label x.subject ["subject"]
            GI.set subject [#wrap := True, #selectable := True]
            #append head' subject
            unless (T.null x.body) $ label x.body ["body"] >>= \b -> GI.set b [#wrap := True, #selectable := True] >> #append head' b
            label (x.author <> " committed " <> ago c x.authoredAt <> " · " <> unparse x.sha) ["muted", "small", "mono"] >>= \l ->
                GI.set l [#selectable := True] >> #append head' l
            #append detail head'
            files <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12]
            label "Loading the changes…" ["muted"] >>= #append files
            #append detail files
            Gtk.stackSetVisibleChildName stack "commit"
            void $ forkIO $ do
                result <- try (a.changes x)
                void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    clear files
                    case result of
                        Right (Right fs) -> forM_ fs $ \f -> Diff.plain f >>= #append files
                        Right (Left problem) -> label problem ["muted"] >>= #append files
                        Left (e :: SomeException) -> label (T.pack (show e)) ["muted"] >>= #append files
                    pure False
    void $ GI.on filterBox #searchChanged draw
    void $ GI.on more #clicked load
    void $ GI.on listScroll #edgeReached $ \position -> when (position == Gtk.PositionTypeBottom) next

    -- Enter opens the chosen commit, y copies its hash, Esc goes back.
    keys <- GI.new Gtk.ShortcutController []
    shortcut keys "Return" (readIORef selected >>= mapM_ open)
    shortcut keys "y"
        $ readIORef selected
            >>= mapM_
                ( \x -> do
                    clipboard <- Gtk.widgetGetClipboard stack
                    value <- GValue.toGValue (Just (unparse x.sha))
                    provider <- Gdk.contentProviderNewForValue value
                    void (Gdk.clipboardSetContent clipboard (Just provider))
                )
    shortcut keys "Escape" (Gtk.stackSetVisibleChildName stack "list")
    Gtk.widgetAddController stack keys

    label "Loading the history…" ["muted"] >>= #append groups
    load
    Gtk.toWidget stack

commitRow :: Clock -> Maybe Avatar.Place -> Commit -> IO Gtk.Box
commitRow c place x = do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["commit"]]
    -- The picture of the author's account; initials when it is not known.
    case x.login of
        Just who -> Avatar.person 24 place who >>= #append row
        Nothing -> Avatar.new 24 (T.toUpper (T.take 2 x.author)) >>= #append row
    body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #hexpand := True]
    subject <- label x.subject ["subject"]
    GI.set subject [#ellipsize := Pango.EllipsizeModeEnd]
    #append body subject
    label (x.author <> " committed " <> ago c x.authoredAt) ["muted", "small"] >>= #append body
    #append row body
    label (T.take 7 (unparse x.sha)) ["mono", "hash"] >>= #append row
    pure row

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css =
    mconcat
        [ "stack.u-repo-commits box.history, stack.u-repo-commits box.detail { padding: 14px 20px; }"
        , "stack.u-repo-commits label.day { padding: 10px 0 4px; font-size: 12px; font-weight: 600; color: var(--text-muted); }"
        , "stack.u-repo-commits list.commits { background: transparent; border-radius: 8px; }"
        , "stack.u-repo-commits list.commits row { padding: 10px 12px; border-bottom: 1px solid var(--border-subtle); background: var(--surface); }"
        , "stack.u-repo-commits list.commits row:last-child { border-bottom: none; }"
        , "stack.u-repo-commits list.commits row { transition: background 120ms ease-out; }"
        , "stack.u-repo-commits list.commits row:hover { background: var(--window); }"
        , "stack.u-repo-commits list.commits row:selected { background: var(--accent-bg); }"
        , "stack.u-repo-commits label.subject { font-weight: 500; }"
        , "stack.u-repo-commits label.small { font-size: 12px; }"
        , "stack.u-repo-commits label.hash { color: var(--accent); font-size: 12px; }"
        , "stack.u-repo-commits box.commit-head { padding: 14px; }"
        , "stack.u-repo-commits box.commit-head label.subject { font-size: 16px; font-weight: 600; }"
        ]
