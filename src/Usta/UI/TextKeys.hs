{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The Emacs keys every text field on macOS has, which GTK leaves out:
Control with a, e, f, b, n, p moves; d, h, k delete; y puts back what
k took; o opens a line. Each window gets them as it opens.
-}
module Usta.UI.TextKeys
    ( everywhere
    )
where

import Control.Monad (forM_, unless, void, when)
import Data.Char (chr, toLower)
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import Usta.UI.Platform qualified as Platform

-- | Every window, now and later, on macOS; elsewhere GTK's keys stay.
everywhere :: IO ()
everywhere = when (Platform.current == Platform.MacOS) $ do
    killed <- newIORef ""
    windows <- Gtk.windowGetToplevels
    let
        each = do
            n <- Gio.listModelGetNItems windows
            forM_ (takeWhile (< n) [0 ..]) $ \i ->
                Gio.listModelGetItem windows i >>= mapM_ (\o -> GI.castTo Gtk.Window o >>= mapM_ (install killed))
    each
    void $ GI.on windows #itemsChanged $ \_ _ _ -> each

-- | Once per window: a class marks it.
install :: IORef Text -> Gtk.Window -> IO ()
install killed window = do
    done <- Gtk.widgetHasCssClass window "u-text-keys"
    unless done $ do
        Gtk.widgetAddCssClass window "u-text-keys"
        keys <- GI.new Gtk.EventControllerKey [#propagationPhase := Gtk.PropagationPhaseCapture]
        void $ GI.on keys #keyPressed $ \keyval _ state ->
            if not (controlOnly state)
                then pure False
                else do
                    key <- toLower . chr . fromIntegral <$> Gdk.keyvalToUnicode keyval
                    focus <- Gtk.rootGetFocus window
                    text <- maybe (pure Nothing) (GI.castTo Gtk.Text) focus
                    view <- maybe (pure Nothing) (GI.castTo Gtk.TextView) focus
                    case (text, view) of
                        (Just t, _) -> inLine killed t key
                        (_, Just v) -> inArea killed v key
                        _ -> pure False
        Gtk.widgetAddController window keys

-- | Control and nothing else that changes a key.
controlOnly :: [Gdk.ModifierType] -> Bool
controlOnly state =
    Gdk.ModifierTypeControlMask `elem` state
        && not (any (`elem` state) [Gdk.ModifierTypeShiftMask, Gdk.ModifierTypeMetaMask, Gdk.ModifierTypeAltMask, Gdk.ModifierTypeSuperMask])

-- | A one-line field: the keys that make sense on one line.
inLine :: IORef Text -> Gtk.Text -> Char -> IO Bool
inLine killed t key = do
    at <- Gtk.editableGetPosition t
    whole <- Gtk.editableGetText t
    editable <- Gtk.editableGetEditable t
    let
        size = fromIntegral (T.length whole)
        go p = Gtk.editableSetPosition t (max 0 (min size p)) >> pure True
        cut from to = when editable (Gtk.editableDeleteText t from to) >> pure True
    case key of
        'a' -> go 0
        'e' -> go size
        'f' -> go (at + 1)
        'b' -> go (at - 1)
        'd' -> cut at (min size (at + 1))
        'h' -> cut (max 0 (at - 1)) at
        'k' -> do
            writeIORef killed (T.drop (fromIntegral at) whole)
            cut at size
        'y' -> do
            back <- readIORef killed
            when editable $ do
                after <- Gtk.editableInsertText t back (-1) at
                Gtk.editableSetPosition t after
            pure True
        _ -> pure False

-- | A text area, by its lines.
inArea :: IORef Text -> Gtk.TextView -> Char -> IO Bool
inArea killed v key = do
    buffer <- Gtk.textViewGetBuffer v
    cursor <- Gtk.textBufferGetInsert buffer
    iter <- Gtk.textBufferGetIterAtMark buffer cursor
    editable <- Gtk.textViewGetEditable v
    let
        place i = do
            Gtk.textBufferPlaceCursor buffer i
            Gtk.textViewScrollMarkOnscreen v cursor
            pure True
        move step = step iter >> place iter
        -- The same spot a line down or up, as the arrows go.
        line down = do
            spot <- Gtk.textViewGetIterLocation v iter
            x <- Gdk.getRectangleX spot
            y <- Gdk.getRectangleY spot
            h <- Gdk.getRectangleHeight spot
            -- The nearest place between letters, not the letter under x.
            (_, there, trailing) <- Gtk.textViewGetIterAtPosition v x (if down then y + h + 1 else y - 1)
            void (Gtk.textIterForwardChars there trailing)
            if not down && y <= 0 then pure True else place there
        cut forward = do
            other <- Gtk.textIterCopy iter
            void (forward other)
            when editable $ void (Gtk.textBufferDeleteInteractive buffer iter other True)
            pure True
    case key of
        'a' -> move (`Gtk.textIterSetLineOffset` 0)
        'e' -> move (\i -> Gtk.textIterEndsLine i >>= \at -> unless at (void (Gtk.textIterForwardToLineEnd i)))
        'f' -> move (void . Gtk.textIterForwardCursorPosition)
        'b' -> move (void . Gtk.textIterBackwardCursorPosition)
        'n' -> line True
        'p' -> line False
        'd' -> cut Gtk.textIterForwardCursorPosition
        'h' -> cut Gtk.textIterBackwardCursorPosition
        'k' -> do
            end <- Gtk.textIterCopy iter
            atEnd <- Gtk.textIterEndsLine end
            void (if atEnd then Gtk.textIterForwardChar end else Gtk.textIterForwardToLineEnd end)
            Gtk.textBufferGetText buffer iter end False >>= writeIORef killed
            when editable $ void (Gtk.textBufferDeleteInteractive buffer iter end True)
            pure True
        'y' -> do
            back <- readIORef killed
            when editable $ void (Gtk.textBufferInsertInteractiveAtCursor buffer back (-1) True)
            pure True
        'o' -> do
            when editable $ do
                void (Gtk.textBufferInsertInteractiveAtCursor buffer "\n" (-1) True)
                Gtk.textBufferGetIterAtMark buffer cursor >>= \i -> Gtk.textIterBackwardChar i >> Gtk.textBufferPlaceCursor buffer i
            pure True
        _ -> pure False
