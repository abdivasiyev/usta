{- | Letting go of a widget tree that is off screen for good.

A handler written in Haskell that reaches its own widget, or one under
it, keeps that widget alive: GLib holds the handler, the handler holds
the widget. GLib drops a widget's handlers only once it is disposed,
which never comes while they hold it, so a page taken off the window
stayed in memory with all it drew. 'letGo' disposes the tree itself.
-}
module Usta.UI.Gone
    ( letGo
    )
where

import Control.Monad (forM_, when)

import Data.GI.Base qualified as GI
import GI.GObject qualified as GObject
import GI.Gtk qualified as Gtk

{- | Disposes @widget@, which has no parent, and then each widget under it
that its parent let go of, so their handlers and what they hold go. A
widget still in a parent is that parent's to drop.

Popovers are taken off first: one set on a widget with
'Gtk.widgetSetParent' is not the widget's own child, and a text view
disposed with one still on it tries to remove it forever.

Scrollbars are left to their scrolled window: they hold none of usta's
handlers, and one disposed here breaks when GTK disposes it again.
-}
letGo :: Gtk.Widget -> IO ()
letGo widget = do
    scrollbar <- GI.castTo Gtk.Scrollbar widget
    when (null scrollbar) $ do
        parent <- Gtk.widgetGetParent widget
        children <- childrenOf widget
        when (null parent) $ do
            forM_ children $ \c -> GI.castTo Gtk.Popover c >>= mapM_ (const (Gtk.widgetUnparent c))
            GObject.objectRunDispose widget
        forM_ children letGo

childrenOf :: Gtk.Widget -> IO [Gtk.Widget]
childrenOf widget = Gtk.widgetGetFirstChild widget >>= go
    where
        go Nothing = pure []
        go (Just c) = (c :) <$> (Gtk.widgetGetNextSibling c >>= go)
