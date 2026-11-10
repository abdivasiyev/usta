{-# LANGUAGE OverloadedLabels #-}

-- | A page that lists things, and refills in place when they change.
module Usta.UI.Page
    ( Page (..)
    , refill
    )
where

import Control.Monad (forM_)
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)

import Data.GI.Base qualified as GI
import GI.Gtk qualified as Gtk

{- | @update@ shows a new list in place of the old one, keeping the tab,
filter and search the user picked.
-}
type Page :: Type -> Type
data Page a = Page
    { widget :: Gtk.Box
    , update :: [a] -> IO ()
    }

{- | Replaces a list box's rows with one row per item. Only rows go:
'Gtk.listBoxRemoveAll' would drop the placeholder too.
-}
refill :: Gtk.ListBox -> (a -> IO Gtk.Box) -> [a] -> IO ()
refill list row items = do
    let
        removeRows = do
            first <- Gtk.listBoxGetRowAtIndex list 0
            forM_ first $ \r -> Gtk.listBoxRemove list r >> removeRows
    removeRows
    forM_ items $ \item -> do
        content <- row item
        listRow <- GI.new Gtk.ListBoxRow [#activatable := True, #child := content]
        #append list listRow
