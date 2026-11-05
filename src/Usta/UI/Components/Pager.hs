{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A forge's list a page at a time, under a scrolled list: the first
-- page when it starts again, the next one when the list is scrolled to its
-- end (or does not fill its room yet). Under the list it says what is
-- loading, how many are shown of how many, or offers Load more when a
-- page failed.
module Usta.UI.Components.Pager
  ( Pager (..)
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import Usta.Types.Listing (Paged (..))

type Pager :: Type
data Pager = Pager
  { widget :: Gtk.Box
    -- ^ The line under the list.
  , restart :: IO ()
    -- ^ From the first page again, as when the state or the order changes.
  }

-- | @fetch@ asks for the page at a cursor (off the main thread); @got@
-- hears each page, 'True' for a first one; @counted@ says, from the number
-- loaded and the forge's total, what the line shows when nothing loads.
new :: Gtk.ScrolledWindow -> (Maybe Text -> IO (Either Text (Paged a))) -> (Bool -> Paged a -> IO ()) -> (Int -> Maybe Int -> Text) -> IO Pager
new scroll fetch got counted = do
  line <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignCenter, #cssClasses := ["u-pager"]]
  spinner <- GI.new Gtk.Spinner [#spinning := True, #visible := False]
  said <- GI.new Gtk.Label [#label := "", #wrap := True, #cssClasses := ["meta"]]
  again <- Button.new Button.defaults {Button.label = Just "Load more", Button.small = True}
  GI.set again [#visible := False]
  mapM_ (#append line) =<< sequence [Gtk.toWidget spinner, Gtk.toWidget said, Gtk.toWidget again]

  -- Each start counts, so a page of an older start is dropped.
  turn <- newIORef (0 :: Int)
  next <- newIORef Nothing
  loading <- newIORef False
  loaded <- newIORef 0
  total <- newIORef Nothing
  failedAt <- newIORef Nothing
  let settle = do
        n <- readIORef loaded
        t <- readIORef total
        GI.set spinner [#visible := False]
        GI.set again [#visible := False]
        GI.set said [#label := counted n t]
      page first cursor = do
        writeIORef loading True
        mine <- readIORef turn
        GI.set spinner [#visible := True]
        GI.set again [#visible := False]
        GI.set said [#label := if first then "Loading…" else "Loading more…"]
        void $ forkIO $ do
          result <- try (fetch cursor)
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            current <- readIORef turn
            when (current == mine) $ do
              writeIORef loading False
              case result of
                Right (Right p) -> do
                  writeIORef next p.next
                  writeIORef failedAt Nothing
                  when (isJust p.total || first) $ writeIORef total p.total
                  modifyIORef' loaded (+ length p.items)
                  got first p
                  settle
                  -- A page that does not fill the room has no end to scroll to.
                  void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (fill >> pure False)
                Right (Left problem) -> failed cursor problem
                Left (e :: SomeException) -> failed cursor (T.pack (takeWhile (/= '\n') (show e)))
            pure False
      failed cursor problem = do
        writeIORef failedAt (Just cursor)
        GI.set spinner [#visible := False]
        GI.set said [#label := "The forge did not list more: " <> problem]
        GI.set again [#visible := True]
      more = do
        busy <- readIORef loading
        cursor <- readIORef next
        case cursor of
          Just c | not busy -> page False (Just c)
          _ -> pure ()
      fill = do
        adjustment <- Gtk.scrolledWindowGetVadjustment scroll
        upper <- Gtk.adjustmentGetUpper adjustment
        shown <- Gtk.adjustmentGetPageSize adjustment
        when (upper <= shown + 1) more
      restart' = do
        modifyIORef' turn (+ 1)
        writeIORef next Nothing
        writeIORef loaded 0
        writeIORef total Nothing
        page True Nothing
  void $ GI.on scroll #edgeReached $ \position -> when (position == Gtk.PositionTypeBottom) more
  void $ GI.on again #clicked $ readIORef failedAt >>= \case
    Just Nothing -> restart'
    Just cursor -> page False cursor
    Nothing -> more
  pure Pager {widget = line, restart = restart'}

css :: Text
css = "box.u-pager { padding: 10px 0 2px; }"
