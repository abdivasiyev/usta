{-# LANGUAGE OverloadedLabels #-}

{- | Scrollers inside a scrolling page.

GTK gives a whole trackpad swipe to the scroller it starts on. A code
block or table that only scrolls sideways takes even an almost upright
swipe that way, and the page stops under the pointer. 'sideways' keeps
an upright swipe for the page, with the glide GTK gives the page after.
-}
module Usta.UI.Scroll
    ( sideways
    )
where

import Control.Monad (void, when)
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import System.IO.Unsafe (unsafePerformIO)

import Data.GI.Base qualified as GI
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk

{- | GTK's own: how far a trackpad's unit moves a page, and how fast a
glide slows.
-}
factor, friction :: Double
factor = 2.5
friction = 4

-- | An upright swipe on @inner@ scrolls the page around it instead.
sideways :: Gtk.ScrolledWindow -> IO ()
sideways inner = do
    swipe <-
        GI.new
            Gtk.EventControllerScroll
            [ #flags := [Gtk.EventControllerScrollFlagsBothAxes, Gtk.EventControllerScrollFlagsKinetic]
            , #propagationPhase := Gtk.PropagationPhaseCapture
            ]
    -- Whether this swipe is upright, from its first move.
    upright <- newIORef Nothing
    void $ GI.on swipe #scrollBegin $ do
        writeIORef upright Nothing
        modifyIORef' swipes (+ 1)
    void $ GI.on swipe #scroll $ \dx dy -> do
        unit <- Gtk.eventControllerScrollGetUnit swipe
        -- A mouse wheel has no sideways part, and GTK passes it on already.
        if unit /= Gdk.ScrollUnitSurface
            then pure False
            else do
                up <- readIORef upright >>= maybe (let u = abs dy > abs dx in writeIORef upright (Just u) >> pure u) pure
                if not up
                    then pure False
                    else
                        pageOf inner >>= \case
                            Nothing -> pure False
                            Just (_, adjustment) -> do
                                value <- Gtk.adjustmentGetValue adjustment
                                Gtk.adjustmentSetValue adjustment (value + dy * factor)
                                pure True
    void $ GI.on swipe #decelerate $ \_ velocity -> do
        up <- readIORef upright
        when (up == Just True) $ pageOf inner >>= mapM_ (\(page, adjustment) -> glide page adjustment (velocity * factor))
    Gtk.widgetAddController inner swipe

-- | Every swipe counts, so a new one stops a glide.
swipes :: IORef Int
swipes = unsafePerformIO (newIORef 0)
{-# NOINLINE swipes #-}

-- | The scroller around @w@ that goes up and down, and its adjustment.
pageOf :: Gtk.IsWidget w => w -> IO (Maybe (Gtk.ScrolledWindow, Gtk.Adjustment))
pageOf w =
    Gtk.widgetGetParent w >>= \case
        Nothing -> pure Nothing
        Just parent ->
            GI.castTo Gtk.ScrolledWindow parent >>= \case
                Just s -> do
                    (_, vertical) <- Gtk.scrolledWindowGetPolicy s
                    if vertical == Gtk.PolicyTypeNever
                        then pageOf parent
                        else Just . (,) s <$> Gtk.scrolledWindowGetVadjustment s
                Nothing -> pageOf parent

{- | Slows from @velocity@ as GTK's own glide does; stops at an end, at a
new swipe, or when something else moves the page.
-}
glide :: Gtk.ScrolledWindow -> Gtk.Adjustment -> Double -> IO ()
glide page adjustment velocity = do
    mine <- readIORef swipes
    from <- Gtk.adjustmentGetValue adjustment
    started <- newIORef (Nothing :: Maybe Int64)
    placed <- newIORef from
    void $ Gtk.widgetAddTickCallback page $ \_ clock -> do
        now <- Gdk.frameClockGetFrameTime clock
        t0 <- readIORef started >>= maybe (writeIORef started (Just now) >> pure now) pure
        current <- readIORef swipes
        value <- Gtk.adjustmentGetValue adjustment
        expected <- readIORef placed
        if current /= mine || abs (value - expected) > 0.5
            then pure False
            else do
                lower <- Gtk.adjustmentGetLower adjustment
                upper <- (-) <$> Gtk.adjustmentGetUpper adjustment <*> Gtk.adjustmentGetPageSize adjustment
                let
                    t = fromIntegral (now - t0) / 1e6
                    wanted = from + velocity / friction * (1 - exp (-friction * t))
                    at = max lower (min upper wanted)
                    moving = abs (velocity * exp (-friction * t)) > 5
                Gtk.adjustmentSetValue adjustment at
                writeIORef placed at
                pure (moving && at == wanted)
