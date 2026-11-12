{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A panel beside a page's main column that folds to a thin rail and
resizes by dragging its inner edge, as a pull request's file list and
review panel do. Dragged narrower than its least it folds; a
double-click on the edge puts the width back.
-}
module Usta.UI.Components.SidePanel
    ( Edge (..)
    , Side (..)
    , Config (..)
    , Panel (..)
    , newSide
    , new
    , css
    )
where

import Control.Monad (void, when)
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)

import Data.GI.Base qualified as GI
import GI.Graphene qualified as Graphene
import GI.Gtk qualified as Gtk

import Usta.UI.Components.Button qualified as Button

-- | Which side of the main column the panel is on.
type Edge :: Type
data Edge = OnLeft | OnRight
    deriving stock (Eq, Show)

{- | A panel as the user left it; shared by every page that shows it, so
the choice is kept from one pull request to the next. 'Nothing' until
they fold or open it, so a small window starts it folded.
-}
type Side :: Type
data Side = Side
    { width :: Int32
    , folded :: Maybe Bool
    }

newSide :: Int32 -> IO (IORef Side)
newSide w = newIORef Side {width = w, folded = Nothing}

type Config :: Type
data Config = Config
    { edge :: Edge
    , side :: IORef Side
    , defaultWidth :: Int32
    , least :: Int32
    -- ^ Narrower than this, a dragged panel folds.
    , foldsUnder :: Int32
    -- ^ A window narrower than this starts the panel folded.
    , title :: Text
    -- ^ For the fold button's tooltip, e.g. "file list".
    }

-- | The panel's widget, its rail's box to put counts in, and folding.
type Panel :: Type
data Panel = Panel
    { widget :: Gtk.Box
    , rail :: Gtk.Box
    , fold :: Gtk.Button
    -- ^ Folds the open panel; for its header.
    , toggle :: IO ()
    }

-- | @content@ is what the panel shows open.
new :: Config -> Gtk.Widget -> IO Panel
new c content = do
    outer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #hexpand := False, #cssClasses := ["u-side-panel"]]
    start <- readIORef c.side
    scroll <-
        GI.new
            Gtk.ScrolledWindow
            [ #child := content
            , #hscrollbarPolicy := Gtk.PolicyTypeNever
            , #widthRequest := start.width
            , #propagateNaturalWidth := False
            , #hexpand := False
            , #vexpand := True
            ]
    edgeBox <- GI.new Gtk.Box [#widthRequest := 7, #cssClasses := ["edge"], #tooltipText := "Drag to resize; double-click to reset"]
    Gtk.widgetSetCursorFromName edgeBox (Just "col-resize")
    line <- GI.new Gtk.Box [#widthRequest := 1, #hexpand := True, #halign := Gtk.AlignCenter, #cssClasses := ["line"]]
    #append edgeBox line
    open <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    if c.edge == OnLeft
        then #append open scroll >> #append open edgeBox
        else #append open edgeBox >> #append open scroll

    railBox <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #spacing := 10
            , #widthRequest := 44
            , #cssClasses := ["rail", if c.edge == OnLeft then "left" else "right"]
            ]
    unfold <-
        Button.new
            Button.defaults
                { Button.label = Just (if c.edge == OnLeft then "»" else "«")
                , Button.small = True
                , Button.tooltip = Just ("Open the " <> c.title)
                }
    GI.set unfold [#halign := Gtk.AlignCenter]
    Gtk.widgetAddCssClass unfold "fold"
    #append railBox unfold
    counts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #halign := Gtk.AlignCenter]
    #append railBox counts
    -- The whole rail opens the panel, not only its button.
    railClick <- Gtk.gestureClickNew
    #append outer open
    #append outer railBox

    let
        show' isFolded = do
            GI.set open [#visible := not isFolded]
            GI.set railBox [#visible := isFolded]
        setFolded isFolded = do
            modifyIORef' c.side (\s -> s {folded = Just isFolded})
            show' isFolded
        setWidth w = do
            modifyIORef' c.side (\s -> s {width = w})
            GI.set scroll [#widthRequest := w]
    -- Folded or open as last chosen; until then by the window's width.
    show' (start.folded == Just True)
    void $ GI.on outer #map $ do
        s <- readIORef c.side
        when (s.folded == Nothing) $ do
            root <- Gtk.widgetGetRoot outer
            width' <- maybe (pure 0) (\r -> GI.castTo Gtk.Widget r >>= maybe (pure 0) Gtk.widgetGetWidth) root
            show' (width' > 0 && width' < c.foldsUnder)
    void $ GI.on unfold #clicked (setFolded False)
    foldButton <-
        Button.new
            Button.defaults
                { Button.label = Just (if c.edge == OnLeft then "«" else "»")
                , Button.small = True
                , Button.tooltip = Just ("Fold the " <> c.title)
                }
    GI.set foldButton [#valign := Gtk.AlignCenter]
    Gtk.widgetAddCssClass foldButton "u-fold"
    void $ GI.on foldButton #clicked (setFolded True)
    void $ GI.on railClick #released $ \_ _ _ -> setFolded False
    Gtk.widgetAddController railBox railClick

    -- Dragging the edge: wider away from the main column, narrower toward it.
    -- The pointer is measured against the window, which stays put; the
    -- edge itself moves as the panel resizes, so offsets from it would
    -- shake the width back and forth.
    drag <- Gtk.gestureDragNew
    startWidth <- newIORef start.width
    startX <- newIORef 0
    let
        windowX x =
            Gtk.widgetGetRoot edgeBox >>= \case
                Nothing -> pure x
                Just root -> do
                    to <- Gtk.toWidget root
                    point <- Graphene.newZeroPoint >>= \p -> Graphene.pointInit p (realToFrac x) 0
                    (ok, at) <- Gtk.widgetComputePoint edgeBox to point
                    if ok then realToFrac <$> Graphene.getPointX at else pure x
    void $ GI.on drag #dragBegin $ \x _ -> do
        s <- readIORef c.side
        writeIORef startWidth s.width
        windowX x >>= writeIORef startX
        Gtk.widgetAddCssClass edgeBox "dragging"
    void $ GI.on drag #dragUpdate $ \dx _ -> do
        (_, began, _) <- Gtk.gestureDragGetStartPoint ?self
        x0 <- readIORef startX
        x <- windowX (began + dx)
        w0 <- readIORef startWidth
        let
            moved = x - x0
            w = w0 + round (if c.edge == OnLeft then moved else negate moved)
        if w < c.least
            then setFolded True >> setWidth c.defaultWidth >> void (Gtk.gestureSetState ?self Gtk.EventSequenceStateDenied)
            else setWidth w
    void $ GI.on drag #dragEnd $ \_ _ -> Gtk.widgetRemoveCssClass edgeBox "dragging"
    Gtk.widgetAddController edgeBox drag
    twice <- Gtk.gestureClickNew
    void $ GI.on twice #pressed $ \n _ _ -> when (n == 2) (setWidth c.defaultWidth)
    Gtk.widgetAddController edgeBox twice

    pure
        Panel
            { widget = outer
            , rail = counts
            , fold = foldButton
            , toggle = do
                visible <- GI.get open #visible
                setFolded visible
            }

css :: Text
css =
    mconcat
        [ "box.u-side-panel box.edge { background: transparent; }"
        , "box.u-side-panel box.edge box.line { background: var(--border); }"
        , "box.u-side-panel box.edge:hover box.line, box.u-side-panel box.edge.dragging box.line { background: var(--accent); min-width: 2px; }"
        , "box.u-side-panel box.rail { padding: 10px 0; background: var(--surface); }"
        , "box.u-side-panel box.rail.left { border-right: 1px solid var(--border); }"
        , "box.u-side-panel box.rail.right { border-left: 1px solid var(--border); }"
        , "button.u-button.small.fold, button.u-button.small.u-fold { min-width: 28px; padding: 0; }"
        , "box.u-side-panel box.rail label.count { font-size: 12px; color: var(--text-muted); }"
        ]
