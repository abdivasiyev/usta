{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Draws a 'Scene' with GTK: cairo for the shapes, Pango for the text,
-- in the palette of the current theme, and again when it changes.
module Usta.UI.Components.DiagramView
  ( Config (..)
  , new
  , colorOf
  , css
  ) where

import Control.Monad (forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Kind (Type)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Text as T
import qualified GI.Cairo as GICairo
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as PangoCairo
import Numeric (readHex)

import Usta.Diagram.Scene
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Highlight as Highlight

-- | @layout@ gets a Pango measure and gives the scene; it runs once, when
-- the view is made.
--
-- The view zooms and pans inside its own size, so a page around it keeps
-- its scrolling: Ctrl + scroll and pinch zoom around the pointer, dragging
-- pans, and the tools in its corner show on hover or focus.
new :: Config -> (Moves -> Measure IO -> IO Scene) -> IO Gtk.Widget
new cfg layout = do
  area <- GI.new Gtk.DrawingArea [#hexpand := True, #vexpand := cfg.large, #focusable := True]
  let measure font text = do
        l <- Gtk.widgetCreatePangoLayout area (Just text)
        Pango.fontDescriptionFromString (describe font) >>= Pango.layoutSetFontDescription l . Just
        (w', h') <- Pango.layoutGetPixelSize l
        pure Size {w = fromIntegral w', h = fromIntegral h'}
      -- Laid out again whenever a node is dragged.
      arrange drags = do
        laid <- layout drags measure
        -- Text reaches as far as it is wide; the canvas grows (and the
        -- items move right or down) so none of it is cut off.
        corners <- for laid.items $ \item -> case item of
          Text {at = p, text = t, font = f, anchor = a} -> do
            s <- measure f t
            let x0 = case a of
                  Start -> p.x
                  Middle -> p.x - s.w / 2
                  End -> p.x - s.w
            pure [(x0, p.y - s.h / 2), (x0 + s.w, p.y + s.h / 2)]
          _ -> pure (extent item)
        let points = concat corners
            dx = max 0 (negate (minimum (0 : map fst points)) + 4)
            dy = max 0 (negate (minimum (0 : map snd points)) + 4)
            width = max laid.size.w (maximum (0 : map fst points) + dx + 4)
            height = max laid.size.h (maximum (0 : map snd points) + dy + 4)
        pure Scene {size = Size width height, items = map (translate dx dy) laid.items}
  shown <- readIORef cfg.drags >>= arrange >>= newIORef
  let fitContent = unless cfg.large $ readIORef shown >>= \drawn ->
        GI.set area [#contentWidth := ceiling drawn.size.w, #contentHeight := ceiling drawn.size.h]
  -- In a page the view is as large as the scene; large, it takes the room
  -- it is given.
  fitContent
  -- Nothing until the reader zooms or pans: until then the scene sits in
  -- the middle, at its own size or, large, fitted, whatever the size.
  let start = if cfg.large then fitted else actual
  camera <- newIORef Nothing
  pointer <- newIORef Nothing
  let sizeOf = do
        w <- Gtk.widgetGetWidth area
        h <- Gtk.widgetGetHeight area
        pure (fromIntegral w, fromIntegral h)
      current = do
        drawn <- readIORef shown
        readIORef camera >>= maybe (uncurry (start drawn.size) <$> sizeOf) pure
  -- The node under the pointer, or the one being dragged.
  pointed <- newIORef (Nothing :: Maybe Text)
  Gtk.drawingAreaSetDrawFunc area $ Just $ \_ context w h -> do
    palette <- Highlight.currentPalette
    drawn <- readIORef shown
    c <- fromMaybe (start drawn.size (fromIntegral w) (fromIntegral h)) <$> readIORef camera
    renderWithContext (Cairo.translate c.dx c.dy >> Cairo.scale c.zoom c.zoom) context
    forM_ drawn.items $ \item -> paint area palette context item
    marked <- readIORef pointed
    forM_ [i | i@Handle {key = k} <- drawn.items, Just k == marked] $ \i -> do
      let (r, g, b) = colorOf palette (Series 0)
      renderWithContext (do
        Cairo.newPath
        rounded (Point (i.at.x - 2) (i.at.y - 2)) (Size (i.size.w + 4) (i.size.h + 4)) 5
        Cairo.setSourceRGB r g b
        Cairo.setLineWidth 2
        Cairo.stroke) context
  Highlight.follow area (const (Gtk.widgetQueueDraw area))

  overlay <- GI.new Gtk.Overlay [#child := area, #cssClasses := ["u-diagram"]]
  tools <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 2
                          , #halign := Gtk.AlignEnd, #valign := Gtk.AlignStart
                          , #visible := cfg.large, #cssClasses := ["tools"] ]
  Gtk.overlayAddOverlay overlay tools
  let tool child hint = GI.new Gtk.Button [#child := child, #focusOnClick := False, #tooltipText := hint, #cssClasses := ["tool"]]
      glyph t = GI.new Gtk.Label [#label := t] >>= Gtk.toWidget
  zoomOut <- glyph "−" >>= \g -> tool g "Zoom out (Ctrl −)"
  percent <- GI.new Gtk.Button [#label := "100%", #focusOnClick := False, #tooltipText := "Actual size", #cssClasses := ["tool", "percent"]]
  zoomIn <- glyph "+" >>= \g -> tool g "Zoom in (Ctrl +)"
  fitButton <- Icon.new 16 Icon.Fit >>= Gtk.toWidget >>= \g -> tool g "Fit (Ctrl 0)"
  #append tools zoomOut >> #append tools percent >> #append tools zoomIn >> #append tools fitButton
  forM_ cfg.fullscreen $ \open -> do
    button <- Icon.new 16 Icon.Fullscreen >>= Gtk.toWidget >>= \g -> tool g "Fullscreen (F)"
    void $ GI.on button #clicked open
    #append tools button
  -- Shown once a node is moved: puts them all back.
  resetButton <- glyph "↺" >>= \g -> tool g "Reset layout (R)"
  readIORef cfg.drags >>= \d -> GI.set resetButton [#visible := not (Map.null d)]
  Gtk.boxPrepend tools resetButton
  minimap <- if cfg.large then Just <$> mapOf area (readIORef shown) current else pure Nothing
  -- Until the reader zooms, the percentage follows the size the view gets.
  void $ GI.on area #resize $ \_ _ -> do
    untouched <- isNothing <$> readIORef camera
    when untouched $ current >>= \c -> GI.set percent [#label := percentOf c]
  forM_ minimap $ \m -> do
    Gtk.overlayAddOverlay overlay m
    void $ GI.on area #resize $ \_ _ -> Gtk.widgetQueueDraw m

  let place c = do
        writeIORef camera (Just c)
        GI.set percent [#label := percentOf c]
        Gtk.widgetQueueDraw area
        mapM_ Gtk.widgetQueueDraw minimap
      zoomBy factor = do
        (w, h) <- sizeOf
        at <- fromMaybe (w / 2, h / 2) <$> readIORef pointer
        current >>= place . zoomAround factor at
      fit = readIORef shown >>= \drawn -> sizeOf >>= place . uncurry (fitted drawn.size)
      reset = readIORef shown >>= \drawn -> sizeOf >>= place . uncurry (actual drawn.size)
      relayout = do
        readIORef cfg.drags >>= arrange >>= writeIORef shown
        mapM_ Gtk.widgetQueueDraw minimap
      resetLayout = do
        writeIORef cfg.drags Map.empty
        relayout
        fitContent
        writeIORef camera Nothing
        GI.set resetButton [#visible := False]
        current >>= \c -> GI.set percent [#label := percentOf c]
        Gtk.widgetQueueDraw area
  void $ GI.on zoomOut #clicked (zoomBy (1 / step))
  void $ GI.on zoomIn #clicked (zoomBy step)
  void $ GI.on percent #clicked reset
  void $ GI.on fitButton #clicked fit
  void $ GI.on resetButton #clicked resetLayout

  -- The tools show while the pointer is over the diagram or it has focus.
  dragging <- newIORef False
  hovered <- newIORef False
  focused <- newIORef False
  let showTools = unless cfg.large $ do
        on' <- (||) <$> readIORef hovered <*> readIORef focused
        GI.set tools [#visible := on']
  motion <- GI.new Gtk.EventControllerMotion []
  void $ GI.on motion #enter $ \_ _ -> writeIORef hovered True >> showTools
  void $ GI.on motion #leave $ do
    writeIORef hovered False >> writeIORef pointer Nothing >> showTools
    busy <- readIORef dragging
    unless busy $ writeIORef pointed Nothing >> Gtk.widgetQueueDraw area
  -- Nodes that can be dragged show the grab cursor and an outline.
  let handleAt x y = do
        c <- current
        drawn <- readIORef shown
        pure (under drawn ((x - c.dx) / c.zoom) ((y - c.dy) / c.zoom))
  void $ GI.on motion #motion $ \x y -> do
    writeIORef pointer (Just (x, y))
    busy <- readIORef dragging
    unless busy $ do
      found <- handleAt x y
      before <- readIORef pointed
      let key' = (\i -> i.key) <$> found
      when (key' /= before) $ do
        writeIORef pointed key'
        Gtk.widgetSetCursorFromName area (if isNothing key' then Nothing else Just "grab")
        Gtk.widgetQueueDraw area
  Gtk.widgetAddController overlay motion
  focus <- GI.new Gtk.EventControllerFocus []
  void $ GI.on focus #enter $ writeIORef focused True >> showTools
  void $ GI.on focus #leave $ writeIORef focused False >> showTools
  Gtk.widgetAddController overlay focus

  -- Scrolling without Ctrl goes on to the page.
  scroll <- GI.new Gtk.EventControllerScroll [#flags := [Gtk.EventControllerScrollFlagsVertical]]
  void $ GI.on scroll #scroll $ \_ amount -> do
    state <- Gtk.eventControllerGetCurrentEventState scroll
    let zooming = any (`elem` state) [Gdk.ModifierTypeControlMask, Gdk.ModifierTypeMetaMask]
    when zooming $ zoomBy (if amount < 0 then 1.1 else 1 / 1.1)
    pure zooming
  Gtk.widgetAddController area scroll
  pinch <- GI.new Gtk.GestureZoom []
  pinchStart <- newIORef (Camera 1 0 0, (0, 0))
  void $ GI.on pinch #begin $ \_ -> do
    c <- current
    (_, x, y) <- Gtk.gestureGetBoundingBoxCenter pinch
    writeIORef pinchStart (c, (x, y))
  void $ GI.on pinch #scaleChanged $ \scale -> do
    (c, at) <- readIORef pinchStart
    place (zoomAround scale at c)
  Gtk.widgetAddController area pinch
  -- Dragging a node moves it; dragging anywhere else pans.
  drag <- GI.new Gtk.GestureDrag []
  dragStart <- newIORef (Camera 1 0 0)
  grabbed <- newIORef Nothing
  void $ GI.on drag #dragBegin $ \x y -> do
    void (Gtk.widgetGrabFocus area)
    c <- current
    writeIORef dragStart c
    found <- handleAt x y
    drags <- readIORef cfg.drags
    writeIORef grabbed $ (\i -> (i.key, Map.findWithDefault (Point 0 0) i.key drags, onScreen c i)) <$> found
    forM_ found $ \i -> do
      writeIORef dragging True
      writeIORef pointed (Just i.key)
      Gtk.widgetSetCursorFromName area (Just "grabbing")
  void $ GI.on drag #dragUpdate $ \ox oy -> do
    c <- readIORef dragStart
    readIORef grabbed >>= \case
      Nothing -> unless (ox == 0 && oy == 0) $ place c {dx = c.dx + ox, dy = c.dy + oy}
      Just (key, was, (sx, sy)) -> do
        modifyIORef' cfg.drags (Map.insert key (Point (was.x + ox / c.zoom) (was.y + oy / c.zoom)))
        relayout
        GI.set resetButton [#visible := True]
        -- The scene may grow or shrink around the node: the camera keeps
        -- the node under the pointer.
        drawn <- readIORef shown
        case [onScreen (Camera c.zoom 0 0) i | i@Handle {key = k} <- drawn.items, k == key] of
          (nx, ny) : _ -> place c {dx = sx + ox - nx, dy = sy + oy - ny}
          [] -> Gtk.widgetQueueDraw area
  void $ GI.on drag #dragEnd $ \_ _ -> do
    held <- readIORef grabbed
    writeIORef grabbed Nothing
    writeIORef dragging False
    forM_ held $ \_ -> do
      Gtk.widgetSetCursorFromName area (Just "grab")
      -- In a page, the view takes the size the moved diagram needs.
      unless cfg.large $ do
        c <- current
        fitContent
        writeIORef camera (Just c)
        Gtk.widgetQueueDraw area
  Gtk.widgetAddController area drag

  keys <- GI.new Gtk.ShortcutController []
  forM_ ["<Primary>plus", "<Primary>equal", "<Primary>KP_Add"] $ \k -> shortcut keys k (zoomBy step)
  forM_ ["<Primary>minus", "<Primary>KP_Subtract"] $ \k -> shortcut keys k (zoomBy (1 / step))
  shortcut keys "<Primary>0" fit
  shortcut keys "r" resetLayout
  forM_ cfg.fullscreen $ shortcut keys "f"
  Gtk.widgetAddController overlay keys
  Gtk.toWidget overlay

-- | How a view shows its scene.
type Config :: Type
data Config = Config
  { large :: Bool
    -- ^ Filling a window: fitted at first, the tools always shown, with a
    -- map of the whole scene in the corner.
  , fullscreen :: Maybe (IO ())
    -- ^ Shows the diagram large; offered as a tool and the F key.
  , drags :: IORef Moves
    -- ^ Where the reader dragged the nodes; kept by the caller, so a
    -- second view of the diagram shows them there too.
  }

-- | The whole scene small, with the part the view shows outlined.
mapOf :: Gtk.DrawingArea -> IO Scene -> IO Camera -> IO Gtk.Widget
mapOf view scene' current = do
  m <- GI.new Gtk.DrawingArea [ #contentWidth := 140, #contentHeight := 90, #canTarget := False
                              , #halign := Gtk.AlignEnd, #valign := Gtk.AlignEnd, #cssClasses := ["minimap"] ]
  Gtk.drawingAreaSetDrawFunc m $ Just $ \_ context w h -> do
    palette <- Highlight.currentPalette
    whole <- scene'
    c <- current
    vw <- fromIntegral <$> Gtk.widgetGetWidth view
    vh <- fromIntegral <$> Gtk.widgetGetHeight view
    let small = fitted whole.size (fromIntegral w) (fromIntegral h)
        (sx, sy) = (small.dx, small.dy)
        k = small.zoom
        (r, g, b) = colorOf palette (Series 0)
    renderWithContext (Cairo.save >> Cairo.translate sx sy >> Cairo.scale k k) context
    forM_ whole.items $ \item -> paint m palette context item
    renderWithContext Cairo.restore context
    -- The view, in scene coordinates and then in the map's, cut to the
    -- scene: fitted, the view is larger than it.
    let x0 = max 0 (negate c.dx / c.zoom)
        y0 = max 0 (negate c.dy / c.zoom)
        x1 = min whole.size.w ((vw - c.dx) / c.zoom)
        y1 = min whole.size.h ((vh - c.dy) / c.zoom)
    renderWithContext (do
      Cairo.rectangle (sx + x0 * k) (sy + y0 * k) (max 0 (x1 - x0) * k) (max 0 (y1 - y0) * k)
      Cairo.setSourceRGB r g b
      Cairo.setLineWidth 1.5
      Cairo.stroke) context
  Highlight.follow m (const (Gtk.widgetQueueDraw m))
  Gtk.toWidget m

-- | The smallest node handle over a scene point, so a node inside
-- another is found before it.
under :: Scene -> Double -> Double -> Maybe Item
under drawn x y = case sortOn (\i -> i.size.w * i.size.h) hits of
  i : _ -> Just i
  [] -> Nothing
  where
    hits = [i | i@Handle {} <- drawn.items, x >= i.at.x, x <= i.at.x + i.size.w, y >= i.at.y, y <= i.at.y + i.size.h]

-- | Where a handle's centre is in the view.
onScreen :: Camera -> Item -> (Double, Double)
onScreen c i = (c.dx + c.zoom * (i.at.x + i.size.w / 2), c.dy + c.zoom * (i.at.y + i.size.h / 2))

percentOf :: Camera -> Text
percentOf c = T.pack (show (round (c.zoom * 100) :: Int)) <> "%"

-- | How much one press of the tools zooms.
step :: Double
step = 1.25

-- | Where the scene sits in the view: scaled by @zoom@, then moved by
-- @dx@ and @dy@ pixels.
type Camera :: Type
data Camera = Camera { zoom :: Double, dx :: Double, dy :: Double }

-- | The scene at its own size, in the middle of a view @w@ by @h@.
actual :: Size -> Double -> Double -> Camera
actual s w h = Camera 1 (max 0 (w - s.w) / 2) (max 0 (h - s.h) / 2)

-- | The whole scene, as large as the view allows, in its middle.
fitted :: Size -> Double -> Double -> Camera
fitted s w h = Camera z ((w - s.w * z) / 2) ((h - s.h * z) / 2)
  where
    z = bounded (min (w / max 1 s.w) (h / max 1 s.h))

-- | Zooms by @factor@, keeping the point under @(px, py)@ where it is.
zoomAround :: Double -> (Double, Double) -> Camera -> Camera
zoomAround factor (px, py) c = Camera z (px - (px - c.dx) * k) (py - (py - c.dy) * k)
  where
    z = bounded (c.zoom * factor)
    k = z / c.zoom

-- | From a quarter to four times the scene's own size.
bounded :: Double -> Double
bounded = max 0.25 . min 4

describe :: Font -> Text
describe font =
  (if font.mono then "JetBrains Mono" else "Inter") <> (if font.heavy then " Semi-Bold " else " ")
    <> T.pack (show (round font.size :: Int)) <> "px"

paint :: Gtk.DrawingArea -> Highlight.Palette -> GICairo.Context -> Item -> IO ()
paint area palette context item = case item of
  Handle {} -> pure ()
  Rect {at = p, size = s, radius = rad, fill = f, edge = e} -> shape f e (rounded p s rad)
  Polygon {points = ps, fill = f, edge = e} -> shape f e $ case ps of
    first : rest -> Cairo.moveTo first.x first.y >> mapM_ (\q -> Cairo.lineTo q.x q.y) rest >> Cairo.closePath
    [] -> pure ()
  Ellipse {center = c, rx = a, ry = b, fill = f, edge = e} -> shape f e $ do
    Cairo.save
    Cairo.translate c.x c.y
    Cairo.scale (max a 0.01) (max b 0.01)
    Cairo.arc 0 0 1 0 (2 * pi)
    Cairo.restore
  Wedge {center = c, r = a, from = f0, to = f1, fill = f, edge = e} -> shape f e $ do
    Cairo.moveTo c.x c.y
    Cairo.arc c.x c.y a f0 f1
    Cairo.closePath
  Path {start = p, segments = ss, line = l, begin = b, end = e} -> do
    render $ do
      Cairo.newPath
      Cairo.moveTo p.x p.y
      forM_ ss $ \s -> case s of
        LineTo q -> Cairo.lineTo q.x q.y
        CurveTo c1 c2 q -> Cairo.curveTo c1.x c1.y c2.x c2.y q.x q.y
      stroke l
    -- Heads point along the first and last pieces of the path.
    case (concatMap ends ss, reverse (p : concatMap ends ss)) of
      (after : _, tip : from' : _) -> do
        unless (e == NoHead) $ render (headAt l e from' tip)
        unless (b == NoHead) $ render (headAt l b after p)
      _ -> pure ()
  Text {at = p, text = t, font = f, color = c, anchor = a} -> do
    l <- Gtk.widgetCreatePangoLayout area (Just t)
    Pango.fontDescriptionFromString (describe f) >>= Pango.layoutSetFontDescription l . Just
    Pango.layoutSetAlignment l $ case a of
      Start -> Pango.AlignmentLeft
      Middle -> Pango.AlignmentCenter
      End -> Pango.AlignmentRight
    (w', h') <- Pango.layoutGetPixelSize l
    let left = case a of
          Start -> p.x
          Middle -> p.x - fromIntegral w' / 2
          End -> p.x - fromIntegral w'
    -- On a whole pixel, so glyphs are not cut where they are hinted.
    render (rgb c >> Cairo.moveTo (whole left) (whole (p.y - fromIntegral h' / 2)))
    PangoCairo.showLayout context l
  where
    whole v = fromIntegral (round v :: Int)
    render :: Cairo.Render () -> IO ()
    render r = renderWithContext r context
    rgb role = let (cr, cg, cb) = colorOf palette role in Cairo.setSourceRGB cr cg cb
    stroke l = do
      rgb l.color
      Cairo.setLineWidth l.width
      Cairo.setDash (maybe [] (\d -> [d, d * 0.8]) l.dash) 0
      Cairo.setLineJoin Cairo.LineJoinRound
      Cairo.setLineCap Cairo.LineCapRound
      Cairo.stroke
    shape f e outline = render $ do
      Cairo.newPath
      outline
      forM_ f $ \role -> rgb role >> (if e == Nothing then Cairo.fill else Cairo.fillPreserve)
      forM_ e stroke
    ends s = case s of
      LineTo q -> [q]
      CurveTo c1 c2 q -> [c1, c2, q]
    -- A head at @tip@, pointing from @from'@.
    headAt l h from' tip = do
      let dx = tip.x - from'.x
          dy = tip.y - from'.y
          len = max 0.001 (sqrt (dx * dx + dy * dy))
          (ux, uy) = (dx / len, dy / len)
          (nx, ny) = (-uy, ux)
          back d = (tip.x - ux * d, tip.y - uy * d)
          side (bx, by) s = (bx + nx * s, by + ny * s)
          triangle filled = do
            let base = back 10
                (ax, ay) = side base 4.5
                (cx', cy') = side base (-4.5)
            Cairo.newPath
            Cairo.moveTo tip.x tip.y >> Cairo.lineTo ax ay >> Cairo.lineTo cx' cy' >> Cairo.closePath
            if filled then rgb l.color >> Cairo.fill else rgb Paper >> Cairo.fillPreserve >> plain
          plain = rgb l.color >> Cairo.setLineWidth l.width >> Cairo.setDash [] 0 >> Cairo.stroke
          bar d = do
            let (bx, by) = back d
                (ax, ay) = side (bx, by) 6
                (cx', cy') = side (bx, by) (-6)
            Cairo.newPath >> Cairo.moveTo ax ay >> Cairo.lineTo cx' cy' >> plain
          ring d = do
            let (bx, by) = back d
            Cairo.newPath >> Cairo.arc bx by 4 0 (2 * pi)
            rgb Paper >> Cairo.fillPreserve >> plain
          fan = do
            let (bx, by) = back 10
                (ax, ay) = side (tip.x, tip.y) 6
                (cx', cy') = side (tip.x, tip.y) (-6)
            Cairo.newPath
            Cairo.moveTo ax ay >> Cairo.lineTo bx by >> Cairo.lineTo cx' cy'
            Cairo.moveTo tip.x tip.y >> Cairo.lineTo bx by
            plain
      case h of
        NoHead -> pure ()
        Arrow -> triangle True
        Hollow -> triangle False
        Diamond filled -> do
          let (mx, my) = back 7
              (ax, ay) = side (mx, my) 4.5
              (cx', cy') = side (mx, my) (-4.5)
              (ex, ey) = back 14
          Cairo.newPath
          Cairo.moveTo tip.x tip.y >> Cairo.lineTo ax ay >> Cairo.lineTo ex ey >> Cairo.lineTo cx' cy' >> Cairo.closePath
          if filled then rgb l.color >> Cairo.fill else rgb Paper >> Cairo.fillPreserve >> plain
        Dot -> Cairo.newPath >> Cairo.arc tip.x tip.y 3.5 0 (2 * pi) >> rgb l.color >> Cairo.fill
        Cross -> do
          let (ax, ay) = side (back 5) 5
              (bx, by) = side (back (-5)) (-5)
              (cx', cy') = side (back 5) (-5)
              (ex, ey) = side (back (-5)) 5
          Cairo.newPath >> Cairo.moveTo ax ay >> Cairo.lineTo bx by >> Cairo.moveTo cx' cy' >> Cairo.lineTo ex ey >> plain
        One -> bar 6 >> bar 10
        ZeroOrOne -> bar 6 >> ring 14
        Many -> fan
        OneOrMany -> fan >> bar 14
        ZeroOrMany -> fan >> ring 18

rounded :: Point -> Size -> Double -> Cairo.Render ()
rounded p s rad
  | rad <= 0 = Cairo.rectangle p.x p.y s.w s.h
  | otherwise = do
      let r' = min rad (min s.w s.h / 2)
          (x0, y0, x1, y1) = (p.x, p.y, p.x + s.w, p.y + s.h)
      Cairo.arc (x1 - r') (y0 + r') r' (-pi / 2) 0
      Cairo.arc (x1 - r') (y1 - r') r' 0 (pi / 2)
      Cairo.arc (x0 + r') (y1 - r') r' (pi / 2) pi
      Cairo.arc (x0 + r') (y0 + r') r' pi (3 * pi / 2)
      Cairo.closePath

-- | The role's color in the palette, as red, green and blue from 0 to 1;
-- the design's tokens, light and dark.
colorOf :: Highlight.Palette -> Role -> (Double, Double, Double)
colorOf palette role = hex $ case role of
  Ink -> pick "#1B1D21" "#E6E8EB"
  Muted -> pick "#5B616B" "#9AA0A9"
  Line -> pick "#5B616B" "#9AA0A9"
  Border -> pick "#B9C0CB" "#4A505A"
  Paper -> pick "#FFFFFF" "#23262B"
  Series n -> cycleOf (pick' series seriesDark) n
  Tint n -> cycleOf (pick' tints tintsDark) n
  Danger -> pick "#B42318" "#F28B82"
  DangerTint -> pick "#FDECEA" "#3C1F1D"
  OnSeries -> pick "#FFFFFF" "#101215"
  where
    dark = palette == Highlight.DarkPalette
    pick l d = if dark then d else l
    pick' l d = if dark then d else l
    cycleOf xs n = xs !! (n `mod` length xs)
    series = ["#2F5FD0", "#1F7A3A", "#C2410C", "#6D28D9", "#8A5A00", "#B42318", "#0E7490", "#BE185D"]
    seriesDark = ["#6E95F0", "#5BBF7A", "#F0905A", "#A88BF0", "#E0B050", "#F07068", "#3BB8D0", "#F06AA8"]
    tints = ["#E3E8F5", "#E3F1E7", "#FDE8DC", "#EFE9FB", "#FFF4E0", "#FDECEA", "#DDF1F5", "#FBE4EF"]
    tintsDark = ["#22304F", "#1E3A28", "#40291C", "#2E2546", "#3A3020", "#40201E", "#163640", "#3E1F30"]
    hex :: Text -> (Double, Double, Double)
    hex t = (channel 1, channel 3, channel 5)
      where
        channel i = case readHex (T.unpack (T.take 2 (T.drop i t))) of
          [(n, "")] -> fromIntegral (n :: Int) / 255
          _ -> 0

css :: Text
css = mconcat
  [ "overlay.u-diagram box.tools {"
  , "  margin: 8px; padding: 3px; border-radius: 8px; border: 1px solid var(--border);"
  , "  background: var(--surface); box-shadow: 0 2px 8px rgba(0,0,0,0.08);"
  , "}"
  , "overlay.u-diagram button.tool {"
  , "  min-height: 26px; min-width: 26px; padding: 0 4px; border-radius: 6px;"
  , "  background: transparent; border: none; box-shadow: none; color: var(--text-soft);"
  , "}"
  , "overlay.u-diagram button.tool:hover { background: var(--border-subtle); color: var(--text); }"
  , "overlay.u-diagram drawingarea.minimap {"
  , "  margin: 12px; border-radius: 6px; border: 1px solid var(--border); background: var(--surface);"
  , "  box-shadow: 0 2px 8px rgba(0,0,0,0.08);"
  , "}"
  , "overlay.u-diagram button.percent { min-width: 48px; font-family: 'JetBrains Mono', monospace; font-size: 12px; }"
  ]
