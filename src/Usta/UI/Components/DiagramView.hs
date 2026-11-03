{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Draws a 'Scene' with GTK: cairo for the shapes, Pango for the text,
-- in the palette of the current theme, and again when it changes.
module Usta.UI.Components.DiagramView
  ( new
  , colorOf
  ) where

import Control.Monad (forM_, unless)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import Data.Traversable (for)
import qualified Data.Text as T
import qualified GI.Cairo as GICairo
import qualified GI.Cairo.Render as Cairo
import GI.Cairo.Render.Connector (renderWithContext)
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import qualified GI.PangoCairo as PangoCairo
import Numeric (readHex)

import Usta.Diagram.Scene
import qualified Usta.UI.Highlight as Highlight

-- | @layout@ gets a Pango measure and gives the scene; it runs once, when
-- the view is made.
new :: (Measure IO -> IO Scene) -> IO Gtk.Widget
new layout = do
  area <- GI.new Gtk.DrawingArea [#halign := Gtk.AlignStart]
  let measure font text = do
        l <- Gtk.widgetCreatePangoLayout area (Just text)
        Pango.fontDescriptionFromString (describe font) >>= Pango.layoutSetFontDescription l . Just
        (w', h') <- Pango.layoutGetPixelSize l
        pure Size {w = fromIntegral w', h = fromIntegral h'}
  laid <- layout measure
  -- Text reaches as far as it is wide; the canvas grows (and the items
  -- move right or down) so none of it is cut off.
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
      drawn = Scene {size = Size width height, items = map (translate dx dy) laid.items}
  GI.set area [#contentWidth := ceiling drawn.size.w, #contentHeight := ceiling drawn.size.h]
  Gtk.drawingAreaSetDrawFunc area $ Just $ \_ context _ _ -> do
    palette <- Highlight.currentPalette
    forM_ drawn.items $ \item -> paint area palette context item
  Highlight.follow area (const (Gtk.widgetQueueDraw area))
  Gtk.toWidget area

describe :: Font -> Text
describe font =
  (if font.mono then "JetBrains Mono" else "Inter") <> (if font.heavy then " Semi-Bold " else " ")
    <> T.pack (show (round font.size :: Int)) <> "px"

paint :: Gtk.DrawingArea -> Highlight.Palette -> GICairo.Context -> Item -> IO ()
paint area palette context item = case item of
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
