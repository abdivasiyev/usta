{-# LANGUAGE OverloadedStrings #-}

-- | A Mermaid flowchart as a scene: its nodes sized to their text, placed
-- by the layered layout, with curved links clipped to each shape.
module Usta.Diagram.Flowchart
  ( draw
  ) where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Traversable (for)

import Usta.Diagram.Layered (Box (..), Layout (..), Link (..), Placed (..), Routed (..), layered, spacing)
import Usta.Diagram.Scene hiding (Head (..))
import Usta.Diagram.Scene (Head (Arrow, NoHead))
import Usta.Mermaid (Direction (..), Edge (..), Flowchart (..), Group (..), Node (..), Shape (..), Stroke (..))

-- | A node with its text's size and its own.
type Sized :: Type
data Sized = Sized {node :: Node, text :: Size, box :: Size}

draw :: Monad m => Measure m -> Flowchart -> m Scene
draw measure chart = do
  sized <- for chart.nodes $ \n -> do
    t <- measure nodeFont n.label
    pure Sized {node = n, text = t, box = boxFor n.shape t}
  labels <- for chart.edges $ \e -> traverse (measure labelFont) e.label
  let across = chart.direction `elem` [LeftRight, RightLeft]
      -- The layout runs top to bottom; across the page, widths and heights
      -- trade places and turn back after.
      turn s = if across then Size {w = s.h, h = s.w} else s
      boxes = [Box {key = s.node.name, size = turn s.box} | s <- sized]
      links =
        [ Link {from = e.from, to = e.to, gap = maybe 0 (\l -> 8 + (if across then l.w else l.h)) label}
        | (e, label) <- zip chart.edges labels
        ]
      result = layered spacing boxes links
      ys = [p.center.y | p <- result.placed]
      bottom = maximum (0 : ys)
      place p = case chart.direction of
        TopDown -> p
        BottomUp -> Point {x = p.x, y = bottom - p.y}
        LeftRight -> Point {x = p.y, y = p.x}
        RightLeft -> Point {x = bottom - p.y, y = p.x}
      centers = Map.fromList [(p.key, place p.center) | p <- result.placed]
      byName = Map.fromList [(s.node.name, s) | s <- sized]
      routes = Map.fromListWith (flip (<>)) [((r.from, r.to), [map place r.points]) | r <- result.routed]
      shapeAt name = (,) <$> Map.lookup name centers <*> Map.lookup name byName
      -- Each edge takes the next route between its two nodes.
      edgeItems = concat (snd (foldl takeRoute (routes, []) (zip chart.edges labels)))
      takeRoute (left, acc) (e, label) = case Map.lookup (e.from, e.to) left of
        Just (ps : rest) -> (Map.insert (e.from, e.to) rest left, acc <> [edge e label ps])
        _ | e.from == e.to, Just (c, s) <- shapeAt e.from -> (left, acc <> [loop e label c s])
        _ -> (left, acc)
      edge e label ps = case ps of
        first : _ | Just (c0, s0) <- shapeAt e.from, Just (c1, s1) <- shapeAt e.to ->
          let inner = drop 1 (take (length ps - 1) ps)
              start = clip c0 s0 (headOr (last' c1 ps) inner) 0
              finish = clip c1 s1 (lastOr first inner) (if e.arrow then 2 else 0)
              path = start : inner <> [finish]
           in [ Path {start = start, segments = smooth path, line = penOf e.stroke, begin = NoHead, end = if e.arrow then Arrow else NoHead} ]
                <> maybe [] (labelAt (middle path)) ((,) <$> e.label <*> label)
        _ -> []
      loop e label c s =
        let side = c.x + s.box.w / 2
            a = Point {x = side, y = c.y - 8}
            b = Point {x = side, y = c.y + 8}
         in [ Path {start = a, segments = [CurveTo (Point (side + 34) (c.y - 30)) (Point (side + 34) (c.y + 30)) b], line = penOf e.stroke, begin = NoHead, end = if e.arrow then Arrow else NoHead} ]
              <> maybe [] (labelAt (Point (side + 30) c.y)) ((,) <$> e.label <*> label)
      labelAt p (t, size') =
        [ Rect {at = Point (p.x - size'.w / 2 - 4) (p.y - size'.h / 2 - 1), size = Size (size'.w + 8) (size'.h + 2), radius = 3, fill = Just Paper, edge = Nothing}
        , Text {at = p, text = t, font = labelFont, color = Muted, anchor = Middle}
        ]
      nodeItems = concat (mapMaybe (\s -> (\c -> shape c s) <$> Map.lookup s.node.name centers) sized)
      groupItems = concatMap frame chart.groups
      frame g = case [(c, s) | m <- g.members, Just (c, s) <- [shapeAt m]] of
        [] -> []
        members ->
          let left = minimum [c.x - s.box.w / 2 | (c, s) <- members] - 16
              top = minimum [c.y - s.box.h / 2 | (c, s) <- members] - 32
              right' = maximum [c.x + s.box.w / 2 | (c, s) <- members] + 16
              bottom' = maximum [c.y + s.box.h / 2 | (c, s) <- members] + 16
           in [ Rect {at = Point left top, size = Size (right' - left) (bottom' - top), radius = 8, fill = Just (Tint 0), edge = Just (pen Border 1)}
              , Text {at = Point (left + 10) (top + 13), text = g.title, font = bold 11, color = Muted, anchor = Start}
              ]
  pure (scene 12 (groupItems <> edgeItems <> nodeItems))
  where
    headOr d xs = case xs of
      x : _ -> x
      [] -> d
    lastOr d xs = if null xs then d else last xs
    last' d xs = if null xs then d else last xs

nodeFont, labelFont :: Font
nodeFont = regular 13
labelFont = regular 11

-- | Room around the text, and the shape's own proportions.
boxFor :: Shape -> Size -> Size
boxFor kind t = case kind of
  Diamond -> let side = max (t.w + 36) (t.h + 36) * 0.75 + max t.w t.h * 0.35 in Size side (max 56 (t.h * 2 + 24))
  Circle -> let d = max (t.w + 24) (t.h + 24) in Size d d
  Hexagon -> Size (t.w + 48) (max 38 (t.h + 18))
  Database -> Size (t.w + 32) (max 48 (t.h + 30))
  Flag -> Size (t.w + 44) (max 38 (t.h + 18))
  Stadium -> Size (t.w + 40) (max 38 (t.h + 18))
  _ -> Size (t.w + 32) (max 38 (t.h + 18))

shape :: Point -> Sized -> [Item]
shape c s =
  body <> [Text {at = textAt, text = s.node.label, font = nodeFont, color = Ink, anchor = Middle}]
  where
    b = s.box
    (l, t, r, bt) = (c.x - b.w / 2, c.y - b.h / 2, c.x + b.w / 2, c.y + b.h / 2)
    paper = Just Paper
    border = Just (pen Border 1.2)
    rect radius = [Rect {at = Point l t, size = b, radius = radius, fill = paper, edge = border}]
    textAt = case s.node.shape of
      Database -> Point c.x (c.y + 4)
      Flag -> Point (c.x - 6) c.y
      _ -> c
    body = case s.node.shape of
      Rectangle -> rect 3
      Rounded -> rect 10
      Stadium -> rect (b.h / 2)
      Subroutine ->
        rect 3
          <> [ Path {start = Point (l + 8) t, segments = [LineTo (Point (l + 8) bt)], line = pen Border 1.2, begin = NoHead, end = NoHead}
             , Path {start = Point (r - 8) t, segments = [LineTo (Point (r - 8) bt)], line = pen Border 1.2, begin = NoHead, end = NoHead}
             ]
      Diamond -> [Polygon {points = [Point c.x t, Point r c.y, Point c.x bt, Point l c.y], fill = Just (Tint 3), edge = Just (pen (Series 3) 1.2)}]
      Hexagon -> [Polygon {points = [Point (l + 14) t, Point (r - 14) t, Point r c.y, Point (r - 14) bt, Point (l + 14) bt, Point l c.y], fill = paper, edge = border}]
      Circle -> [Ellipse {center = c, rx = b.w / 2, ry = b.h / 2, fill = paper, edge = border}]
      Flag -> [Polygon {points = [Point l t, Point r t, Point (r - 14) c.y, Point r bt, Point l bt], fill = paper, edge = border}]
      Database ->
        let cap = 7
         in [ Rect {at = Point l (t + cap), size = Size b.w (b.h - 2 * cap), radius = 0, fill = paper, edge = Nothing}
            , Ellipse {center = Point c.x (bt - cap), rx = b.w / 2, ry = cap, fill = paper, edge = border}
            , Rect {at = Point (l + 0.6) (t + cap), size = Size (b.w - 1.2) (b.h - 2 * cap), radius = 0, fill = paper, edge = Nothing}
            , Path {start = Point l (t + cap), segments = [LineTo (Point l (bt - cap))], line = pen Border 1.2, begin = NoHead, end = NoHead}
            , Path {start = Point r (t + cap), segments = [LineTo (Point r (bt - cap))], line = pen Border 1.2, begin = NoHead, end = NoHead}
            , Ellipse {center = Point c.x (t + cap), rx = b.w / 2, ry = cap, fill = paper, edge = border}
            ]

-- | Where the line from @c@ toward @toward@ leaves the shape, @extra@
-- pixels further out (room for an arrowhead's tip).
clip :: Point -> Sized -> Point -> Double -> Point
clip c s toward extra = Point (c.x + ux * (t + extra)) (c.y + uy * (t + extra))
  where
    dx = toward.x - c.x
    dy = toward.y - c.y
    len = max 0.001 (sqrt (dx * dx + dy * dy))
    (ux, uy) = (dx / len, dy / len)
    hw = s.box.w / 2
    hh = s.box.h / 2
    t = case s.node.shape of
      Diamond -> 1 / max 0.001 (abs ux / hw + abs uy / hh)
      Circle -> hw
      _ -> min (if abs ux < 0.001 then 1 / 0 else hw / abs ux) (if abs uy < 0.001 then 1 / 0 else hh / abs uy)

-- | A curve through the points (Catmull-Rom as Béziers); straight when
-- there are only two.
smooth :: [Point] -> [Segment]
smooth ps = case ps of
  [_, b] -> [LineTo b]
  _ -> [CurveTo (ahead i) (behind (i + 1)) (at' (i + 1)) | i <- [0 .. length ps - 2]]
  where
    n = length ps
    at' i = ps !! max 0 (min (n - 1) i)
    ahead i = let a = at' (i - 1); b = at' (i + 1); p = at' i in Point (p.x + (b.x - a.x) / 6) (p.y + (b.y - a.y) / 6)
    behind i = let a = at' (i - 1); b = at' (i + 1); p = at' i in Point (p.x - (b.x - a.x) / 6) (p.y - (b.y - a.y) / 6)

middle :: [Point] -> Point
middle ps
  | odd (length ps) = ps !! (length ps `div` 2)
  | otherwise = let a = ps !! (length ps `div` 2 - 1); b = ps !! (length ps `div` 2) in Point ((a.x + b.x) / 2) ((a.y + b.y) / 2)

penOf :: Stroke -> Pen
penOf stroke = case stroke of
  Solid -> pen Line 1.4
  Dotted -> dashed (pen Line 1.4)
  Thick -> pen Line 2.6

