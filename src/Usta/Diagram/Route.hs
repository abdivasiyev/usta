-- | Links between laid-out boxes: where a line leaves a box's outline,
-- a smooth curve through the points it bends at, and its middle, for a
-- label.
module Usta.Diagram.Route
  ( Outline (..)
  , clip
  , smooth
  , middle
  ) where

import Data.Kind (Type)

import Usta.Diagram.Scene (Point (..), Segment (..), Size (..))

-- | The outline a line is clipped to.
type Outline :: Type
data Outline = Boxy | Rhombus | Round

-- | Where the line from the box's centre @c@ toward @toward@ leaves its
-- outline, @extra@ pixels further out (room for an arrowhead's tip).
clip :: Outline -> Point -> Size -> Point -> Double -> Point
clip outline c size toward extra = Point (c.x + ux * (t + extra)) (c.y + uy * (t + extra))
  where
    dx = toward.x - c.x
    dy = toward.y - c.y
    len = max 0.001 (sqrt (dx * dx + dy * dy))
    (ux, uy) = (dx / len, dy / len)
    hw = size.w / 2
    hh = size.h / 2
    t = case outline of
      Rhombus -> 1 / max 0.001 (abs ux / hw + abs uy / hh)
      Round -> hw
      Boxy -> min (if abs ux < 0.001 then 1 / 0 else hw / abs ux) (if abs uy < 0.001 then 1 / 0 else hh / abs uy)

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

-- | The point halfway along the bends, for a label.
middle :: [Point] -> Point
middle ps
  | odd (length ps) = ps !! (length ps `div` 2)
  | otherwise = let a = ps !! (length ps `div` 2 - 1); b = ps !! (length ps `div` 2) in Point ((a.x + b.x) / 2) ((a.y + b.y) / 2)
