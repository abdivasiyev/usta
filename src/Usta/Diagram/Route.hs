{- | Links between laid-out boxes: where a line leaves a box's outline,
a smooth curve through the points it bends at, and its middle, for a
label.
-}
module Usta.Diagram.Route
    ( Outline (..)
    , clip
    , smooth
    , middle
    , spread
    , Area (..)
    , around
    , labelSpots
    , unbent
    )
where

import Data.Kind (Type)
import Data.List (minimumBy)
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Data.Text (Text)

import Data.Map.Strict qualified as Map

import Usta.Diagram.Scene (Moves, Point (..), Segment (..), Size (..))

-- | The outline a line is clipped to.
type Outline :: Type
data Outline = Boxy | Rhombus | Round

{- | Where the line from the box's centre @c@ toward @toward@ leaves its
outline, @extra@ pixels further out (room for an arrowhead's tip).
-}
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

{- | A curve through the points (Catmull-Rom as Béziers); straight when
there are only two.
-}
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

{- | Links between the same two boxes, either way round, bowed apart so
they and their labels do not lie on one another; a link to itself is
left as it is.
-}
spread :: Eq k => [(k, k, [Point])] -> [[Point]]
spread links = [bow (offset i) (canon i) ps | (i, (_, _, ps)) <- zip [0 :: Int ..] links]
    where
        pair (a, b, _) = (a, b)
        same (a, b) (c, d) = a /= b && ((a, b) == (c, d) || (a, b) == (d, c))
        twins i = [j | (j, l) <- zip [0 ..] links, same (pair (links !! i)) (pair l)]
        offset i = case twins i of
            js@(_ : _ : _) -> (fromIntegral (length (takeWhile (/= i) js)) - fromIntegral (length js - 1) / 2) * 38
            _ -> 0
        -- One direction for the whole group, so the two ways bow apart.
        canon i =
            let
                (a, _, _) = links !! i
                (first, _, _) = links !! headOr i (twins i)
            in
                a == first
        headOr d xs = case xs of
            x : _ -> x
            [] -> d

bow :: Double -> Bool -> [Point] -> [Point]
bow 0 _ ps = ps
bow o forward ps = case ps of
    a : rest@(_ : _) ->
        let
            b = last rest
            (dx, dy) = if forward then (b.x - a.x, b.y - a.y) else (a.x - b.x, a.y - b.y)
            len = max 0.001 (sqrt (dx * dx + dy * dy))
            (nx, ny) = (-dy / len * o, dx / len * o)
            shift p = Point (p.x + nx) (p.y + ny)
        in
            case rest of
                [_] -> [a, shift (Point ((a.x + b.x) / 2) ((a.y + b.y) / 2)), b]
                _ -> a : map shift (init rest) <> [b]
    _ -> ps

-- | A box on the page by its top-left corner, for labels to keep off.
type Area :: Type
data Area = Area {at :: Point, size :: Size}

-- | The area of a box of @size@ centred on @c@.
around :: Point -> Size -> Area
around c size = Area (Point (c.x - size.w / 2) (c.y - size.h / 2)) size

{- | Where each link's label goes: the middle of the link when it is
clear of the boxes and the labels placed before it, or else the clear
spot nearest the middle along it, or the least covered one.
-}
labelSpots :: [Area] -> [([Point], Maybe Size)] -> [Maybe Point]
labelSpots boxes = go boxes
    where
        go _ [] = []
        go taken ((path, Just size) : rest) =
            let
                room = Size (size.w + 12) (size.h + 6)
                tries = [along path t | t <- [0.5, 0.4, 0.6, 0.3, 0.7, 0.22, 0.78]]
                cost p = sum [overlap (around p room) a | a <- taken]
                spot = case [p | p <- tries, cost p == 0] of
                    p : _ -> p
                    [] -> minimumBy (comparing cost) tries
            in
                Just spot : go (around spot room : taken) rest
        go taken ((_, Nothing) : rest) = Nothing : go taken rest

overlap :: Area -> Area -> Double
overlap a b =
    max 0 (min (a.at.x + a.size.w) (b.at.x + b.size.w) - max a.at.x b.at.x)
        * max 0 (min (a.at.y + a.size.h) (b.at.y + b.size.h) - max a.at.y b.at.y)

-- | The point a fraction @t@ of the way along the bends.
along :: [Point] -> Double -> Point
along ps t = case ps of
    [] -> Point 0 0
    [p] -> p
    _ -> walk (t * total) (zip ps (drop 1 ps))
    where
        dist a b = sqrt ((b.x - a.x) ^ (2 :: Int) + (b.y - a.y) ^ (2 :: Int))
        total = sum [dist a b | (a, b) <- zip ps (drop 1 ps)]
        walk _ [] = last ps
        walk left ((a, b) : more)
            | left <= d || null more = let f = if d == 0 then 0 else min 1 (left / d) in Point (a.x + (b.x - a.x) * f) (a.y + (b.y - a.y) * f)
            | otherwise = walk (left - d) more
            where
                d = dist a b

{- | A link's bends, or a straight line between the centres when the
reader dragged either end: the bends were laid out for where it was.
-}
unbent :: Moves -> Map Text Point -> Text -> Text -> [Point] -> [Point]
unbent drags centers a b ps
    | Map.member a drags || Map.member b drags, Just c0 <- Map.lookup a centers, Just c1 <- Map.lookup b centers = [c0, c1]
    | otherwise = ps
