{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid architecture diagrams: services and junctions on a grid,
-- placed by the sides their edges join ("db:L -- R:server" puts the
-- server to the database's left), groups framed around their members,
-- and edges drawn square from side to side.
module Usta.Diagram.Architecture
  ( Architecture (..)
  , parse
  , draw
  ) where

import Data.Kind (Type)
import Control.Monad (join)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)

import Usta.Diagram.Scene

type Side :: Type
data Side = L | R | T | B
  deriving stock (Eq)

-- | A service (with its icon and title) or a junction, and its group.
type Node :: Type
data Node = Node {name :: Text, icon :: Maybe Text, title :: Maybe Text, parent :: Maybe Text}

type Group :: Type
data Group = Group {name :: Text, icon :: Text, title :: Text, parent :: Maybe Text}

-- | One end of an edge: a node's side, or its group's with "{group}".
type Joint :: Type
data Joint = Joint {node :: Text, side :: Side, group :: Bool, head :: Bool}

type Edge :: Type
data Edge = Edge {from :: Joint, to :: Joint}

type Architecture :: Type
data Architecture = Architecture {groups :: [Group], nodes :: [Node], edges :: [Edge]}

parse :: Text -> Either Text Architecture
parse source = do
  parsed <- traverse line [l | l <- drop 1 (map T.strip (T.lines source)), not (T.null l), not ("%%" `T.isPrefixOf` l)]
  let a = Architecture {groups = [g | Left (Left g) <- parsed], nodes = [n | Left (Right n) <- parsed], edges = [e | Right e <- parsed]}
  if null a.nodes then Left "The architecture diagram has no services." else pure a
  where
    line l = case T.words l of
      "group" : _ -> let (n, i, t, p) = declared (T.drop 5 l) in pure (Left (Left (Group n (fromMaybe "cloud" i) (fromMaybe n t) p)))
      "service" : _ -> let (n, i, t, p) = declared (T.drop 7 l) in pure (Left (Right (Node n (Just (fromMaybe "server" i)) t p)))
      "junction" : _ -> let (n, _, _, p) = declared (T.drop 8 l) in pure (Left (Right (Node n Nothing Nothing p)))
      [a, connector, b] | Just (begin, end) <- arrows connector, Just from <- left a begin, Just to <- right b end -> pure (Right (Edge from to))
      _ -> Left ("usta could not read \"" <> T.take 40 l <> "\" in the architecture diagram.")
    -- "id(icon)[Title] in parent"
    declared t =
      let (front, inPart) = T.breakOn " in " (T.strip t)
          (name, rest) = T.break (`elem` ("([ " :: String)) front
          icon = between "(" ")" rest
          title = between "[" "]" rest
          parent = if T.null inPart then Nothing else Just (T.strip (T.drop 4 inPart))
       in (T.strip name, icon, title, parent)
    between o c t = case T.breakOn o t of
      (_, r) | not (T.null r) -> Just (fst (T.breakOn c (T.drop 1 r)))
      _ -> Nothing
    arrows c = lookup c [("--", (False, False)), ("-->", (False, True)), ("<--", (True, False)), ("<-->", (True, True))]
    -- "db{group}:L" and "R:server{group}"
    left t begin = case T.splitOn ":" t of
      [n, s] -> (\side -> Joint (T.replace "{group}" "" n) side ("{group}" `T.isSuffixOf` n) begin) <$> sideOf s
      _ -> Nothing
    right t end = case T.splitOn ":" t of
      [s, n] -> (\side -> Joint (T.replace "{group}" "" n) side ("{group}" `T.isSuffixOf` n) end) <$> sideOf s
      _ -> Nothing
    sideOf s = lookup s [("L", L), ("R", R), ("T", T), ("B", B)]

-- | Grid cells, from each edge's sides: what is joined on a node's left
-- goes one column left of it, and so on; a taken cell pushes it further.
cells :: Architecture -> Map.Map Text (Int, Int)
cells a = foldl' start Map.empty (map (.name) a.nodes)
  where
    start placed n
      | Map.member n placed = placed
      | otherwise =
          let row = if Map.null placed then 0 else maximum [r | (_, r) <- Map.elems placed] + 2
           in spread (Map.insert n (0, row) placed) [n]
    spread placed [] = placed
    spread placed (n : queue) =
      let here = placed Map.! n
          next = [(o, step here s) | (o, s) <- neighbours n, not (Map.member o placed)]
          (placed', added) = foldl' (\(p, acc) (o, (cell, dir)) -> if Map.member o p then (p, acc) else (Map.insert o (free p cell dir) p, acc <> [o])) (placed, []) next
       in spread placed' (queue <> added)
    neighbours n = [(e.to.node, e.from.side) | e <- a.edges, e.from.node == n] <> [(e.from.node, e.to.side) | e <- a.edges, e.to.node == n]
    step (x, y) s = case s of
      L -> ((x - 1, y), (-1, 0))
      R -> ((x + 1, y), (1, 0))
      T -> ((x, y - 1), (0, -1))
      B -> ((x, y + 1), (0, 1))
    free p cell@(x, y) d@(dx, dy) = if cell `elem` Map.elems p then free p (x + dx, y + dy) d else cell

draw :: Monad m => Measure m -> Architecture -> m Scene
draw measure a = do
  labels <- for a.nodes $ \n -> (n.name,) <$> maybe (pure []) (wrap measure (regular 12) 130) n.title
  let grid = cells a
      minX = minimum (0 : [x | (x, _) <- Map.elems grid])
      minY = minimum (0 : [y | (_, y) <- Map.elems grid])
      lines' = Map.fromList labels
      centre n = (\(x, y) -> Point (fromIntegral (x - minX) * 190 + 100) (fromIntegral (y - minY) * 160 + 80)) <$> Map.lookup n grid
      isJunction n = maybe True (\x -> isNothing x.icon) (lookup n [(x.name, x) | x <- a.nodes])
      -- The box an edge meets: the icon, and the title under it.
      box n = do
        c <- centre n
        let half = if isJunction n then 5 else iconSize / 2
            below = case Map.findWithDefault [] n lines' of
              [] -> 0
              ls -> fromIntegral (length ls) * 16 + 8
        pure (c.x - half, c.y - half, c.x + half, c.y + half + below)
      depthOf g = maybe 0 ((+ 1) . depthOf) (lookup g [(x.name, x) | x <- a.groups] >>= (.parent))
      members g = [n.name | n <- a.nodes, n.parent == Just g.name]
      children g = [c | c <- a.groups, c.parent == Just g.name]
      frame :: Group -> Maybe (Double, Double, Double, Double)
      frame g =
        let inner = mapMaybe box (members g) <> mapMaybe frame (children g)
            pad = 22
         in if null inner
              then Nothing
              else Just (minimum [l | (l, _, _, _) <- inner] - pad, minimum [t | (_, t, _, _) <- inner] - pad - 26, maximum [r | (_, _, r, _) <- inner] + pad, maximum [b | (_, _, _, b) <- inner] + pad)
      groupOf n = join (lookup n [(x.name, x.parent) | x <- a.nodes])
      endBox e = if e.group then groupOf e.node >>= \g -> lookup g [(x.name, x) | x <- a.groups] >>= frame else box e.node
      sidePoint (l, t, r, b) s = case s of
        L -> Point l ((t + b) / 2)
        R -> Point r ((t + b) / 2)
        T -> Point ((l + r) / 2) t
        B -> Point ((l + r) / 2) b
      horizontal s = s == L || s == R
      edgeItem e = do
        b0 <- endBox e.from
        b1 <- endBox e.to
        let p = sidePoint b0 e.from.side
            q = sidePoint b1 e.to.side
            bends = case (horizontal e.from.side, horizontal e.to.side) of
              (True, True) -> let m = (p.x + q.x) / 2 in [Point m p.y, Point m q.y]
              (False, False) -> let m = (p.y + q.y) / 2 in [Point p.x m, Point q.x m]
              (True, False) -> [Point q.x p.y]
              (False, True) -> [Point p.x q.y]
        pure Path {start = p, segments = map LineTo (bends <> [q]), line = pen Line 1.4, begin = if e.from.head then Arrow else NoHead, end = if e.to.head then Arrow else NoHead}
      groupItems g = concat
        [ [ Rect {at = Point l t, size = Size (r - l) (b - t), radius = 8, fill = Just (Tint (depthOf g.name)), edge = Just (dashed (pen Border 1.2))}
          , Text {at = Point (l + 36) (t + 16), text = g.title, font = bold 12, color = Ink, anchor = Start} ]
            <> glyph g.icon 22 (Point (l + 8) (t + 5))
        | Just (l, t, r, b) <- [frame g] ]
      nodeItems = concat
        [ case n.icon of
            Nothing -> [Ellipse {center = c, rx = 5, ry = 5, fill = Just Line, edge = Nothing}]
            Just i ->
              glyph i iconSize (Point (c.x - iconSize / 2) (c.y - iconSize / 2))
                <> [Text {at = Point c.x (c.y + iconSize / 2 + 12 + 16 * fromIntegral k), text = t, font = regular 12, color = Ink, anchor = Middle} | (k, t) <- zip [0 :: Int ..] (Map.findWithDefault [] n.name lines')]
        | n <- a.nodes, Just c <- [centre n.name] ]
  -- Outer groups first, so inner ones are drawn over them.
  pure (scene 12 (concatMap groupItems (sortOn (depthOf . (.name)) a.groups) <> mapMaybe edgeItem a.edges <> nodeItems))
  where
    iconSize = 52

-- | A built-in icon as a filled square with its glyph; an icon usta does
-- not have (such as "logos:aws") shows its first letter.
glyph :: Text -> Double -> Point -> [Item]
glyph name s o =
  Rect {at = o, size = Size s s, radius = s * 0.18, fill = Just (Series colour), edge = Nothing} : marks
  where
    k = s / 48
    at x y = Point (o.x + x * k) (o.y + y * k)
    stroke = Just (pen OnSeries (max 1 (2 * k)))
    line' ps = Path {start = head' ps, segments = map LineTo (drop 1 ps), line = pen OnSeries (max 1 (2 * k)), begin = NoHead, end = NoHead}
    head' ps = case ps of
      p : _ -> p
      [] -> o
    known = ["server", "database", "disk", "cloud", "internet"]
    colour = length (takeWhile (/= name) known)
    marks = case name of
      "server" -> concat [[Rect {at = at 10 y, size = Size (28 * k) (9 * k), radius = 2 * k, fill = Nothing, edge = stroke}, Ellipse {center = at 33 (y + 4.5), rx = 1.6 * k, ry = 1.6 * k, fill = Just OnSeries, edge = Nothing}] | y <- [9, 20, 31]]
      "database" ->
        [ Ellipse {center = at 24 13, rx = 13 * k, ry = 5 * k, fill = Nothing, edge = stroke}
        , line' [at 11 13, at 11 35], line' [at 37 13, at 37 35]
        , Path {start = at 11 24, segments = [CurveTo (at 11 31) (at 37 31) (at 37 24)], line = pen OnSeries (max 1 (2 * k)), begin = NoHead, end = NoHead}
        , Path {start = at 11 35, segments = [CurveTo (at 11 42) (at 37 42) (at 37 35)], line = pen OnSeries (max 1 (2 * k)), begin = NoHead, end = NoHead} ]
      "disk" ->
        [ Ellipse {center = at 24 24, rx = 14 * k, ry = 14 * k, fill = Nothing, edge = stroke}
        , Ellipse {center = at 24 24, rx = 4 * k, ry = 4 * k, fill = Just OnSeries, edge = Nothing} ]
      "cloud" ->
        [ Ellipse {center = at 17 28, rx = 8 * k, ry = 7 * k, fill = Just OnSeries, edge = Nothing}
        , Ellipse {center = at 26 22, rx = 10 * k, ry = 9 * k, fill = Just OnSeries, edge = Nothing}
        , Ellipse {center = at 34 29, rx = 7 * k, ry = 6 * k, fill = Just OnSeries, edge = Nothing}
        , Rect {at = at 12 28, size = Size (24 * k) (7 * k), radius = 3 * k, fill = Just OnSeries, edge = Nothing} ]
      "internet" ->
        [ Ellipse {center = at 24 24, rx = 14 * k, ry = 14 * k, fill = Nothing, edge = stroke}
        , Ellipse {center = at 24 24, rx = 6 * k, ry = 14 * k, fill = Nothing, edge = stroke}
        , line' [at 10 24, at 38 24], line' [at 13 17, at 35 17], line' [at 13 31, at 35 31] ]
      _ -> [Text {at = at 24 24, text = T.toUpper (T.take 1 (T.takeWhileEnd (/= ':') name)), font = bold (16 * k), color = OnSeries, anchor = Middle}]
