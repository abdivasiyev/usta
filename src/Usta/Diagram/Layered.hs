{-# LANGUAGE OverloadedStrings #-}

{- | Layered graph layout (Sugiyama's method), for flowcharts, state and
class diagrams: nodes go in ranks along the flow, edges between them.

1. Edges that close a cycle are turned around, so the graph flows one
   way. 2. Each node's rank is its longest path from a source. 3. An
   edge across several ranks gets a point in each rank between, so it
   bends around nodes. 4. The order within each rank is improved by
   moving nodes toward their neighbours' average position, sweeping down
   and up. 5. Nodes are placed in that order, pulled toward their
   neighbours and kept apart. Ranks run top to bottom here; callers turn
   the result for other directions.
-}
module Usta.Diagram.Layered
    ( Box (..)
    , Link (..)
    , Placed (..)
    , Routed (..)
    , Layout (..)
    , Spacing (..)
    , spacing
    , layered
    )
where

import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

import Usta.Diagram.Scene (Point (..), Size (..))

-- | A node to place, by its key and size.
type Box :: Type
data Box = Box {key :: Text, size :: Size}

{- | An edge between two boxes' keys; @gap@ is extra room it needs between
the two ranks, for its label.
-}
type Link :: Type
data Link = Link {from :: Text, to :: Text, gap :: Double}

type Placed :: Type
data Placed = Placed {key :: Text, center :: Point, size :: Size}

{- | An edge's points from its source's centre to its target's, through
the points it bends at; 'reversed' when it was turned around to break a
cycle (its points still run from source to target).
-}
type Routed :: Type
data Routed = Routed {from :: Text, to :: Text, points :: [Point], reversed :: Bool}

type Layout :: Type
data Layout = Layout {placed :: [Placed], routed :: [Routed]}

type Spacing :: Type
data Spacing = Spacing {nodes :: Double, ranks :: Double}

spacing :: Spacing
spacing = Spacing {nodes = 40, ranks = 56}

layered :: Spacing -> [Box] -> [Link] -> Layout
layered gaps boxes links = Layout {placed = placed', routed = map route chains}
    where
        keys = map (.key) boxes
        sizes = Map.fromList [(b.key, b.size) | b <- boxes]
        proper = [l | l <- links, l.from /= l.to, Map.member l.from sizes, Map.member l.to sizes]

        -- 1. Turn around the edges that go back to a node still being visited.
        backs = backEdges keys [(l.from, l.to) | l <- proper]
        directed = [if Set.member (l.from, l.to) backs then (l.to, l.from, l, True) else (l.from, l.to, l, False) | l <- proper]

        -- 2. Longest path from a source, repeated until nothing grows.
        rank0 = Map.fromList [(k, 0 :: Int) | k <- keys]
        ranked = fixpoint (\r -> foldl' (\m (a, b, _, _) -> Map.insert b (max (m Map.! b) (m Map.! a + 1)) m) r directed) rank0
        rankOf k = Map.findWithDefault 0 k ranked

        -- 3. Chains of points: real ends and one stand-in per rank between.
        chains =
            [ (link, flipped, a : [dummy i r | r <- [rankOf a + 1 .. rankOf b - 1]] <> [b])
            | (i, (a, b, link, flipped)) <- zip [0 :: Int ..] directed
            ]
        dummy i r = "\0" <> showText i <> "/" <> showText r
        dummies = [(d, rankOf' d) | (_, _, ps) <- chains, d <- ps, isDummy d]
        rankOf' d = maybe (rankOf d) snd (lookup d dummyRanks)
        dummyRanks = [(dummy i r, (i, r)) | (i, (a, b, _, _)) <- zip [0 :: Int ..] directed, r <- [rankOf a + 1 .. rankOf b - 1]]
        isDummy d = d `notElem` keys
        allNodes = [(k, rankOf k) | k <- keys] <> dummies
        rankCount = 1 + maximum (0 : map snd allNodes)
        pairs = concat [zip ps (drop 1 ps) | (_, _, ps) <- chains]
        ups = Map.fromListWith (<>) [(b, [a]) | (a, b) <- pairs]
        downs = Map.fromListWith (<>) [(a, [b]) | (a, b) <- pairs]
        initial = [[k | (k, r') <- allNodes, r' == r] | r <- [0 .. rankCount - 1]]

        -- 4. Barycentre sweeps, keeping the order with the fewest crossings.
        ordered = best (take 16 (iterate sweep initial))
        sweep layers = upward (downward layers)
        downward layers = scanl1 (\above here -> sortBy' (barycentre above ups) here) layers
        upward layers = reverse (scanl1 (\below here -> sortBy' (barycentre below downs) here) (reverse layers))
        barycentre neighbours links' node =
            case [i | n <- Map.findWithDefault [] node links', Just i <- [lookup n (zip neighbours [0 :: Int ..])]] of
                [] -> Nothing
                is -> Just (fromIntegral (sum is) / fromIntegral (length is) :: Double)
        -- Nodes without neighbours above keep their place.
        sortBy' score here =
            let
                scored = [(maybe (fromIntegral i) id (score n), n) | (i, n) <- zip [0 :: Int ..] here]
            in
                map snd (sortOn fst scored)
        best candidates = snd (minimum' [(crossings c, c) | c <- candidates])
        minimum' = foldr1 (\a b -> if fst a <= fst b then a else b)
        crossings layers = sum [between u l | (u, l) <- zip layers (drop 1 layers)]
        between upper lower =
            let
                pos layer = Map.fromList (zip layer [0 :: Int ..])
                pu = pos upper
                pl = pos lower
                es = [(pu Map.! a, pl Map.! b) | (a, b) <- pairs, Map.member a pu, Map.member b pl]
            in
                length [() | (a1, b1) <- es, (a2, b2) <- es, a1 < a2, b1 > b2]

        widthOf k = maybe 0 (.w) (Map.lookup k sizes)
        heightOf k = maybe 0 (.h) (Map.lookup k sizes)

        -- 5. Positions: packed left to right, then pulled toward neighbours.
        packed = Map.fromList (concatMap pack ordered)
        pack layer = snd (foldl' (\(x, acc) k -> (x + widthOf k + gaps.nodes, (k, x + widthOf k / 2) : acc)) (0, []) layer)
        xs = foldl' (\m _ -> relax m) packed [1 :: Int .. 8]
        relax m = Map.fromList (concatMap (settle m) ordered)
        settle m layer =
            let
                wanted k = case Map.findWithDefault [] k ups <> Map.findWithDefault [] k downs of
                    [] -> m Map.! k
                    ns -> sum [m Map.! n | n <- ns] / fromIntegral (length ns)
                desired = [(k, wanted k) | k <- layer]
                -- Keep each node clear of the one before it, then of the one
                -- after it, and meet in the middle.
                forward = snd (foldl' (\(edge, acc) (k, x) -> let x' = max x (edge + widthOf k / 2) in (x' + widthOf k / 2 + gaps.nodes, acc <> [(k, x')])) (-1 / 0, []) desired)
                backward = snd (foldr (\(k, x) (edge, acc) -> let x' = min x (edge - widthOf k / 2) in (x' - widthOf k / 2 - gaps.nodes, (k, x') : acc)) (1 / 0, []) desired)
            in
                [(k, (a + b) / 2) | ((k, a), (_, b)) <- zip forward backward]
        rankHeights = [maximum (0 : map heightOf layer) | layer <- ordered]
        labelGaps = Map.fromListWith max [(min (rankOf a) (rankOf b), l.gap) | (a, b, l, _) <- directed]
        rankTops = scanl (\top (r, h) -> top + h + gaps.ranks + Map.findWithDefault 0 r labelGaps) 0 (zip [0 ..] rankHeights)
        yOf k = let r = rankOf' k in (rankTops !! r) + (rankHeights !! r) / 2
        pointOf k = Point {x = xs Map.! k, y = yOf k}
        placed' = [Placed {key = k, center = pointOf k, size = fromMaybe (Size 0 0) (Map.lookup k sizes)} | k <- keys]
        route (link, flipped, ps) =
            let
                points' = map pointOf ps
            in
                Routed {from = link.from, to = link.to, points = if flipped then reverse points' else points', reversed = flipped}

-- | Edges that reach a node still on the depth-first path.
backEdges :: [Text] -> [(Text, Text)] -> Set.Set (Text, Text)
backEdges keys edges = third (foldl' visitFrom (Set.empty, Set.empty, Set.empty) keys)
    where
        third (_, _, c) = c
        out = Map.fromListWith (flip (<>)) [(a, [b]) | (a, b) <- edges]
        visitFrom st@(done, _, _) k
            | Set.member k done = st
            | otherwise = visit st k
        visit (done, onPath, backs) k =
            let
                (done', _, backs') = foldl' (step k) (Set.insert k done, Set.insert k onPath, backs) (Map.findWithDefault [] k out)
            in
                (done', onPath, backs')
        step k (done, onPath, backs) n
            | Set.member n onPath = (done, onPath, Set.insert (k, n) backs)
            | Set.member n done = (done, onPath, backs)
            | otherwise = let (d, _, b) = visit (done, onPath, backs) n in (d, onPath, b)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a = let b = f a in if b == a then a else fixpoint f b

showText :: Int -> Text
showText = T.pack . show
