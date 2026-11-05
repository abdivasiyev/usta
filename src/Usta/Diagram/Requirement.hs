{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid requirement diagrams: requirements and the elements that
-- satisfy, verify or trace them, as boxes listing their fields, joined by
-- links named after their relationship.
module Usta.Diagram.Requirement
  ( Requirements (..)
  , parse
  , draw
  ) where

import Data.Char (toUpper)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)

import Usta.Diagram.Layered (Box (..), Layout (..), Link (..), Placed (..), Routed (..), layered, spacing)
import qualified Usta.Diagram.Route as Route
import Usta.Diagram.Scene

-- | A requirement or an element: its name, what it is ("Functional
-- Requirement", "Element") and its fields in order, named as shown.
type Node :: Type
data Node = Node {name :: Text, kind :: Text, fields :: [(Text, Text)]}

-- | @from@ satisfies, verifies, traces … @to@.
type Relation :: Type
data Relation = Relation {from :: Text, to :: Text, kind :: Text}

type Flow :: Type
data Flow = Down | Up | Across | Back

type Requirements :: Type
data Requirements = Requirements {flow :: Flow, nodes :: [Node], relations :: [Relation]}

parse :: Text -> Either Text Requirements
parse source = go (drop 1 lines') Down [] []
  where
    lines' = [l | l <- map T.strip (T.lines source), not (T.null l), not ("%%" `T.isPrefixOf` l)]
    go ls flow ns rs = case ls of
      [] | null ns -> Left "The requirement diagram has no requirements or elements."
         | otherwise -> Right Requirements {flow, nodes = reverse ns, relations = reverse rs}
      l : rest
        | Just d <- T.stripPrefix "direction" l -> go rest (direction (T.strip d)) ns rs
        | any (`T.isPrefixOf` l) ["style ", "classDef ", "class "] -> go rest flow ns rs
        | word : _ <- T.words l, Just kind <- lookup word kinds -> do
            let name = unquote (T.strip (T.takeWhile (/= '{') (T.drop (T.length word) l)))
                (body, after) = break (== "}") rest
            if "{" `T.isSuffixOf` l
              then go (drop 1 after) flow (Node {name, kind, fields = concatMap field body} : ns) rs
              else Left ("usta could not read \"" <> T.take 40 l <> "\"; a " <> word <> " opens with {.")
        | Just r <- relation l -> go rest flow ns (r : rs)
        | otherwise -> Left ("usta could not read \"" <> T.take 40 l <> "\" in the requirement diagram.")
    direction d = case d of
      "BT" -> Up
      "LR" -> Across
      "RL" -> Back
      _ -> Down
    kinds =
      [ ("requirement", "Requirement")
      , ("functionalRequirement", "Functional Requirement")
      , ("interfaceRequirement", "Interface Requirement")
      , ("performanceRequirement", "Performance Requirement")
      , ("physicalRequirement", "Physical Requirement")
      , ("designConstraint", "Design Constraint")
      , ("element", "Element") ]
    field l = case T.breakOn ":" l of
      (k, v) | not (T.null v) -> [(shownKey (T.strip k), shownValue (T.strip k) (unquote (T.strip (T.drop 1 v))))]
      _ -> []
    shownKey k = fromMaybe k (lookup (T.toLower k) [("id", "Id"), ("text", "Text"), ("risk", "Risk"), ("verifymethod", "Verification"), ("type", "Type"), ("docref", "Doc Ref")])
    shownValue k v = if T.toLower k `elem` ["risk", "verifymethod"] then capital v else v
    capital v = maybe v (\(c, cs) -> T.cons (toUpper c) cs) (T.uncons v)
    -- "a - satisfies -> b", or "b <- copies - a" the other way.
    relation l
      | (left, right) <- T.breakOn "->" l, not (T.null right), [a, k] <- map T.strip (T.splitOn " - " (T.strip left)) =
          Just Relation {from = unquote a, to = unquote (T.strip (T.drop 2 right)), kind = k}
      | (left, right) <- T.breakOn "<-" l, not (T.null right), [k, a] <- map T.strip (T.splitOn " - " (T.strip (T.drop 2 right))) =
          Just Relation {from = unquote a, to = unquote (T.strip left), kind = k}
      | otherwise = Nothing
    unquote = T.dropAround (== '"')

draw :: Monad m => Measure m -> Requirements -> m Scene
draw measure diagram = do
  sized <- for diagram.nodes $ \n -> do
    rows <- concat <$> for n.fields (\(k, v) -> wrap measure (regular 12) 240 (k <> ": " <> v))
    widths <- for (n.name : rows) (fmap (.w) . measure (bold 13))
    kindW <- (.w) <$> measure (regular 11) ("<<" <> n.kind <> ">>")
    let width = max 140 (maximum (kindW : widths) + 28)
        height = 48 + fromIntegral (length rows) * 17 + (if null rows then 0 else 12)
    pure (n, Size width height, rows)
  labels <- for diagram.relations (\r -> measure (regular 11) (shown r))
  let across = case diagram.flow of
        Across -> True
        Back -> True
        _ -> False
      turn s = if across then Size s.h s.w else s
      boxes = [Box {key = n.name, size = turn s} | (n, s, _) <- sized]
      links = [Link {from = r.from, to = r.to, gap = 10 + (if across then l.w else l.h)} | (r, l) <- zip diagram.relations labels]
      result = layered spacing boxes links
      bottom = maximum (0 : [p.center.y | p <- result.placed])
      place p = case diagram.flow of
        Down -> p
        Up -> Point p.x (bottom - p.y)
        Across -> Point p.y p.x
        Back -> Point (bottom - p.y) p.x
      centers = Map.fromList [(p.key, place p.center) | p <- result.placed]
      sizes = Map.fromList [(n.name, s) | (n, s, _) <- sized]
      routes = Map.fromListWith (flip (<>)) [((r.from, r.to), [map place r.points]) | r <- result.routed]
      taken = snd (foldl' takeRoute (routes, []) (zip diagram.relations labels))
      takeRoute (left, acc) (r, l) = case Map.lookup (r.from, r.to) left of
        Just (ps : more) -> (Map.insert (r.from, r.to) more left, acc <> [(r, l, ps)])
        _ -> (left, acc)
      drawn = catMaybes [link r l ps | ((r, l, _), ps) <- zip taken (Route.spread [(r.from, r.to, ps) | (r, _, ps) <- taken])]
      keepOff = [Route.around c s | (n, s, _) <- sized, Just c <- [Map.lookup n.name centers]]
      spots = Route.labelSpots keepOff [(path, Just size) | (path, _, size, _) <- drawn]
      linkItems = concat [line' <> maybe [] labelOf spot | ((_, line', _, labelOf), spot) <- zip drawn spots]
      link r l ps = do
        c0 <- Map.lookup r.from centers
        s0 <- Map.lookup r.from sizes
        c1 <- Map.lookup r.to centers
        s1 <- Map.lookup r.to sizes
        let inner = drop 1 (take (length ps - 1) ps)
            a = Route.clip Route.Boxy c0 s0 (headOr c1 inner) 1
            b = Route.clip Route.Boxy c1 s1 (lastOr c0 inner) 1
            path = a : inner <> [b]
            contains = r.kind == "contains"
        pure
          ( path
          , [Path {start = a, segments = Route.smooth path, line = (if contains then id else dashed) (pen Line 1.3), begin = if contains then Dot else NoHead, end = Arrow}]
          , l
          , \m -> [ Rect {at = Point (m.x - l.w / 2 - 4) (m.y - l.h / 2 - 1), size = Size (l.w + 8) (l.h + 2), radius = 3, fill = Just Paper, edge = Nothing}
                  , Text {at = m, text = shown r, font = regular 11, color = Muted, anchor = Middle} ] )
      nodeItems = concat [maybe [] (\c -> box c n s rows) (Map.lookup n.name centers) | (n, s, rows) <- sized]
  pure (scene 12 (linkItems <> nodeItems))
  where
    shown r = "<<" <> r.kind <> ">>"
    headOr d xs = case xs of
      x : _ -> x
      [] -> d
    lastOr d xs = if null xs then d else last xs

box :: Point -> Node -> Size -> [Text] -> [Item]
box c n s rows =
  [ Rect {at = Point l t, size = s, radius = 4, fill = Just (if n.kind == "Element" then Tint 1 else Tint 0), edge = Just (pen Border 1.2)}
  , Text {at = Point c.x (t + 14), text = "<<" <> n.kind <> ">>", font = regular 11, color = Muted, anchor = Middle}
  , Text {at = Point c.x (t + 32), text = n.name, font = bold 13, color = Ink, anchor = Middle} ]
    <> [Path {start = Point l (t + 46), segments = [LineTo (Point (l + s.w) (t + 46))], line = pen Border 1, begin = NoHead, end = NoHead} | not (null rows)]
    <> [Text {at = Point (l + 12) (t + 62 + 17 * fromIntegral i), text = r, font = regular 12, color = Ink, anchor = Start} | (i, r) <- zip [0 :: Int ..] rows]
  where
    l = c.x - s.w / 2
    t = c.y - s.h / 2
