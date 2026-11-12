{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid entity relationship diagrams: entities with their typed
attributes and keys, and relationships with crow's-foot cardinalities
at each end, laid out in ranks.
-}
module Usta.Diagram.Entity
    ( Entities (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Traversable (for)

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Usta.Diagram.Layered (Box (..), Layout (..), Link (..), Placed (..), Routed (..), layered, spacing)
import Usta.Diagram.Scene

import Usta.Diagram.Route qualified as Route

type Attribute :: Type
data Attribute = Attribute {kind :: Text, name :: Text, keys :: Text, comment :: Text}

type Entity :: Type
data Entity = Entity {name :: Text, label :: Text, attributes :: [Attribute]}

type Relationship :: Type
data Relationship = Relationship {left :: Text, right :: Text, leftEnd :: Head, rightEnd :: Head, identifying :: Bool, label :: Text}

type Entities :: Type
data Entities = Entities {across :: Bool, entities :: [Entity], relationships :: [Relationship]}

parse :: Text -> Either Text Entities
parse source = do
    (found, order, rels) <- go Map.empty [] [] Nothing (drop 1 (filter (not . T.null) (map T.strip (T.lines source))))
    let
        entityOf n = Map.findWithDefault (Entity n n []) n found
    pure
        Entities
            { across = any ((`elem` ["direction LR", "direction RL"]) . T.strip) (T.lines source)
            , entities = map entityOf (nub (reverse order))
            , relationships = reverse rels
            }
    where
        go found order rels open lines' = case lines' of
            [] -> Right (found, order, rels)
            line : rest -> case open of
                Just n
                    | line == "}" -> go found order rels Nothing rest
                    | otherwise -> go (Map.adjust (\e -> e {attributes = e.attributes <> [attribute line]}) n found) order rels open rest
                Nothing
                    | any (`T.isPrefixOf` line) ["direction ", "style ", "classDef ", "class "] -> go found order rels Nothing rest
                    | Just r <- relationship line -> go (ensure r.right (ensure r.left found)) (r.right : r.left : order) (r : rels) Nothing rest
                    | "{" `T.isSuffixOf` line ->
                        let
                            (n, l) = named (T.strip (T.dropEnd 1 line))
                        in
                            go (Map.insert n (Entity n l (maybe [] (.attributes) (Map.lookup n found))) found) (n : order) rels (Just n) rest
                    | [n] <- T.words line -> let (n', l) = named n in go (Map.insertWith (\_ old -> old) n' (Entity n' l []) found) (n' : order) rels Nothing rest
                    | otherwise -> Left ("usta could not read \"" <> T.take 40 line <> "\" in the ER diagram.")
        ensure n m = if Map.member n m then m else Map.insert n (Entity n n []) m
        -- "p[Person]" names p and shows Person.
        named t = case T.breakOn "[" t of
            (n, alias) | not (T.null alias) -> (T.strip n, T.dropAround (`elem` ("[]\" " :: String)) alias)
            _ -> (t, t)

-- | "string name PK, FK \"comment\"".
attribute :: Text -> Attribute
attribute line =
    let
        (before, quoted) = T.breakOn "\"" line
        ws = T.words before
    in
        case ws of
            k : n : ks -> Attribute k n (T.intercalate ", " (filter (not . T.null) (map (T.strip . T.dropWhileEnd (== ',')) ks))) (T.dropAround (== '"') quoted)
            [k] -> Attribute k "" "" (T.dropAround (== '"') quoted)
            [] -> Attribute "" "" "" ""

-- | "A ||--o{ B : label", the cardinality marks at each end and the line.
relationship :: Text -> Maybe Relationship
relationship line = do
    let
        (spec, labelPart) = T.breakOn ":" line
    case T.words spec of
        [a, token, b] | T.length token >= 6 -> do
            let
                leftMark = T.take 2 token
                rightMark = T.takeEnd 2 token
                middle = T.drop 2 (T.dropEnd 2 token)
            l <- leftOf leftMark
            r <- rightOf rightMark
            if middle `elem` ["--", ".."]
                then Just Relationship {left = a, right = b, leftEnd = l, rightEnd = r, identifying = middle == "--", label = T.dropAround (`elem` ("\" " :: String)) (T.drop 1 labelPart)}
                else Nothing
        _ -> Nothing
    where
        leftOf m = case m of
            "|o" -> Just ZeroOrOne
            "||" -> Just One
            "}o" -> Just ZeroOrMany
            "}|" -> Just OneOrMany
            _ -> Nothing
        rightOf m = case m of
            "o|" -> Just ZeroOrOne
            "||" -> Just One
            "o{" -> Just ZeroOrMany
            "|{" -> Just OneOrMany
            _ -> Nothing

draw :: Monad m => Moves -> Measure m -> Entities -> m Scene
draw drags measure d = do
    sized <- for d.entities $ \e -> do
        title <- measure (bold 13) e.label
        cells <- for e.attributes $ \a -> do
            k <- measure (regular 11) a.kind
            n <- measure (regular 12) a.name
            ks <- measure (bold 11) a.keys
            c <- measure (regular 11) a.comment
            pure (k.w, n.w, ks.w, c.w)
        let
            col f = maximum (0 : map f cells)
            widths = (col (\(a, _, _, _) -> a), col (\(_, b, _, _) -> b), col (\(_, _, c, _) -> c), col (\(_, _, _, x) -> x))
            (w1, w2, w3, w4) = widths
            rowsW = w1 + w2 + w3 + w4 + 24 + 14 * fromIntegral (length (filter (> 0) [w3, w4]) + 1)
            width = max (title.w + 32) rowsW
            height = 30 + 22 * fromIntegral (length e.attributes) + (if null e.attributes then 0 else 6)
        pure (e, Size width height, widths)
    labels <- for d.relationships $ \r -> measure (regular 11) r.label
    let
        turn z = if d.across then Size z.h z.w else z
        result = layered spacing [Box e.name (turn z) | (e, z, _) <- sized] [Link r.left r.right (if T.null r.label then 0 else 10 + (if d.across then l.w else l.h)) | (r, l) <- zip d.relationships labels]
        place p = if d.across then Point p.y p.x else p
        centers = Map.mapWithKey (dragged drags) (Map.fromList [(p.key, place p.center) | p <- result.placed])
        sizes = Map.fromList [(e.name, z) | (e, z, _) <- sized]
        routes = Map.fromListWith (flip (<>)) [((r.from, r.to), [Route.unbent drags centers r.from r.to (map place r.points)]) | r <- result.routed]
        taken = snd (foldl' take' (routes, []) (zip d.relationships labels))
        take' (left', acc) (r, l) = case Map.lookup (r.left, r.right) left' of
            Just (ps : more) -> (Map.insert (r.left, r.right) more left', acc <> [(r, l, ps)])
            _ -> (left', acc)
        drawn = catMaybes [link r l ps | ((r, l, _), ps) <- zip taken (Route.spread [(r.left, r.right, ps) | (r, _, ps) <- taken])]
        keepOff = [Route.around c z | (e, z, _) <- sized, Just c <- [Map.lookup e.name centers]]
        spots = Route.labelSpots keepOff [(path, size) | (path, _, size, _) <- drawn]
        links = concat [line' <> maybe [] labelOf spot | ((_, line', _, labelOf), spot) <- zip drawn spots]
        link r l ps = do
            c0 <- Map.lookup r.left centers
            z0 <- Map.lookup r.left sizes
            c1 <- Map.lookup r.right centers
            z1 <- Map.lookup r.right sizes
            let
                inner = drop 1 (take (length ps - 1) ps)
                a = Route.clip Route.Boxy c0 z0 (case inner of x : _ -> x; [] -> c1) 0
                b = Route.clip Route.Boxy c1 z1 (if null inner then c0 else last inner) 0
                path = a : inner <> [b]
            pure
                ( path
                , [Path {start = a, segments = Route.smooth path, line = (if r.identifying then id else dashed) (pen Line 1.3), begin = r.leftEnd, end = r.rightEnd}]
                , if T.null r.label then Nothing else Just l
                , \mid ->
                    [ Rect {at = Point (mid.x - l.w / 2 - 4) (mid.y - l.h / 2 - 1), size = Size (l.w + 8) (l.h + 2), radius = 3, fill = Just Paper, edge = Nothing}
                    , Text {at = mid, text = r.label, font = regular 11, color = Muted, anchor = Middle}
                    ]
                )
        boxes = concat [entityBox p e z ws | (e, z, ws) <- sized, Just p <- [Map.lookup e.name centers]]
    pure (scene 12 (links <> boxes <> [handle e.name c z | (e, z, _) <- sized, Just c <- [Map.lookup e.name centers]]))

entityBox :: Point -> Entity -> Size -> (Double, Double, Double, Double) -> [Item]
entityBox c e z (w1, w2, w3, _) =
    [ Rect {at = Point l t, size = z, radius = 6, fill = Just Paper, edge = Just (pen Border 1.2)}
    , Rect {at = Point l t, size = Size z.w 30, radius = 6, fill = Just (Tint 1), edge = Just (pen Border 1.2)}
    , Text {at = Point c.x (t + 15), text = e.label, font = bold 13, color = Ink, anchor = Middle}
    ]
        <> concat
            [ [ Text {at = Point (l + 12) y, text = a.kind, font = regular 11, color = Muted, anchor = Start}
              , Text {at = Point (l + 12 + w1 + 14) y, text = a.name, font = regular 12, color = Ink, anchor = Start}
              , Text {at = Point (l + 12 + w1 + 14 + w2 + 14) y, text = a.keys, font = bold 11, color = Series 0, anchor = Start}
              , Text {at = Point (l + 12 + w1 + 14 + w2 + 14 + w3 + (if w3 > 0 then 14 else 0)) y, text = a.comment, font = regular 11, color = Muted, anchor = Start}
              ]
            | (i, a) <- zip [0 :: Int ..] e.attributes
            , let
                y = t + 30 + 14 + 22 * fromIntegral i
            ]
    where
        l = c.x - z.w / 2
        t = c.y - z.h / 2
