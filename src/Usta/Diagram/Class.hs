{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid class diagrams: classes with their annotation, attributes and
methods, and the relations between them, laid out in ranks like a
flowchart, parents above their children.
-}
module Usta.Diagram.Class
    ( Classes (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Traversable (for)

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Usta.Diagram.Layered (Box (..), Layout (..), Link (..), Placed (..), Routed (..), layered, spacing)
import Usta.Diagram.Scene

import Usta.Diagram.Route qualified as Route

type Class :: Type
data Class = Class {name :: Text, label :: Text, annotation :: Maybe Text, members :: [Text]}

type Relation :: Type
data Relation = Relation
    { left :: Text
    , right :: Text
    , leftHead :: Head
    , rightHead :: Head
    , dotted :: Bool
    , leftCount :: Maybe Text
    , rightCount :: Maybe Text
    , label :: Maybe Text
    }

type Flow :: Type
data Flow = Down | Up | Across | Back

type Classes :: Type
data Classes = Classes {flow :: Flow, classes :: [Class], relations :: [Relation]}

parse :: Text -> Either Text Classes
parse source = do
    (found, order, relations) <- walk Map.empty [] [] Nothing (drop 1 (filter (not . T.null) (map T.strip (T.lines source))))
    let
        classOf n = Map.findWithDefault Class {name = n, label = n, annotation = Nothing, members = []} n found
    pure Classes {flow = flowOf, classes = map classOf (nub (reverse order)), relations = reverse relations}
    where
        flowOf = case [w | l <- T.lines source, ("direction" : w : _) <- [T.words (T.strip l)]] of
            "LR" : _ -> Across
            "RL" : _ -> Back
            "BT" : _ -> Up
            _ -> Down

{- | Reads the lines; @order@ has the classes as they first came, newest
first, and @open@ is the class whose "{ … }" body is being read.
-}
walk :: Map.Map Text Class -> [Text] -> [Relation] -> Maybe Text -> [Text] -> Either Text (Map.Map Text Class, [Text], [Relation])
walk found order relations open lines' = case lines' of
    [] -> Right (found, order, relations)
    line : rest -> case open of
        Just n
            | line == "}" -> walk found order relations Nothing rest
            | Just a <- annotationOf line -> walk (Map.adjust (\c -> c {annotation = Just a}) n found) order relations open rest
            | otherwise -> walk (Map.adjust (\c -> c {members = c.members <> [member line]}) n found) order relations open rest
        Nothing
            | any (`T.isPrefixOf` line) ["direction ", "note ", "click ", "link ", "callback ", "style ", "classDef ", "cssClass "] -> walk found order relations Nothing rest
            | Just spec <- T.stripPrefix "class " line ->
                let
                    body = T.isSuffixOf "{" spec
                    (n, l) = nameOf (T.strip (T.dropWhileEnd (== '{') spec))
                in
                    walk (ensure n l found) (n : order) relations (if body then Just n else Nothing) rest
            | Just a <- annotationOf (T.takeWhile (/= ' ') line)
            , (_ : target : _) <- T.words line ->
                walk (Map.adjust (\c -> c {annotation = Just a}) target (ensure target target found)) (target : order) relations Nothing rest
            | Just r <- relation line -> walk (ensure r.right r.right (ensure r.left r.left found)) (r.right : r.left : order) (r : relations) Nothing rest
            -- "Name : member", for a class named by one word.
            | (n, m) <- T.breakOn ":" line
            , not (T.null m)
            , [n'] <- T.words n ->
                walk (Map.adjust (\c -> c {members = c.members <> [member (T.strip (T.drop 1 m))]}) n' (ensure n' n' found)) (n' : order) relations Nothing rest
            | otherwise -> Left ("usta could not read \"" <> T.take 40 line <> "\" in the class diagram.")
    where
        ensure n l m = if Map.member n m then m else Map.insert n Class {name = n, label = l, annotation = Nothing, members = []} m

annotationOf :: Text -> Maybe Text
annotationOf t = do
    inner <- T.stripPrefix "<<" (T.strip t) >>= T.stripSuffix ">>"
    pure ("«" <> inner <> "»")

-- | "Name", "Name~T~" or "Name[\"Label\"]", as the name and what is shown.
nameOf :: Text -> (Text, Text)
nameOf spec =
    let
        (n, rest) = T.breakOn "[" spec
        base = T.takeWhile (/= '~') (T.strip n)
        generic = T.takeWhile (/= '~') (T.drop 1 (T.dropWhile (/= '~') n))
        shown = if T.null rest then base <> (if T.null generic then "" else "<" <> generic <> ">") else T.dropAround (`elem` ("[]\" " :: String)) rest
    in
        (base, shown)

{- | A member as shown: generics as <T>, the static and abstract marks
dropped.
-}
member :: Text -> Text
member = T.replace "~" "" . T.dropWhileEnd (`elem` ("$*" :: String)) . T.strip

-- | "A \"1\" *-- \"many\" B : label" and every relation Mermaid has.
relation :: Text -> Maybe Relation
relation line = do
    let
        (spec, labelPart) = T.breakOn " : " line
    token <- case [t | t <- T.words spec, isRelation t] of
        t : _ -> Just t
        [] -> Nothing
    let
        (before, after) = T.breakOn token spec
        (leftName, leftCount) = counted (T.strip before) True
        (rightName, rightCount) = counted (T.strip (T.drop (T.length token) after)) False
        dotted = ".." `T.isInfixOf` token
        body = if dotted then ".." else "--"
        (lh, rh) = T.breakOn body token
    pure
        Relation
            { left = leftName
            , right = rightName
            , leftHead = headOf lh
            , rightHead = headOf (T.drop 2 rh)
            , dotted = dotted
            , leftCount = leftCount
            , rightCount = rightCount
            , label = if T.null labelPart then Nothing else Just (T.strip (T.drop 3 labelPart))
            }
    where
        isRelation t = ("--" `T.isInfixOf` t || ".." `T.isInfixOf` t) && T.all (`elem` ("<|*o>.-" :: String)) t
        headOf h = case h of
            "<|" -> Hollow
            "|>" -> Hollow
            "*" -> Diamond True
            "o" -> Diamond False
            "<" -> Arrow
            ">" -> Arrow
            _ -> NoHead
        -- A name with its quoted cardinality next to the relation.
        counted t first =
            let
                quoted = T.breakOn "\"" t
            in
                case T.splitOn "\"" t of
                    [n, c, ""] | first -> (T.strip n, Just c)
                    ["", c, n] | not first -> (T.strip n, Just c)
                    _ -> (T.strip (fst quoted), Nothing)

draw :: Monad m => Moves -> Measure m -> Classes -> m Scene
draw drags measure diagram = do
    sized <- for diagram.classes $ \c -> do
        title <- measure (bold 13) c.label
        note <- traverse (measure (regular 11)) c.annotation
        rows <- for c.members (measure (Font 12 False True))
        let
            width = maximum (title.w : maybe 0 (.w) note : map (.w) rows) + 28
            headH = 28 + maybe 0 (const 16) c.annotation
            attributes = [m | m <- c.members, not ("(" `T.isInfixOf` m)]
            methods = [m | m <- c.members, "(" `T.isInfixOf` m]
            sectionH xs = if null xs then 10 else fromIntegral (length xs) * 18 + 10
            height = headH + sectionH attributes + sectionH methods
        pure (c, Size width height, headH, attributes, methods)
    labels <- for diagram.relations $ \r -> traverse (measure (regular 11)) r.label
    let
        across = case diagram.flow of
            Across -> True
            Back -> True
            _ -> False
        turn s = if across then Size s.h s.w else s
        boxes = [Box {key = c.name, size = turn s} | (c, s, _, _, _) <- sized]
        links = [Link {from = r.left, to = r.right, gap = maybe 0 (\s -> 10 + (if across then s.w else s.h)) l} | (r, l) <- zip diagram.relations labels]
        result = layered spacing boxes links
        bottom = maximum (0 : [p.center.y | p <- result.placed])
        place p = case diagram.flow of
            Down -> p
            Up -> Point p.x (bottom - p.y)
            Across -> Point p.y p.x
            Back -> Point (bottom - p.y) p.x
        centers = Map.mapWithKey (dragged drags) (Map.fromList [(p.key, place p.center) | p <- result.placed])
        sizes = Map.fromList [(c.name, s) | (c, s, _, _, _) <- sized]
        routes = Map.fromListWith (flip (<>)) [((r.from, r.to), [Route.unbent drags centers r.from r.to (map place r.points)]) | r <- result.routed]
        taken = snd (foldl' takeRoute (routes, []) (zip diagram.relations labels))
        takeRoute (left', acc) (r, l) = case Map.lookup (r.left, r.right) left' of
            Just (ps : more) -> (Map.insert (r.left, r.right) more left', acc <> [(r, l, ps)])
            _ -> (left', acc)
        drawn = catMaybes [link' r l ps | ((r, l, _), ps) <- zip taken (Route.spread [(r.left, r.right, ps) | (r, _, ps) <- taken])]
        keepOff = [Route.around c s | (cl, s, _, _, _) <- sized, Just c <- [Map.lookup cl.name centers]]
        spots = Route.labelSpots keepOff [(path, size) | (path, _, size, _) <- drawn]
        linkItems = concat [line' <> maybe [] labelOf spot | ((_, line', _, labelOf), spot) <- zip drawn spots]
        link' r l ps = do
            c0 <- Map.lookup r.left centers
            s0 <- Map.lookup r.left sizes
            c1 <- Map.lookup r.right centers
            s1 <- Map.lookup r.right sizes
            let
                inner = drop 1 (take (length ps - 1) ps)
                a = Route.clip Route.Boxy c0 s0 (headOr c1 inner) 1
                b = Route.clip Route.Boxy c1 s1 (lastOr c0 inner) 1
                path = a : inner <> [b]
                towardA = headOr b (drop 1 path)
                towardB = lastOr a (init path)
                near p q = let dx = q.x - p.x; dy = q.y - p.y; len = max 1 (sqrt (dx * dx + dy * dy)) in Point (p.x + dx / len * 16 + dy / len * 10) (p.y + dy / len * 16 - dx / len * 10)
            pure
                ( path
                , [Path {start = a, segments = Route.smooth path, line = (if r.dotted then dashed else id) (pen Line 1.3), begin = r.leftHead, end = r.rightHead}]
                    <> [Text {at = near a towardA, text = t, font = regular 11, color = Muted, anchor = Middle} | Just t <- [r.leftCount]]
                    <> [Text {at = near b towardB, text = t, font = regular 11, color = Muted, anchor = Middle} | Just t <- [r.rightCount]]
                , r.label >> l
                , \m -> concat [labelAt m t s | (Just t, Just s) <- [(r.label, l)]]
                )
        labelAt p t s =
            [ Rect {at = Point (p.x - s.w / 2 - 4) (p.y - s.h / 2 - 1), size = Size (s.w + 8) (s.h + 2), radius = 3, fill = Just Paper, edge = Nothing}
            , Text {at = p, text = t, font = regular 11, color = Muted, anchor = Middle}
            ]
        classItems = concat (mapMaybe (\(c, s, headH, attributes, methods) -> (\p -> box p c s headH attributes methods) <$> Map.lookup c.name centers) sized)
    pure (scene 12 (linkItems <> classItems <> [handle c.name p s | (c, s, _, _, _) <- sized, Just p <- [Map.lookup c.name centers]]))
    where
        headOr d xs = case xs of
            x : _ -> x
            [] -> d
        lastOr d xs = if null xs then d else last xs

box :: Point -> Class -> Size -> Double -> [Text] -> [Text] -> [Item]
box c cls s headH attributes methods =
    [ Rect {at = Point l t, size = s, radius = 6, fill = Just Paper, edge = Just (pen Border 1.2)}
    , Rect {at = Point l t, size = Size s.w headH, radius = 6, fill = Just (Tint 0), edge = Just (pen Border 1.2)}
    ]
        <> [Text {at = Point c.x (t + 11), text = a, font = regular 11, color = Muted, anchor = Middle} | Just a <- [cls.annotation]]
        <> [Text {at = Point c.x (t + headH / 2 + maybe 0 (const 7) cls.annotation), text = cls.label, font = bold 13, color = Ink, anchor = Middle}]
        <> rows (t + headH + 5) attributes
        <> [Path {start = Point l divider, segments = [LineTo (Point (l + s.w) divider)], line = pen Border 1, begin = NoHead, end = NoHead}]
        <> rows (divider + 5) methods
    where
        l = c.x - s.w / 2
        t = c.y - s.h / 2
        divider = t + headH + (if null attributes then 10 else fromIntegral (length attributes) * 18 + 10)
        rows top xs = [Text {at = Point (l + 12) (top + 9 + 18 * fromIntegral i), text = x, font = Font 12 False True, color = Ink, anchor = Start} | (i, x) <- zip [0 :: Int ..] xs]
