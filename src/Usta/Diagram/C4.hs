{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid C4 diagrams (context, container, component, dynamic and
deployment): people, systems, containers and components as filled
boxes, boundaries and deployment nodes as dashed frames around what
they hold, laid out in rows as Mermaid does, and relationships as
straight labelled arrows between them.
-}
module Usta.Diagram.C4
    ( C4 (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Text.Read (readMaybe)

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Usta.Diagram.Scene

import Usta.Diagram.Route qualified as Route

type Store :: Type
data Store = Plain | Database | Queue
    deriving stock (Eq)

-- | A person, system, container or component.
type Element :: Type
data Element = Element
    { alias :: Text
    , label :: Text
    , kind :: Text
    , technology :: Maybe Text
    , description :: Maybe Text
    , external :: Bool
    , store :: Store
    }

type Part :: Type
data Part
    = Single Element
    | Boundary {alias :: Text, label :: Text, kind :: Text, parts :: [Part]}

type Rel :: Type
data Rel = Rel {from :: Text, to :: Text, label :: Text, technology :: Maybe Text, both :: Bool}

type C4 :: Type
data C4 = C4 {title :: Maybe Text, parts :: [Part], rels :: [Rel], shapesInRow :: Int, boundariesInRow :: Int}

{- | What is open while reading: each boundary not yet closed, with the
parts read inside it so far, newest first.
-}
type Open :: Type
data Open = Open (Maybe (Text, Text, Text)) [Part]

parse :: Text -> Either Text C4
parse source = go (drop 1 lines') [Open Nothing []] (C4 Nothing [] [] 4 2)
    where
        lines' = [l | l <- map T.strip (T.lines source), not (T.null l), not ("%%" `T.isPrefixOf` l)]
        go ls stack c4 = case (ls, stack) of
            ([], [Open _ ps])
                | null ps -> Left "The C4 diagram has no elements."
                | otherwise -> Right c4 {parts = reverse ps, rels = reverse c4.rels}
            ([], _) -> Left "A boundary in the C4 diagram is not closed with }."
            (l : rest, _)
                | Just t <- T.stripPrefix "title " l -> go rest stack c4 {title = Just (T.strip t)}
                | l == "}" -> case stack of
                    Open (Just (a, n, k)) ps : Open h outer : more -> go rest (Open h (Boundary a n k (reverse ps) : outer) : more) c4
                    _ -> Left "A } in the C4 diagram closes nothing."
                | otherwise -> do
                    let
                        (name, args, named) = call l
                        opens = "{" `T.isSuffixOf` l
                        arg i = listToMaybe (drop i args)
                        bad = Left ("usta could not read \"" <> T.take 40 l <> "\" in the C4 diagram.")
                    case () of
                        _
                            | name == "UpdateLayoutConfig" ->
                                let
                                    count k d = fromMaybe d (lookup k named >>= readMaybe . T.unpack)
                                in
                                    go rest stack c4 {shapesInRow = max 1 (count "c4ShapeInRow" c4.shapesInRow), boundariesInRow = max 1 (count "c4BoundaryInRow" c4.boundariesInRow)}
                            | any (`T.isPrefixOf` name) ["Update", "Add"] -> go rest stack c4
                            | Just k <- boundary name
                            , Just a <- arg 0
                            , opens ->
                                go rest (Open (Just (a, fromMaybe a (arg 1), fromMaybe k (if name == "Boundary" then arg 2 else Nothing))) [] : stack) c4
                            | Just (both, skip) <- relation name
                            , (f : t : more) <- drop skip args ->
                                let
                                    index = if skip == 1 then maybe "" (<> ": ") (arg 0) else ""
                                in
                                    go rest stack c4 {rels = Rel f t (index <> fromMaybe "" (listToMaybe more)) (listToMaybe (drop 1 more)) both : c4.rels}
                            | Just e <- element name args -> case stack of
                                Open h ps : more -> go rest (Open h (Single e : ps) : more) c4
                                [] -> bad
                            | otherwise -> bad
        -- "Name(a, \"b, c\", $key=\"v\")" as its name, its plain arguments and
        -- its named ones.
        call l =
            let
                (name, rest) = T.breakOn "(" l
                inner = T.dropWhileEnd (/= ')') (T.drop 1 rest)
                fields = map T.strip (splitArgs (T.dropEnd 1 inner))
                isNamed f = "$" `T.isPrefixOf` f
                plain = [unquote f | f <- fields, not (isNamed f)]
                named = [(T.drop 1 (T.strip k), unquote (T.strip (T.drop 1 v))) | f <- fields, isNamed f, let
                                                                                                            (k, v) = T.breakOn "=" f]
            in
                (T.strip name, plain, named)
        splitArgs t
            | T.null (T.strip t) = []
            | otherwise = go' False "" (T.unpack t)
            where
                go' _ acc [] = [T.pack (reverse acc)]
                go' q acc (ch : more)
                    | ch == '"' = go' (not q) (ch : acc) more
                    | ch == ',' && not q = T.pack (reverse acc) : go' q "" more
                    | otherwise = go' q (ch : acc) more
        unquote = T.dropAround (== '"') . T.strip
        boundary name =
            lookup
                name
                [ ("Boundary", "boundary")
                , ("Enterprise_Boundary", "Enterprise")
                , ("System_Boundary", "System")
                , ("Container_Boundary", "Container")
                , ("Deployment_Node", "Node")
                , ("Node", "Node")
                , ("Node_L", "Node")
                , ("Node_R", "Node")
                ]
        relation name
            | name == "BiRel" = Just (True, 0)
            | name == "RelIndex" = Just (False, 1)
            | name `elem` ["Rel", "Rel_U", "Rel_Up", "Rel_D", "Rel_Down", "Rel_L", "Rel_Left", "Rel_R", "Rel_Right", "Rel_Back"] = Just (False, 0)
            | otherwise = Nothing
        element name args = do
            let
                (base0, external) = maybe (name, False) (,True) (T.stripSuffix "_Ext" name)
                (base, store) = case (T.stripSuffix "Db" base0, T.stripSuffix "Queue" base0) of
                    (Just b, _) -> (b, Database)
                    (_, Just b) -> (b, Queue)
                    _ -> (base0, Plain)
                withTechnology = base `elem` ["Container", "Component"]
            _ <- if base `elem` ["Person", "System", "Container", "Component"] then Just () else Nothing
            alias <- listToMaybe args
            let
                at i = mfilter' (listToMaybe (drop i args))
            pure
                Element
                    { alias
                    , label = fromMaybe alias (at 1)
                    , kind = (if external then "External " else "") <> base
                    , technology = if withTechnology then at 2 else Nothing
                    , description = at (if withTechnology then 3 else 2)
                    , external
                    , store
                    }
        mfilter' m = case m of
            Just t | not (T.null t) -> Just t
            _ -> Nothing

{- | Something laid out at the origin: its size, what to draw, and where
each element's box is, for the relationships.
-}
type Laid :: Type
data Laid = Laid {framed :: Bool, size :: Size, items :: [Item], spots :: [(Text, (Point, Size))]}

shift :: Double -> Double -> Laid -> Laid
shift dx dy l = l {items = map (translate dx dy) l.items, spots = [(k, (Point (c.x + dx) (c.y + dy), s)) | (k, (c, s)) <- l.spots]}

draw :: Monad m => Measure m -> C4 -> m Scene
draw measure c4 = do
    body <- grid =<< for c4.parts part
    relItems <- concat <$> for (offsets c4.rels) (\(r, k) -> rel (Map.fromList body.spots) r k)
    let
        top = maybe 0 (const 36) c4.title
        titleItem = [Text {at = Point 0 10, text = t, font = bold 15, color = Ink, anchor = Start} | Just t <- [c4.title]]
    pure (scene 12 (map (translate 0 top) (body.items <> relItems) <> titleItem))
    where
        part = \case
            Single e -> element e
            Boundary {alias, label, kind, parts} -> do
                inner <- grid =<< for parts part
                let
                    w = max 200 (inner.size.w + 32)
                    h = inner.size.h + 56
                    framed = shift 16 44 inner
                pure
                    Laid
                        { framed = True
                        , size = Size w h
                        , items =
                            [ Rect {at = Point 0 0, size = Size w h, radius = 8, fill = Nothing, edge = Just (dashed (pen Muted 1.2))}
                            , Text {at = Point 12 16, text = label, font = bold 13, color = Ink, anchor = Start}
                            , Text {at = Point 12 32, text = "[" <> kind <> "]", font = regular 11, color = Muted, anchor = Start}
                            ]
                                <> framed.items
                        , spots = (alias, (Point (w / 2) (h / 2), Size w h)) : framed.spots
                        }
        -- Elements in rows of 'shapesInRow', then boundaries in rows of
        -- 'boundariesInRow' under them.
        grid laid = do
            let
                rows = chunks c4.shapesInRow (filter (not . (.framed)) laid) <> chunks c4.boundariesInRow (filter (.framed) laid)
                placeRow (y, acc) row =
                    let
                        h = maximum (0 : [l.size.h | l <- row])
                        xs = scanl (\x l -> x + l.size.w + gapX) 0 row
                    in
                        (y + h + gapY, acc <> zipWith (\x l -> shift x y l) xs row)
                (bottom, placed) = foldl placeRow (0, []) rows
                width = maximum (0 : [x | row <- rows, let
                                                        x = sum [l.size.w + gapX | l <- row] - gapX])
            pure Laid {framed = False, size = Size width (max 0 (bottom - gapY)), items = concatMap (.items) placed, spots = concatMap (.spots) placed}
        gapX = 136
        gapY = 72
        element e = do
            description <- maybe (pure []) (wrapped 190) e.description
            let
                isPerson = e.kind `elem` ["Person", "External Person"]
                -- Room for the person's figure, or below a cylinder's rim.
                icon = (if isPerson then 30 else 0) + (if e.store == Database then 10 else 0)
                w = 216
                rows =
                    [(e.label, bold 13, 18)]
                        <> [("[" <> t <> "]", regular 11, 16) | Just t <- [e.technology]]
                        <> [(d, regular 11, 15) | d <- description]
                h = max 72 (icon + 30 + sum [r | (_, _, r) <- rows] + 10)
                fillRole = if e.external then Muted else Series 0
                rim = Just (pen Paper 1.2)
                shape = case e.store of
                    Plain -> [Rect {at = Point 0 0, size = Size w h, radius = 10, fill = Just fillRole, edge = Nothing}]
                    Database ->
                        [ Ellipse {center = Point (w / 2) (h - 8), rx = w / 2, ry = 8, fill = Just fillRole, edge = Nothing}
                        , Rect {at = Point 0 8, size = Size w (h - 16), radius = 0, fill = Just fillRole, edge = Nothing}
                        , Ellipse {center = Point (w / 2) 8, rx = w / 2, ry = 8, fill = Just fillRole, edge = rim}
                        ]
                    Queue ->
                        [ Ellipse {center = Point 10 (h / 2), rx = 10, ry = h / 2, fill = Just fillRole, edge = Nothing}
                        , Rect {at = Point 10 0, size = Size (w - 20) h, radius = 0, fill = Just fillRole, edge = Nothing}
                        , Ellipse {center = Point (w - 10) (h / 2), rx = 10, ry = h / 2, fill = Just fillRole, edge = rim}
                        ]
                person =
                    [ Ellipse {center = Point (w / 2) 24, rx = 12, ry = 6, fill = Just OnSeries, edge = Nothing}
                    , Ellipse {center = Point (w / 2) 12, rx = 6, ry = 6, fill = Just OnSeries, edge = Nothing}
                    ]
                ys = scanl (+) (icon + 32) [r | (_, _, r) <- rows]
                texts =
                    Text {at = Point (w / 2) (icon + 14), text = "<<" <> T.toLower (T.replace " " "_" e.kind) <> (case e.store of Database -> "_db"; Queue -> "_queue"; Plain -> "") <> ">>", font = regular 10, color = OnSeries, anchor = Middle}
                        : [Text {at = Point (w / 2) y, text = t, font = f, color = OnSeries, anchor = Middle} | ((t, f, _), y) <- zip rows ys]
            pure Laid {framed = False, size = Size w h, items = shape <> (if isPerson then person else []) <> texts, spots = [(e.alias, (Point (w / 2) (h / 2), Size w h))]}
        -- Lines no wider than @width@; "<br/>" breaks one.
        wrapped width t = concat <$> for (T.splitOn "<br/>" (T.replace "<br>" "<br/>" t)) (wrap measure (regular 11) width)
        -- Relationships between the same two boxes side by side, not on top
        -- of each other.
        offsets rs =
            let
                key r = (min r.from r.to, max r.from r.to)
                counts = Map.fromListWith (+) [(key r, 1 :: Int) | r <- rs]
                step (seen, acc) r =
                    let
                        n = Map.findWithDefault (0 :: Int) (key r) seen
                    in
                        (Map.insert (key r) (n + 1) seen, acc <> [(r, fromIntegral n - fromIntegral (counts Map.! key r - 1) / 2)])
            in
                snd (foldl step (Map.empty, []) rs)
        rel spots r k = fromMaybe (pure []) $ do
            (c0, s0) <- Map.lookup r.from spots
            (c1, s1) <- Map.lookup r.to spots
            pure $ do
                let
                    dx = c1.x - c0.x
                    dy = c1.y - c0.y
                    len = max 1 (sqrt (dx * dx + dy * dy))
                    off = Point (negate dy / len * 18 * k) (dx / len * 18 * k)
                    p0 = Point (c0.x + off.x) (c0.y + off.y)
                    p1 = Point (c1.x + off.x) (c1.y + off.y)
                    a = Route.clip Route.Boxy p0 s0 p1 2
                    b = Route.clip Route.Boxy p1 s1 p0 2
                    m = Point ((a.x + b.x) / 2) ((a.y + b.y) / 2)
                label <- wrapped 124 r.label
                technology <- maybe (pure []) (wrapped 124 . (\t -> "[" <> t <> "]")) r.technology
                let
                    texts = [(t, regular 11) | t <- label] <> [(t, regular 10) | t <- technology]
                sizes <- for texts (\(t, f) -> measure f t)
                let
                    w = maximum (0 : map (.w) sizes)
                    h = sum (map (\s -> s.h + 2) sizes)
                    ys = scanl (\y s -> y + s.h + 2) (m.y - h / 2) sizes
                pure
                    $ [Path {start = a, segments = [LineTo b], line = pen Line 1.2, begin = if r.both then Arrow else NoHead, end = Arrow}]
                        <> [Rect {at = Point (m.x - w / 2 - 4) (m.y - h / 2 - 2), size = Size (w + 8) (h + 4), radius = 3, fill = Just Paper, edge = Nothing} | not (null texts)]
                        <> [Text {at = Point m.x (y + s.h / 2 + 1), text = t, font = f, color = Muted, anchor = Middle} | ((t, f), s, y) <- zip3 texts sizes ys]

chunks :: Int -> [a] -> [[a]]
chunks n xs = case splitAt n xs of
    ([], _) -> []
    (row, rest) -> row : chunks n rest
