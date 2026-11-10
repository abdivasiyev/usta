{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid git graphs: commits left to right, one lane per branch,
branches leaving and merges joining, with ids, tags and commit types.
-}
module Usta.Diagram.GitGraph
    ( GitGraph (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Text.Read (readMaybe)

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Usta.Diagram.Scene

type Look :: Type
data Look = Normal | Highlight | Reverse
    deriving stock (Eq)

type Commit :: Type
data Commit = Commit
    { key :: Text
    , branch :: Text
    , column :: Int
    , parents :: [Text]
    , tag :: Maybe Text
    , look :: Look
    , shown :: Bool
    -- ^ Whether the id was given, so it is worth printing.
    }

type GitGraph :: Type
data GitGraph = GitGraph {branches :: [Text], commits :: [Commit]}

type Walk :: Type
data Walk = Walk
    { current :: Text
    , heads :: Map.Map Text Text
    -- ^ Each branch's last commit.
    , made :: [Text]
    -- ^ Branches, newest first, with their order when given.
    , orders :: Map.Map Text Int
    , done :: [Commit]
    , next :: Int
    }

parse :: Text -> Either Text GitGraph
parse source = do
    w <- foldl' (\acc l -> acc >>= \x -> step x l) (Right start) body
    let
        ordered = sortOn (\b -> (Map.findWithDefault 0 b w.orders, length (takeWhile (/= b) (reverse w.made)))) (reverse w.made)
    if null w.done then Left "The git graph has no commits." else pure GitGraph {branches = ordered, commits = reverse w.done}
    where
        body = drop 1 (filter (not . T.null) (map T.strip (T.lines source)))
        start = Walk {current = "main", heads = Map.empty, made = ["main"], orders = Map.empty, done = [], next = 0}

step :: Walk -> Text -> Either Text Walk
step w l = case T.words l of
    "commit" : _ -> Right (add w (attr "id") (attr "tag") (lookOf (attr "type")) [p | Just p <- [Map.lookup w.current w.heads]])
    "branch" : name : _ ->
        let
            order = attr "order" >>= readMaybe . T.unpack
        in
            Right
                w
                    { current = name
                    , made = name : w.made
                    , orders = maybe w.orders (\o -> Map.insert name o w.orders) order
                    , heads = maybe w.heads (\h -> Map.insert name h w.heads) (Map.lookup w.current w.heads)
                    }
    verb : name : _
        | verb `elem` ["checkout", "switch"] ->
            if name `elem` w.made then Right w {current = name} else Left ("No branch " <> name <> " to check out.")
    "merge" : name : _ -> case Map.lookup name w.heads of
        Nothing -> Left ("No branch " <> name <> " to merge.")
        Just h -> Right (add w (attr "id") (attr "tag") (lookOf (attr "type")) ([p | Just p <- [Map.lookup w.current w.heads]] <> [h]))
    "cherry-pick" : _ -> case attr "id" of
        Just source' -> Right (add w Nothing (Just (fromMaybe ("cherry-pick " <> source') (attr "tag"))) Highlight [p | Just p <- [Map.lookup w.current w.heads]])
        Nothing -> Left "A cherry-pick names the commit's id."
    _
        | "%%" `T.isPrefixOf` l -> Right w
        | otherwise -> Left ("usta could not read \"" <> T.take 40 l <> "\" in the git graph.")
    where
        -- id: "x", tag: "y", type: HIGHLIGHT, order: 2
        attr k = case T.breakOn (k <> ":") l of
            (_, rest)
                | not (T.null rest) ->
                    let
                        v = T.stripStart (T.drop (T.length k + 1) rest)
                    in
                        Just $ case T.uncons v of
                            Just ('"', q) -> T.takeWhile (/= '"') q
                            _ -> T.takeWhile (/= ' ') v
            _ -> Nothing
        lookOf t = case t of
            Just "HIGHLIGHT" -> Highlight
            Just "REVERSE" -> Reverse
            _ -> Normal

add :: Walk -> Maybe Text -> Maybe Text -> Look -> [Text] -> Walk
add w given tag look parents =
    let
        k = fromMaybe (T.pack (show w.next)) given
        c = Commit {key = k, branch = w.current, column = w.next, parents = parents, tag = tag, look = look, shown = maybe False (const True) given}
    in
        w {done = c : w.done, heads = Map.insert w.current k w.heads, next = w.next + 1}

draw :: Monad m => Measure m -> GitGraph -> m Scene
draw measure g = do
    tagSizes <- for [(c, t) | c <- g.commits, Just t <- [c.tag]] $ \(c, t) -> (,,) c t <$> measure (regular 10) t
    let
        tags =
            concat
                [ [ Rect {at = Point (p.x - z.w / 2 - 6) (p.y - 34), size = Size (z.w + 12) 16, radius = 3, fill = Just (Tint 4), edge = Just (pen (Series 4) 1)}
                  , Text {at = Point p.x (p.y - 26), text = t, font = regular 10, color = Ink, anchor = Middle}
                  ]
                | (c, t, z) <- tagSizes
                , let
                    p = pointOf c
                ]
    pure (scene 12 (lanes <> links <> dots <> names <> tags <> ids))
    where
        lane b = fromMaybe 0 (lookup b (zip g.branches [0 :: Int ..]))
        colorOf b = Series (lane b)
        pointOf c = Point (110 + fromIntegral c.column * 56) (30 + fromIntegral (lane c.branch) * 50)
        byKey = Map.fromList [(c.key, c) | c <- g.commits]
        spanOf b = case [c.column | c <- g.commits, c.branch == b] of
            [] -> Nothing
            cs -> Just (minimum cs, maximum cs)
        lanes =
            [ Path {start = Point (110 + fromIntegral a * 56) y, segments = [LineTo (Point (110 + fromIntegral z * 56) y)], line = pen (colorOf b) 3, begin = NoHead, end = NoHead}
            | b <- g.branches
            , Just (a, z) <- [spanOf b]
            , let
                y = 30 + fromIntegral (lane b) * 50
            ]
        -- Where a branch leaves and a merge comes back, curved between lanes.
        links =
            concat
                [ [ let
                        a = pointOf p
                        b = pointOf c
                        midX = (a.x + b.x) / 2
                        color' = if p.branch == c.branch then colorOf c.branch else if lane p.branch < lane c.branch then colorOf c.branch else colorOf p.branch
                    in
                        Path {start = a, segments = [CurveTo (Point midX a.y) (Point midX b.y) b], line = pen color' 3, begin = NoHead, end = NoHead}
                  | pk <- c.parents
                  , Just p <- [Map.lookup pk byKey]
                  , p.branch /= c.branch || p.column + 1 /= c.column
                  ]
                | c <- g.commits
                ]
        dots =
            concat
                [ case c.look of
                    Highlight -> [Rect {at = Point (p.x - 8) (p.y - 8), size = Size 16 16, radius = 3, fill = Just (colorOf c.branch), edge = Just (pen Paper 2)}]
                    Reverse ->
                        [ Ellipse {center = p, rx = 8, ry = 8, fill = Just (colorOf c.branch), edge = Nothing}
                        , Path {start = Point (p.x - 4) (p.y - 4), segments = [LineTo (Point (p.x + 4) (p.y + 4))], line = pen Paper 2, begin = NoHead, end = NoHead}
                        , Path {start = Point (p.x + 4) (p.y - 4), segments = [LineTo (Point (p.x - 4) (p.y + 4))], line = pen Paper 2, begin = NoHead, end = NoHead}
                        ]
                    Normal
                        | length c.parents > 1 -> [Ellipse {center = p, rx = 8, ry = 8, fill = Just (colorOf c.branch), edge = Just (pen Paper 2)}]
                        | otherwise -> [Ellipse {center = p, rx = 7, ry = 7, fill = Just Paper, edge = Just (pen (colorOf c.branch) 3)}]
                | c <- g.commits
                , let
                    p = pointOf c
                ]
        names =
            concat
                [ [ Rect {at = Point 0 (y - 10), size = Size 86 20, radius = 10, fill = Just (colorOf b), edge = Nothing}
                  , Text {at = Point 43 y, text = b, font = bold 11, color = OnSeries, anchor = Middle}
                  ]
                | b <- g.branches
                , let
                    y = 30 + fromIntegral (lane b) * 50
                ]
        ids = [Text {at = Point p.x (p.y + 18), text = c.key, font = regular 10, color = Muted, anchor = Middle} | c <- g.commits, c.shown, let
                                                                                                                                                p = pointOf c]
