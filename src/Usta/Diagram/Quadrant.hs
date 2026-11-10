{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid quadrant charts: a square in four tinted quarters, named, with
its two axes and points placed by their coordinates from 0 to 1.
-}
module Usta.Diagram.Quadrant
    ( Quadrant (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Text.Read (readMaybe)

import Data.Text qualified as T

import Usta.Diagram.Scene

type Quadrant :: Type
data Quadrant = Quadrant
    { title :: Maybe Text
    , xAxis :: (Text, Text)
    , yAxis :: (Text, Text)
    , names :: [Maybe Text]
    -- ^ Quadrants 1 to 4: top right, top left, bottom left, bottom right.
    , points :: [(Text, Double, Double)]
    }

parse :: Text -> Either Text Quadrant
parse source = do
    let
        lines' = drop 1 (filter (not . T.null) (map T.strip (T.lines source)))
        field key = case [T.strip v | l <- lines', Just v <- [T.stripPrefix key l]] of
            v : _ -> Just v
            [] -> Nothing
        axis key = case field key of
            Just v -> let (a, b) = T.breakOn "-->" v in (T.strip a, T.strip (T.drop 3 b))
            Nothing -> ("", "")
    points <- traverse point [l | l <- lines', ":" `T.isInfixOf` l, "[" `T.isInfixOf` l]
    pure
        Quadrant
            { title = field "title"
            , xAxis = axis "x-axis"
            , yAxis = axis "y-axis"
            , names = [field ("quadrant-" <> T.pack (show (i :: Int))) | i <- [1 .. 4]]
            , points = points
            }
    where
        -- The name runs to the last ":" before the "[", so it may have colons.
        point line =
            let
                before = T.takeWhile (/= '[') line
                n = T.strip (T.dropWhileEnd (== ':') (T.strip before))
                inside = T.takeWhile (/= ']') (T.drop 1 (T.dropWhile (/= '[') line))
            in
                case map (readMaybe . T.unpack . T.strip) (T.splitOn "," inside) of
                    [Just x, Just y] | x >= 0, x <= 1, y >= 0, y <= (1 :: Double) -> Right (n, x, y)
                    _ -> Left ("usta could not read the point \"" <> T.take 40 line <> "\"; points go from 0 to 1.")

draw :: Monad m => Measure m -> Quadrant -> m Scene
draw _ q = pure (scene 12 (titleItem <> quarters <> axes <> dots))
    where
        side = 400 :: Double
        half = side / 2
        top = maybe 0 (const 34) q.title
        left = 30
        -- Quarters 1 to 4 as Mermaid numbers them.
        corners = [(half, 0), (0, 0), (0, half), (half, half)]
        quarters =
            concat
                [ [Rect {at = Point (left + x) (top + y), size = Size half half, radius = 0, fill = Just (Tint i), edge = Just (pen Paper 2)}]
                    <> [Text {at = Point (left + x + half / 2) (top + y + 20), text = n, font = bold 12, color = Ink, anchor = Middle} | Just n <- [name]]
                | (i, ((x, y), name)) <- zip [0 ..] (zip corners q.names)
                ]
        axes =
            [ Text {at = Point (left + half / 2) (top + side + 16), text = fst q.xAxis, font = regular 12, color = Muted, anchor = Middle}
            , Text {at = Point (left + half + half / 2) (top + side + 16), text = snd q.xAxis, font = regular 12, color = Muted, anchor = Middle}
            , Text {at = Point (left - 8) (top + half + half / 2), text = fst q.yAxis, font = regular 12, color = Muted, anchor = End}
            , Text {at = Point (left - 8) (top + half / 2), text = snd q.yAxis, font = regular 12, color = Muted, anchor = End}
            ]
        dots =
            concat
                [ [ Ellipse {center = p, rx = 6, ry = 6, fill = Just (Series i), edge = Just (pen Paper 1.5)}
                  , Text {at = Point (p.x + 10) p.y, text = n, font = regular 11, color = Ink, anchor = Start}
                  ]
                | (i, (n, x, y)) <- zip [0 :: Int ..] q.points
                , let
                    p = Point (left + x * side) (top + (1 - y) * side)
                ]
        titleItem = [Text {at = Point (left + half) 14, text = t, font = bold 14, color = Ink, anchor = Middle} | Just t <- [q.title]]
