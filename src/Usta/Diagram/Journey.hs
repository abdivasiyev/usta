{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid user journeys: tasks left to right under their sections, each
with a score from 1 to 5 shown as a face, higher when happier, and the
people taking part.
-}
module Usta.Diagram.Journey
    ( Journey (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.List (nub)
import Data.Text (Text)
import Data.Traversable (for)
import Text.Read (readMaybe)

import Data.Text qualified as T

import Usta.Diagram.Scene

type Step :: Type
data Step = Step {section :: Text, name :: Text, score :: Int, people :: [Text]}

type Journey :: Type
data Journey = Journey {title :: Maybe Text, steps :: [Step]}

parse :: Text -> Either Text Journey
parse source = go Nothing "" [] (drop 1 (filter (not . T.null) (map T.strip (T.lines source))))
    where
        go title _ acc [] = if null acc then Left "The journey has no tasks." else Right Journey {title = title, steps = reverse acc}
        go title section acc (l : rest)
            | Just t <- T.stripPrefix "title " l = go (Just (T.strip t)) section acc rest
            | Just s <- T.stripPrefix "section " l = go title (T.strip s) acc rest
            | [n, s, ps] <- map T.strip (T.splitOn ":" l)
            , Just v <- readMaybe (T.unpack s) =
                go title section (Step section n (max 1 (min 5 v)) (filter (not . T.null) (map T.strip (T.splitOn "," ps))) : acc) rest
            | [n, s] <- map T.strip (T.splitOn ":" l)
            , Just v <- readMaybe (T.unpack s) =
                go title section (Step section n (max 1 (min 5 v)) [] : acc) rest
            | otherwise = Left ("usta could not read \"" <> T.take 40 l <> "\" in the journey; a task is \"Name: score: people\".")

draw :: Monad m => Measure m -> Journey -> m Scene
draw measure j = do
    sizes <- for j.steps $ \s -> measure (regular 12) s.name
    let
        width = maximum (100 : map (\z -> z.w + 24) sizes)
        gap = 16
        people = nub (concatMap (.people) j.steps)
        colorOf p = Series (length (takeWhile (/= p) people) + 1)
        top = maybe 0 (const 34) j.title
        xOf i = fromIntegral i * (width + gap)
        cardTop = top + 34
        cardH = 44
        faceTop = cardTop + cardH + 24
        faceY s = faceTop + fromIntegral (5 - s) * 26
        sections = [(s, [i | (i, st) <- zip [0 :: Int ..] j.steps, st.section == s]) | s <- nub (map (.section) j.steps)]
        bands =
            concat
                [ [ Rect {at = Point (xOf (head' is)) top, size = Size (xOf (last' is) + width - xOf (head' is)) 26, radius = 6, fill = Just (Series k), edge = Nothing}
                  , Text {at = Point ((xOf (head' is) + xOf (last' is) + width) / 2) (top + 13), text = s, font = bold 12, color = OnSeries, anchor = Middle}
                  ]
                | (k, (s, is)) <- zip [0 ..] sections
                , not (T.null s)
                , not (null is)
                ]
        cards =
            concat
                [ [ Rect {at = Point (xOf i) cardTop, size = Size width cardH, radius = 6, fill = Just Paper, edge = Just (pen Border 1.2)}
                  , Text {at = Point (xOf i + width / 2) (cardTop + cardH / 2), text = st.name, font = regular 12, color = Ink, anchor = Middle}
                  , Path {start = Point (xOf i + width / 2) (cardTop + cardH), segments = [LineTo (Point (xOf i + width / 2) (faceY 1 + 14))], line = dashed (pen Border 1), begin = NoHead, end = NoHead}
                  ]
                    <> face (Point (xOf i + width / 2) (faceY st.score)) st.score
                    <> [ Ellipse {center = Point (xOf i + width / 2 - fromIntegral (length st.people - 1) * 7 + fromIntegral k * 14) (faceY 1 + 30), rx = 5, ry = 5, fill = Just (colorOf p), edge = Nothing}
                       | (k, p) <- zip [0 :: Int ..] st.people
                       ]
                | (i, st) <- zip [0 :: Int ..] j.steps
                ]
        legendY = faceY 1 + 56
        legend =
            concat
                [ [ Ellipse {center = Point (fromIntegral k * 110 + 6) legendY, rx = 5, ry = 5, fill = Just (colorOf p), edge = Nothing}
                  , Text {at = Point (fromIntegral k * 110 + 16) legendY, text = p, font = regular 12, color = Ink, anchor = Start}
                  ]
                | (k, p) <- zip [0 :: Int ..] people
                ]
        titleItem = [Text {at = Point (xOf (length j.steps) / 2) 12, text = t, font = bold 14, color = Ink, anchor = Middle} | Just t <- [j.title]]
    pure (scene 12 (bands <> cards <> legend <> titleItem))
    where
        head' xs = case xs of
            x : _ -> x
            [] -> 0
        last' xs = if null xs then 0 else last xs

{- | A face for the score: a smile at 4 and 5, a flat mouth at 3, a frown
below.
-}
face :: Point -> Int -> [Item]
face c s =
    [ Ellipse {center = c, rx = 13, ry = 13, fill = Just tint, edge = Just (pen tone 1.4)}
    , Ellipse {center = Point (c.x - 4.5) (c.y - 3), rx = 1.6, ry = 1.6, fill = Just tone, edge = Nothing}
    , Ellipse {center = Point (c.x + 4.5) (c.y - 3), rx = 1.6, ry = 1.6, fill = Just tone, edge = Nothing}
    , Path {start = Point (c.x - 5) (c.y + 4), segments = [CurveTo (Point (c.x - 2) (c.y + 4 + bend)) (Point (c.x + 2) (c.y + 4 + bend)) (Point (c.x + 5) (c.y + 4))], line = pen tone 1.4, begin = NoHead, end = NoHead}
    ]
    where
        (tone, tint) = if s >= 4 then (Series 1, Tint 1) else if s == 3 then (Series 4, Tint 4) else (Danger, DangerTint)
        bend = if s >= 4 then 4 else if s == 3 then 0 else -4
