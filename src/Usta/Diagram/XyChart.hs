{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid XY charts: bars and lines over categories or a number range,
-- with a value axis, upright or lying down.
module Usta.Diagram.XyChart
  ( XyChart (..)
  , parse
  , draw
  ) where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Usta.Diagram.Scene

type Series' :: Type
data Series' = Bars [Double] | Points [Double]

type XyChart :: Type
data XyChart = XyChart
  { title :: Maybe Text
  , horizontal :: Bool
  , xTitle :: Text
  , categories :: [Text]
  , yTitle :: Text
  , range :: Maybe (Double, Double)
  , series :: [Series']
  }

parse :: Text -> Either Text XyChart
parse source = do
  let ls = filter (not . T.null) (map T.strip (T.lines source))
      header = case ls of
        h : _ -> h
        [] -> ""
      body = drop 1 ls
      field k = case [T.strip v | l <- body, Just v <- [T.stripPrefix k l]] of
        v : _ -> Just v
        [] -> Nothing
      (xTitle, xRest) = titled (fromMaybe "" (field "x-axis"))
      (yTitle, yRest) = titled (fromMaybe "" (field "y-axis"))
  series <- traverse seriesOf [l | l <- body, any (`T.isPrefixOf` l) ["bar ", "bar[", "line ", "line["]]
  let longest = maximum (0 : map (length . valuesOf) series)
      categories = case listOf xRest of
        Just cs -> cs
        Nothing -> case numbers xRest of
          Just (a, b) | longest > 1 -> [number (a + (b - a) * fromIntegral i / fromIntegral (longest - 1)) | i <- [0 .. longest - 1]]
          _ -> [T.pack (show i) | i <- [1 .. longest]]
  if null series then Left "The chart has no bar or line." else
    pure XyChart
      { title = T.dropAround (== '"') <$> field "title"
      , horizontal = "horizontal" `T.isInfixOf` header
      , xTitle = xTitle
      , categories = categories
      , yTitle = yTitle
      , range = numbers yRest
      , series = series
      }
  where
    titled t = case T.uncons t of
      Just ('"', rest) -> let (a, b) = T.breakOn "\"" rest in (a, T.strip (T.drop 1 b))
      _ | "[" `T.isPrefixOf` t || T.any (`elem` ['0' .. '9']) (T.take 1 t) -> ("", t)
        | otherwise -> let (a, b) = T.breakOn " " t in (a, T.strip b)
    listOf t = do
      inner <- T.stripPrefix "[" t >>= T.stripSuffix "]"
      pure (map (T.dropAround (`elem` ("\" " :: String))) (T.splitOn "," inner))
    numbers t = case map (readMaybe . T.unpack . T.strip) (T.splitOn "-->" t) of
      [Just a, Just b] -> Just (a, b)
      _ -> Nothing
    seriesOf l =
      let (kind, rest) = T.breakOn "[" l
          values = traverse (readMaybe . T.unpack . T.strip) (T.splitOn "," (T.dropAround (`elem` ("[] " :: String)) rest))
       in case values of
            Just vs | "bar" `T.isPrefixOf` T.strip kind -> Right (Bars vs)
                    | otherwise -> Right (Points vs)
            Nothing -> Left ("usta could not read the values in \"" <> T.take 40 l <> "\".")
    valuesOf s = case s of
      Bars vs -> vs
      Points vs -> vs

number :: Double -> Text
number v = if v == fromIntegral (round v :: Int) then T.pack (show (round v :: Int)) else T.pack (show v)

draw :: Monad m => Measure m -> XyChart -> m Scene
draw _ c = pure (scene 12 (grid <> marks <> labels <> titles))
  where
    values = concat [vs | s <- c.series, vs <- [case s of { Bars xs -> xs; Points xs -> xs }]]
    (lo, hi) = fromMaybe (min 0 (minimum (0 : values)), maximum (1 : values)) c.range
    (top', step') = niceTop lo hi
    span' = max 0.0001 (top' - lo)
    n = max 1 (length c.categories)
    long = 480 :: Double
    short = 300 :: Double
    titleRoom = maybe 0 (const 34) c.title
    -- Positions along the categories (0 to 1) and the values (0 to 1).
    at u v
      | c.horizontal = Point (90 + v * long) (titleRoom + u * short)
      | otherwise = Point (60 + u * long) (titleRoom + (1 - v) * short)
    slot i = (fromIntegral i + 0.5) / fromIntegral n
    value v = (v - lo) / span'
    ticks = takeWhile (<= top' + 0.0001) [lo + fromIntegral k * step' | k <- [0 :: Int ..]]
    grid = concat
      [ [ Path {start = at 0 (value t), segments = [LineTo (at 1 (value t))], line = pen Border 0.8, begin = NoHead, end = NoHead}
        , Text {at = tickAt (value t), text = number t, font = regular 10, color = Muted, anchor = if c.horizontal then Middle else End} ]
      | t <- ticks ]
      <> [Path {start = at 0 (value lo), segments = [LineTo (at 1 (value lo))], line = pen Line 1.2, begin = NoHead, end = NoHead}]
    -- Value ticks: left of an upright chart, under a lying one.
    tickAt v = if c.horizontal then let p = at 1 v in Point p.x (p.y + 12) else let p = at 0 v in Point (p.x - 8) p.y
    bars = [vs | Bars vs <- c.series]
    lines' = [vs | Points vs <- c.series]
    groupW = 0.7 / fromIntegral n
    marks =
      concat
        [ [ box (slot i - groupW / 2 + groupW * fromIntegral k / fromIntegral (length bars)) (groupW / fromIntegral (length bars)) v k
          | (i, v) <- zip [0 :: Int ..] vs ]
        | (k, vs) <- zip [0 :: Int ..] bars ]
        <> concat
          [ Path {start = at (slot 0) (value (head' vs)), segments = [LineTo (at (slot i) (value v)) | (i, v) <- drop 1 (zip [0 :: Int ..] vs)], line = pen (Series (k + length bars)) 2.2, begin = NoHead, end = NoHead}
              : [Ellipse {center = at (slot i) (value v), rx = 3.5, ry = 3.5, fill = Just Paper, edge = Just (pen (Series (k + length bars)) 2)} | (i, v) <- zip [0 :: Int ..] vs]
          | (k, vs) <- zip [0 :: Int ..] lines', not (null vs) ]
    box u w v k =
      let a = at u (value (max lo 0))
          b = at (u + w) (value v)
       in Rect {at = Point (min a.x b.x) (min a.y b.y), size = Size (abs (b.x - a.x)) (abs (b.y - a.y)), radius = 2, fill = Just (Series k), edge = Nothing}
    labels =
      [ Text {at = categoryAt (slot i), text = cat, font = regular 11, color = Ink, anchor = if c.horizontal then End else Middle}
      | (i, cat) <- zip [0 :: Int ..] c.categories ]
    categoryAt u = let p = at u 0 in if c.horizontal then Point (p.x - 8) p.y else Point p.x (p.y + 14)
    titles =
      [Text {at = Point (60 + long / 2) 12, text = t, font = bold 14, color = Ink, anchor = Middle} | Just t <- [c.title]]
        <> [Text {at = if c.horizontal then Point (90 + long / 2) (titleRoom + short + 34) else Point 30 (titleRoom - 14), text = c.yTitle, font = bold 11, color = Muted, anchor = Middle} | not (T.null c.yTitle)]
        <> [Text {at = if c.horizontal then Point 20 (titleRoom - 14) else Point (60 + long / 2) (titleRoom + short + 34), text = c.xTitle, font = bold 11, color = Muted, anchor = Middle} | not (T.null c.xTitle)]
    head' xs = case xs of
      x : _ -> x
      [] -> 0

-- | A round top above @hi@ and the step between grid lines, about five.
niceTop :: Double -> Double -> (Double, Double)
niceTop lo hi = (lo + step' * fromIntegral (ceiling ((hi - lo) / step') :: Int), step')
  where
    raw = max 0.0001 ((hi - lo) / 5)
    magnitude = 10 ** fromIntegral (floor (logBase 10 raw) :: Int)
    step' = head' [m * magnitude | m <- [1, 2, 2.5, 5, 10], m * magnitude >= raw]
    head' xs = case xs of
      x : _ -> x
      [] -> raw
