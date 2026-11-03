{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid radar charts: axes around a centre, each curve a shape
-- through its values, on a polygon or circle grid.
module Usta.Diagram.Radar
  ( Radar (..)
  , parse
  , draw
  ) where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Usta.Diagram.Scene

type Radar :: Type
data Radar = Radar
  { title :: Maybe Text
  , axes :: [Text]
  , curves :: [(Text, [Double])]
  , low :: Maybe Double
  , high :: Maybe Double
  , circles :: Bool
  , ticks :: Int
  }

parse :: Text -> Either Text Radar
parse source = do
  let body = drop 1 (filter (not . T.null) (map T.strip (T.lines source)))
      field k = case [T.strip v | l <- body, Just v <- [T.stripPrefix k l]] of
        v : _ -> Just v
        [] -> Nothing
      axes = concat [map shown (items v) | l <- body, Just v <- [T.stripPrefix "axis " l]]
  curves <- traverse curve (concat [splitCurves v | l <- body, Just v <- [T.stripPrefix "curve " l]])
  if null axes then Left "The radar chart has no axes." else
    pure Radar
      { title = T.dropAround (== '"') <$> field "title"
      , axes = axes
      , curves = curves
      , low = field "min" >>= readMaybe . T.unpack
      , high = field "max" >>= readMaybe . T.unpack
      , circles = field "graticule" == Just "circle"
      , ticks = fromMaybe 5 (field "ticks" >>= readMaybe . T.unpack)
      }
  where
    items = map T.strip . T.splitOn ","
    -- "m[\"Math\"]" shows Math; a bare name shows itself.
    shown t = case T.breakOn "[" t of
      (n, l) | T.null l -> T.strip n
             | otherwise -> T.dropAround (`elem` ("[]\" " :: String)) l
    splitCurves v = filter (not . T.null) (map T.strip (T.splitOn "}" v)) >>= \c -> [T.dropWhile (== ',') (T.strip c)]
    curve c =
      let (name, vs) = T.breakOn "{" c
          values = traverse (readMaybe . T.unpack . T.strip . snd . T.breakOnEnd ":") (T.splitOn "," (T.drop 1 vs))
       in maybe (Left ("usta could not read the curve \"" <> T.take 40 c <> "\".")) (\x -> Right (shown (T.strip name), x)) values

draw :: Monad m => Measure m -> Radar -> m Scene
draw _ r = pure (scene 12 (grid <> spokes <> shapes <> labels <> legend <> titles))
  where
    n = length r.axes
    radius = 140 :: Double
    titleRoom = maybe 0 (const 34) r.title
    c = Point (radius + 80) (radius + titleRoom + 30)
    lo = fromMaybe 0 r.low
    hi = fromMaybe (maximum (1 : concatMap snd r.curves)) r.high
    angle i = -pi / 2 + 2 * pi * fromIntegral i / fromIntegral n
    pointAt i f = Point (c.x + radius * f * cos (angle i)) (c.y + radius * f * sin (angle i))
    fraction v = max 0 (min 1 ((v - lo) / max 0.0001 (hi - lo)))
    levels = [fromIntegral k / fromIntegral (max 1 r.ticks) | k <- [1 .. max 1 r.ticks]]
    grid =
      [ if r.circles
          then Ellipse {center = c, rx = radius * f, ry = radius * f, fill = Nothing, edge = Just (pen Border 0.8)}
          else Polygon {points = [pointAt i f | i <- [0 .. n - 1]], fill = Nothing, edge = Just (pen Border 0.8)}
      | f <- levels ]
    spokes = [Path {start = c, segments = [LineTo (pointAt i 1)], line = pen Border 0.8, begin = NoHead, end = NoHead} | i <- [0 .. n - 1]]
    shapes = concat
      [ Polygon {points = [pointAt i (fraction v) | (i, v) <- zip [0 ..] vs], fill = Nothing, edge = Just (pen (Series k) 2)}
          : [Ellipse {center = pointAt i (fraction v), rx = 3, ry = 3, fill = Just (Series k), edge = Nothing} | (i, v) <- zip [0 ..] vs]
      | (k, (_, vs)) <- zip [0 ..] r.curves ]
    labels =
      [ let p = pointAt i 1.12
            anchor' = if abs (cos (angle i)) < 0.2 then Middle else if cos (angle i) > 0 then Start else End
         in Text {at = p, text = a, font = regular 12, color = Ink, anchor = anchor'}
      | (i, a) <- zip [0 ..] r.axes ]
    legend = concat
      [ [ Rect {at = Point (c.x + radius + 70) (c.y - radius + 24 * fromIntegral k), size = Size 12 12, radius = 3, fill = Just (Series k), edge = Nothing}
        , Text {at = Point (c.x + radius + 90) (c.y - radius + 6 + 24 * fromIntegral k), text = name, font = regular 12, color = Ink, anchor = Start} ]
      | (k, (name, _)) <- zip [0 :: Int ..] r.curves ]
    titles = [Text {at = Point c.x 12, text = t, font = bold 14, color = Ink, anchor = Middle} | Just t <- [r.title]]
