{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid pie charts: slices in the series colors with their share,
-- and a legend.
module Usta.Diagram.Pie
  ( Pie (..)
  , parse
  , draw
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Text.Read (readMaybe)

import Usta.Diagram.Scene

type Pie :: Type
data Pie = Pie {title :: Maybe Text, showData :: Bool, slices :: [(Text, Double)]}

parse :: Text -> Either Text Pie
parse source = do
  let header = T.words (T.strip (head' (T.lines source)))
      lines' = drop 1 (filter (not . T.null) (map T.strip (T.lines source)))
      inlineTitle = case dropWhile (/= "title") header of
        _ : t -> Just (T.unwords t)
        [] -> Nothing
      title = case [T.strip t | l <- lines', Just t <- [T.stripPrefix "title" l]] of
        t : _ -> Just t
        [] -> inlineTitle
  slices <- traverse slice [l | l <- lines', "\"" `T.isPrefixOf` l]
  if null slices then Left "The pie chart has no slices." else
    pure Pie {title = title, showData = "showData" `elem` header || "showData" `elem` lines', slices = slices}
  where
    head' xs = case xs of
      x : _ -> x
      [] -> ""
    slice line = case T.breakOnEnd ":" line of
      (name, value) | Just v <- readMaybe (T.unpack (T.strip value)), v >= (0 :: Double) ->
        Right (T.dropAround (`elem` ("\" :" :: String)) (T.strip name), v)
      _ -> Left ("usta could not read the slice \"" <> T.take 40 line <> "\".")

draw :: Monad m => Measure m -> Pie -> m Scene
draw measure pie = do
  names <- for pie.slices $ \(n, v) -> measure (regular 12) (legendText n v)
  let total = max 0.0001 (sum (map snd pie.slices))
      r = 120 :: Double
      c = Point r (r + titleRoom)
      titleRoom = maybe 0 (const 34) pie.title
      angles = scanl (+) (-pi / 2) [2 * pi * v / total | (_, v) <- pie.slices]
      wedges = concat
        [ [ Wedge {center = c, r = r, from = a0, to = a1, fill = Just (Series i), edge = Just (pen Paper 2)} ]
            <> [ Text {at = Point (c.x + r * 0.64 * cos mid) (c.y + r * 0.64 * sin mid), text = percent v, font = bold 12, color = OnSeries, anchor = Middle}
               | v / total >= 0.05 ]
        | (i, ((_, v), (a0, a1))) <- zip [0 ..] (zip pie.slices (zip angles (drop 1 angles)))
        , let mid = (a0 + a1) / 2 ]
      percent v = T.pack (show (round (100 * v / total) :: Int)) <> "%"
      legendX = 2 * r + 36
      legendTop = c.y - fromIntegral (length pie.slices) * 12
      legend = concat
        [ [ Rect {at = Point legendX (y - 7), size = Size 14 14, radius = 3, fill = Just (Series i), edge = Nothing}
          , Text {at = Point (legendX + 22) y, text = legendText n v, font = regular 12, color = Ink, anchor = Start} ]
        | (i, (n, v)) <- zip [0 :: Int ..] pie.slices
        , let y = legendTop + 12 + 24 * fromIntegral i ]
      widest = maximum (0 : map (.w) names)
      titleItem = [Text {at = Point ((legendX + 22 + widest) / 2) 14, text = t, font = bold 14, color = Ink, anchor = Middle} | Just t <- [pie.title]]
      corner = Rect {at = Point (legendX + 22 + widest) 0, size = Size 0 0, radius = 0, fill = Nothing, edge = Nothing}
  pure (scene 12 (wedges <> legend <> titleItem <> [corner]))
  where
    legendText n v = if pie.showData then n <> "  " <> number v else n
    number v = if v == fromIntegral (round v :: Int) then T.pack (show (round v :: Int)) else T.pack (show v)
