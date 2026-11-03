{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid timelines: periods along an axis, each with its events
-- stacked under it, grouped by sections.
module Usta.Diagram.Timeline
  ( Timeline (..)
  , parse
  , draw
  ) where

import Data.Kind (Type)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)

import Usta.Diagram.Scene

type Period :: Type
data Period = Period {section :: Text, name :: Text, events :: [Text]}

type Timeline :: Type
data Timeline = Timeline {title :: Maybe Text, periods :: [Period]}

parse :: Text -> Either Text Timeline
parse source = go Nothing "" [] (drop 1 (filter (not . T.null) (map T.strip (T.lines source))))
  where
    go title _ acc [] = if null acc then Left "The timeline has no periods." else Right Timeline {title = title, periods = reverse acc}
    go title section acc (l : rest)
      | Just t <- T.stripPrefix "title " l = go (Just (T.strip t)) section acc rest
      | Just s <- T.stripPrefix "section " l = go title (T.strip s) acc rest
      -- ": more" adds events to the period before.
      | ":" `T.isPrefixOf` l, p : more <- acc = go title section (p {events = p.events <> pieces (T.drop 1 l)} : more) rest
      | (n, es) <- T.breakOn ":" l = go title section (Period section (T.strip n) (pieces (T.drop 1 es)) : acc) rest
    pieces t = filter (not . T.null) (map T.strip (T.splitOn ":" t))

draw :: Monad m => Measure m -> Timeline -> m Scene
draw measure t = do
  widths <- for t.periods $ \p -> do
    n <- measure (bold 12) p.name
    es <- for p.events (measure (regular 12))
    pure (maximum (n.w : map (.w) es) + 28)
  let width = maximum (110 : widths)
      gap = 18
      sections = nub [p.section | p <- t.periods, not (T.null p.section)]
      colorOf p = if null sections then length (takeWhile ((/= p.name) . (.name)) t.periods) else length (takeWhile (/= p.section) sections)
      top = maybe 0 (const 34) t.title + (if null sections then 0 else 34)
      xOf i = fromIntegral i * (width + gap)
      axisY = top + 60
      bands = concat
        [ [ Rect {at = Point (xOf (head' is)) (top - 32), size = Size (xOf (last' is) + width - xOf (head' is)) 24, radius = 6, fill = Just (Tint k), edge = Just (pen (Series k) 1)}
          , Text {at = Point ((xOf (head' is) + xOf (last' is) + width) / 2) (top - 20), text = s, font = bold 12, color = Ink, anchor = Middle} ]
        | (k, s) <- zip [0 ..] sections
        , let is = [i | (i, p) <- zip [0 :: Int ..] t.periods, p.section == s] ]
      axis = [Path {start = Point (-10) axisY, segments = [LineTo (Point (xOf (length t.periods) + 10) axisY)], line = pen Line 1.6, begin = NoHead, end = Arrow}]
      columns = concat
        [ [ Rect {at = Point (xOf i) top, size = Size width 36, radius = 8, fill = Just (Series (colorOf p)), edge = Nothing}
          , Text {at = Point (xOf i + width / 2) (top + 18), text = p.name, font = bold 12, color = OnSeries, anchor = Middle}
          , Ellipse {center = Point (xOf i + width / 2) axisY, rx = 5, ry = 5, fill = Just Paper, edge = Just (pen (Series (colorOf p)) 2)} ]
            <> concat
              [ [ Path {start = Point (xOf i + width / 2) (axisY + 5), segments = [LineTo (Point (xOf i + width / 2) (y - 1))], line = pen (Series (colorOf p)) 1, begin = NoHead, end = NoHead} | k == 0 ]
                  <> [ Rect {at = Point (xOf i) y, size = Size width 32, radius = 6, fill = Just (Tint (colorOf p)), edge = Just (pen (Series (colorOf p)) 1)}
                     , Text {at = Point (xOf i + width / 2) (y + 16), text = e, font = regular 12, color = Ink, anchor = Middle} ]
              | (k, e) <- zip [0 :: Int ..] p.events
              , let y = axisY + 22 + fromIntegral k * 40 ]
        | (i, p) <- zip [0 :: Int ..] t.periods ]
      titleItem = [Text {at = Point (xOf (length t.periods) / 2) 12, text = x, font = bold 14, color = Ink, anchor = Middle} | Just x <- [t.title]]
  pure (scene 12 (bands <> axis <> columns <> titleItem))
  where
    head' xs = case xs of
      x : _ -> x
      [] -> 0
    last' xs = if null xs then 0 else last xs
