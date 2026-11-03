{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid treemaps: nested sections whose leaves are rectangles as large
-- as their values, laid out squarified so they stay close to square.
module Usta.Diagram.Treemap
  ( Treemap (..)
  , parse
  , draw
  ) where

import Data.Char (isSpace)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import Text.Read (readMaybe)

import Usta.Diagram.Scene

-- | A section with what it holds, or a leaf with its value.
type Node :: Type
data Node = Section Text [Node] | Leaf Text Double

type Treemap :: Type
newtype Treemap = Treemap {roots :: [Node]}

parse :: Text -> Either Text Treemap
parse source = do
  items <- traverse item [l | l <- drop 1 (T.lines source), not (T.null (T.strip l)), not ("%%" `T.isPrefixOf` T.strip l)]
  case forest items of
    [] -> Left "The treemap has nothing in it."
    roots -> pure (Treemap roots)
  where
    -- "\"Name\"" for a section, "\"Name\": 12" for a leaf; a ":::class"
    -- after either is read past.
    item l = do
      let depth = T.length (T.takeWhile isSpace l)
          body = fst (T.breakOn ":::" (T.strip l))
          (name, rest) = T.breakOn "\"" (T.drop 1 (T.dropWhile (/= '"') body))
          value = T.strip (T.dropWhile (== ':') (T.strip (T.drop 1 rest)))
      if T.null rest
        then Left ("usta could not read \"" <> T.take 40 l <> "\"; names are in double quotes.")
        else case value of
          "" -> pure (depth, Section name [])
          v -> maybe (Left ("\"" <> v <> "\" is not a number.")) (\x -> pure (depth, Leaf name x)) (readMaybe (T.unpack v))
    forest xs = case xs of
      [] -> []
      (d, node) : rest ->
        let (inside, more) = span ((> d) . fst) rest
         in (case node of Section n _ -> Section n (forest inside); leaf -> leaf) : forest more

total :: Node -> Double
total = \case
  Leaf _ v -> max 0 v
  Section _ ns -> sum (map total ns)

draw :: Monad m => Measure m -> Treemap -> m Scene
draw measure t = scene 12 . concat <$> for (zip [0 ..] (squarify t.roots (Box 0 0 720 440))) (\(i, (n, b)) -> node i n b)
  where
    node i n b = case n of
      Leaf name v -> do
        nameSize <- measure (bold 12) name
        let room = b.w - 10
        pure $
          [Rect {at = Point b.x b.y, size = Size b.w b.h, radius = 4, fill = Just (Series i), edge = Just (pen Paper 2)}]
            <> [Text {at = Point (b.x + 6) (b.y + 16), text = name, font = bold 12, color = OnSeries, anchor = Start} | nameSize.w <= room, b.h >= 22]
            <> [Text {at = Point (b.x + 6) (b.y + 32), text = amount v, font = regular 11, color = OnSeries, anchor = Start} | nameSize.w <= room, b.h >= 40]
      Section name ns -> do
        nameSize <- measure (bold 12) name
        let header = if b.h > 40 then 22 else 0
            inner = Box (b.x + 4) (b.y + header) (b.w - 8) (b.h - header - 4)
        children <- for (squarify ns inner) (\(c, cb) -> node i c cb)
        pure $
          [Rect {at = Point b.x b.y, size = Size b.w b.h, radius = 6, fill = Just (Tint i), edge = Just (pen Paper 2)}]
            <> [Text {at = Point (b.x + 6) (b.y + 15), text = name <> "  " <> amount (total n), font = bold 12, color = Ink, anchor = Start} | header > 0, nameSize.w <= b.w - 12]
            <> concat children
    amount v =
      let cents = round (v * 100) :: Integer
          (whole, part) = cents `divMod` 100
       in T.pack (show whole) <> if part == 0 then "" else "." <> T.dropWhileEnd (== '0') (T.justifyRight 2 '0' (T.pack (show part)))

type Box :: Type
data Box = Box {x :: Double, y :: Double, w :: Double, h :: Double}

-- | Places the nodes in the box, each with room in step with its total:
-- rows along the box's shorter side, a row growing while that keeps its
-- rectangles closer to square.
squarify :: [Node] -> Box -> [(Node, Box)]
squarify nodes box
  | null weighed || box.w <= 0 || box.h <= 0 = []
  | otherwise = go [(n, v * scale) | (n, v) <- weighed] box
  where
    weighed = sortOn (Down . snd) [(n, total n) | n <- nodes, total n > 0]
    scale = box.w * box.h / sum (map snd weighed)
    go [] _ = []
    go areas b =
      let side = min b.w b.h
          worst r = let t = sum (map snd r) in maximum [max (side * side * a / (t * t)) (t * t / (side * side * a)) | (_, a) <- r]
          grow r left = case left of
            i : more | null r || worst (r <> [i]) <= worst r -> grow (r <> [i]) more
            _ -> (r, left)
          (row, rest) = grow [] areas
          s = sum (map snd row)
       in if b.w >= b.h
            then
              let thick = s / b.h
                  ys = scanl (+) b.y [a / thick | (_, a) <- row]
               in [(n, Box b.x y0 thick (a / thick)) | ((n, a), y0) <- zip row ys] <> go rest (Box (b.x + thick) b.y (b.w - thick) b.h)
            else
              let thick = s / b.w
                  xs = scanl (+) b.x [a / thick | (_, a) <- row]
               in [(n, Box x0 b.y (a / thick) thick) | ((n, a), x0) <- zip row xs] <> go rest (Box b.x (b.y + thick) b.w (b.h - thick))
