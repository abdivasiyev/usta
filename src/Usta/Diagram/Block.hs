{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid block diagrams: blocks placed by the author in a grid of
-- columns, some spanning several, with nested blocks, gaps and block
-- arrows, and links drawn straight over them.
module Usta.Diagram.Block
  ( Blocks (..)
  , parse
  , draw
  ) where

import Data.Char (isAlphaNum, isDigit)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)

import Usta.Diagram.Flowchart (Sized (..), boxFor, edgeOf, labelFont, nodeFont, penOf, shape)
import qualified Usta.Diagram.Route as Route
import Usta.Diagram.Scene hiding (Head (..))
import Usta.Diagram.Scene (Head (Arrow, NoHead))
import Usta.Mermaid (Edge (..), Node (..), Shape (..), chain, node)

-- | Which way a block arrow points.
type Pointing :: Type
data Pointing = PointRight | PointLeft | PointUp | PointDown | Sideways | Upright

-- | One cell's content, with how many columns it spans.
type Piece :: Type
data Piece
  = Block Node Int
  | Gap Int
  | BlockArrow Text Pointing Int
  | Nested Int (Maybe Int) [Piece]

type Blocks :: Type
data Blocks = Blocks {columns :: Maybe Int, pieces :: [Piece], edges :: [Edge]}

parse :: Text -> Either Text Blocks
parse source = do
  (columns, pieces, edges, rest) <- level (drop 1 lines')
  case rest of
    [] | null pieces -> Left "The block diagram has no blocks."
       | otherwise -> pure Blocks {columns, pieces, edges}
    _ -> Left "An \"end\" in the block diagram closes no block."
  where
    lines' = [l | l <- map (T.strip . fst . T.breakOn "%%") (T.lines source), not (T.null l)]

-- | The lines of one level, up to its "end": its column count, its pieces
-- and the links among them, and the lines after it.
level :: [Text] -> Either Text (Maybe Int, [Piece], [Edge], [Text])
level = go Nothing [] []
  where
    go cols ps es ls = case ls of
      [] -> pure (cols, reverse ps, reverse es, [])
      l : rest
        | l == "end" -> pure (cols, reverse ps, reverse es, l : rest)
        | Just n <- T.stripPrefix "columns" l, Just k <- count (T.strip n) -> go (Just k) ps es rest
        | any (`T.isPrefixOf` l) ["classDef ", "class ", "style ", "linkStyle "] -> go cols ps es rest
        | l == "block" || "block:" `T.isPrefixOf` l -> do
            (inner, innerPs, innerEs, after) <- level rest
            case after of
              "end" : more -> go cols (Nested (spanOf l) inner innerPs : ps) (reverse innerEs <> es) more
              _ -> Left "A block in the block diagram is not closed with end."
        | Right (_, made@(_ : _)) <- chain l -> go cols ps (reverse made <> es) rest
        | otherwise -> do
            row <- cells l
            go cols (reverse row <> ps) es rest
    -- "block:id:2" spans 2; "block:id" and "block" span 1.
    spanOf l = case T.splitOn ":" l of
      [_, _, n] | Just k <- count n -> k
      _ -> 1
    count t = if not (T.null t) && T.all isDigit t then Just (read (T.unpack t)) else Nothing
    -- Blocks on one line: "a[\"A\"]:2 b space:3 c<[\"go\"]>(right)".
    cells t0 = case T.stripStart t0 of
      "" -> pure []
      t
        | Just after <- T.stripPrefix "space" t, not (startsName after) -> let (n, more) = width after in (Gap n :) <$> cells more
        | (name, after) <- T.span nameChar t, not (T.null name), Just inside <- T.stripPrefix "<[" after ->
            case T.breakOn "]>" inside of
              (_, "") -> Left "A block arrow's text is not closed with ]>."
              (label, more) -> do
                -- "]>(right)": the direction is between the brackets.
                let (pointing, more') = T.breakOn ")" (T.drop 3 more)
                    (n, rest) = width (T.drop 1 more')
                (BlockArrow (T.dropAround (== '"') label) (direction (T.strip pointing)) n :) <$> cells rest
        | otherwise -> do
            (n, after) <- node t
            let (k, more) = width after
            (Block n k :) <$> cells more
    startsName t = maybe False (nameChar . fst) (T.uncons t)
    nameChar c = isAlphaNum c || c == '_'
    width t = case T.stripPrefix ":" t of
      Just more | (digits, rest) <- T.span isDigit more, not (T.null digits) -> (read (T.unpack digits), rest)
      _ -> (1, t)
    direction d = case d of
      "left" -> PointLeft
      "up" -> PointUp
      "down" -> PointDown
      "x" -> Sideways
      "y" -> Upright
      _ -> PointRight

-- | Something laid out at the origin, with each block's centre and shape.
type Laid :: Type
data Laid = Laid {size :: Size, items :: [Item], spots :: [(Text, (Point, Sized))]}

moved :: Double -> Double -> Laid -> Laid
moved dx dy l = l {items = map (translate dx dy) l.items, spots = [(k, (Point (c.x + dx) (c.y + dy), s)) | (k, (c, s)) <- l.spots]}

draw :: Monad m => Measure m -> Blocks -> m Scene
draw measure diagram = do
  body <- grid 0 0 diagram.columns diagram.pieces
  links <- for diagram.edges (link (Map.fromList body.spots))
  pure (scene 12 (body.items <> concat links))
  where
    gap = 16
    -- A level's pieces in rows of its columns (all on one row without a
    -- count), every column as wide as the widest block needs, and the
    -- whole at least @room@ wide.
    grid depth room cols pieces = do
      natural <- for pieces (piece depth)
      let n = fromMaybe (max 1 (sum (map spanOf pieces))) cols
          cellW = maximum ((room - gap * fromIntegral (n - 1)) / fromIntegral n : 60 : [(l.size.w - gap * fromIntegral (k - 1)) / fromIntegral k | (p, Just l) <- zip pieces natural, let k = min n (spanOf p)])
          placed = place n (zip pieces natural)
          rows = maximum (0 : [r + 1 | (_, r, _, _) <- placed])
          rowH r = maximum (0 : [l.size.h | (_, r', _, Just l) <- placed, r' == r])
          rowY = scanl (\y r -> y + rowH r + gap) 0 [0 .. rows - 1]
          cellsW k = cellW * fromIntegral k + gap * fromIntegral (k - 1)
          laid (p, r, c, l) = do
            let x = fromIntegral c * (cellW + gap)
                y = rowY !! r
                cell = Size (cellsW (min n (spanOf p))) (rowH r)
            fmap (moved x y) <$> case (p, l) of
              (Block nd _, Just l') -> pure (Just (leaf nd l'.size cell))
              (BlockArrow t d _, Just _) -> pure (Just (arrow t d cell))
              -- A nested block again, as wide as its cell.
              (Nested _ cols' inner, Just _) -> Just <$> nested depth cell.w cols' inner
              _ -> pure Nothing
      ps <- concatMap (maybe [] pure) <$> for placed laid
      pure Laid {size = Size (cellsW n) (max 0 (last rowY - gap)), items = concatMap (.items) ps, spots = concatMap (.spots) ps}
    -- A block as wide as its cell; circles and diamonds keep their shape.
    leaf nd natural cell =
      let box = case nd.shape of
            Circle -> natural
            Diamond -> natural
            _ -> Size cell.w natural.h
          c = Point (cell.w / 2) (cell.h / 2)
          sized = Sized {node = nd, text = Size 0 0, box = box}
       in Laid {size = cell, items = shape c sized, spots = [(nd.name, (c, sized))]}
    -- Each piece's own size, before it is stretched to its cell; a gap has none.
    piece depth p = case p of
      Gap _ -> pure Nothing
      Block nd _ -> do
        t <- measure nodeFont nd.label
        pure (Just Laid {size = boxFor nd.shape t, items = [], spots = []})
      BlockArrow t _ _ -> do
        s <- measure nodeFont t
        pure (Just Laid {size = Size (s.w + 56) (max 44 (s.h + 24)), items = [], spots = []})
      Nested _ cols inner -> Just <$> nested depth 0 cols inner
    -- A frame around a level's own grid.
    nested depth room cols inner = do
      l <- grid (depth + 1) (room - 20) cols inner
      let w = l.size.w + 20
          h = l.size.h + 20
      pure Laid
        { size = Size w h
        , items = Rect {at = Point 0 0, size = Size w h, radius = 6, fill = Just (Tint depth), edge = Just (pen Border 1)} : (moved 10 10 l).items
        , spots = (moved 10 10 l).spots }
    spanOf p = case p of
      Block _ k -> k
      Gap k -> k
      BlockArrow _ _ k -> k
      Nested k _ _ -> k
    -- Pieces flow along a row and wrap when the next does not fit.
    place n = go 0 0
      where
        go _ _ [] = []
        go r c ((p, l) : rest)
          | c > 0 && c + k > n = go (r + 1) 0 ((p, l) : rest)
          | c + k >= n = (p, r, c, l) : go (r + 1) 0 rest
          | otherwise = (p, r, c, l) : go r (c + k) rest
          where
            k = min n (spanOf p)
    arrow t d cell =
      let vertical = case d of
            PointUp -> True
            PointDown -> True
            Upright -> True
            _ -> False
          both = case d of
            Sideways -> True
            Upright -> True
            _ -> False
          backward = case d of
            PointLeft -> True
            PointUp -> True
            _ -> False
          len = if vertical then cell.h else cell.w
          thick = min 48 (if vertical then cell.w else cell.h)
          tip = min 22 (len / 3)
          m = thick / 2
          shaft = thick * 0.28
          tail' = if both then tip else 0
          -- Along the arrow's length x, across it y, pointing to x = len.
          outline =
            [Point tail' (m - shaft), Point (len - tip) (m - shaft), Point (len - tip) 0, Point len m, Point (len - tip) thick, Point (len - tip) (m + shaft), Point tail' (m + shaft)]
              <> (if both then [Point tail' thick, Point 0 m, Point tail' 0] else [Point 0 (m + shaft), Point 0 (m - shaft)])
          turned p =
            let along = if backward then len - p.x else p.x
             in if vertical then Point (p.y + (cell.w - thick) / 2) along else Point along (p.y + (cell.h - thick) / 2)
       in Laid
            { size = cell
            , items =
                [ Polygon {points = map turned outline, fill = Just (Tint 2), edge = Just (pen (Series 2) 1.2)}
                , Text {at = Point (cell.w / 2) (cell.h / 2), text = t, font = nodeFont, color = Ink, anchor = Middle} ]
            , spots = [] }
    link spots e = fromMaybe (pure []) $ do
      (c0, s0) <- Map.lookup e.from spots
      (c1, s1) <- Map.lookup e.to spots
      pure $ do
        let a = edgeOf c0 s0 c1 2
            b = edgeOf c1 s1 c0 2
            m = Route.middle [a, b]
        label <- for (fmap (T.dropAround (== '"')) e.label) (\t -> (t,) <$> measure labelFont t)
        pure $
          [Path {start = a, segments = [LineTo b], line = penOf e.stroke, begin = NoHead, end = if e.arrow then Arrow else NoHead}]
            <> concat
              [ [ Rect {at = Point (m.x - s.w / 2 - 4) (m.y - s.h / 2 - 1), size = Size (s.w + 8) (s.h + 2), radius = 3, fill = Just Paper, edge = Nothing}
                , Text {at = m, text = t, font = labelFont, color = Muted, anchor = Middle} ]
              | Just (t, s) <- [label] ]
