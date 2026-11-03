{-# LANGUAGE OverloadedStrings #-}

-- | A diagram as something to draw: boxes, shapes, lines and text at
-- positions, in colors named by their role. Each diagram type's layout
-- makes one; the window draws it with GTK, and the theme decides what each
-- role looks like, so a theme change only redraws.
module Usta.Diagram.Scene
  ( Role (..)
  , Point (..)
  , Pen (..)
  , pen
  , dashed
  , Head (..)
  , Segment (..)
  , Anchor (..)
  , Font (..)
  , regular
  , bold
  , Size (..)
  , Measure
  , wrap
  , Item (..)
  , Scene (..)
  , scene
  , extent
  , translate
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T

-- | What a color is for; the palette gives each a light and a dark value.
type Role :: Type
data Role
  = Ink
    -- ^ Text.
  | Muted
    -- ^ Secondary text: labels on links, axes.
  | Line
    -- ^ Links and arrows.
  | Border
  | Paper
    -- ^ A shape's fill.
  | Series Int
    -- ^ The nth data color, cycling.
  | Tint Int
    -- ^ The nth data color, pale, for fills.
  | Danger
  | DangerTint
  | OnSeries
    -- ^ Text on a series color.
  deriving stock (Eq, Show)

type Point :: Type
data Point = Point {x :: Double, y :: Double}
  deriving stock (Eq, Show)

type Pen :: Type
data Pen = Pen
  { color :: Role
  , width :: Double
  , dash :: Maybe Double
    -- ^ The length of each dash, for a dashed line.
  }
  deriving stock (Eq, Show)

pen :: Role -> Double -> Pen
pen c w = Pen {color = c, width = w, dash = Nothing}

dashed :: Pen -> Pen
dashed p = Pen {color = p.color, width = p.width, dash = Just 5}

-- | What ends a path.
type Head :: Type
data Head
  = NoHead
  | Arrow
    -- ^ A filled triangle.
  | Hollow
    -- ^ An open triangle, for inheritance.
  | Diamond Bool
    -- ^ Filled ('True', composition) or open (aggregation).
  | Dot
  | Cross
  | One
    -- ^ Crow's-foot cardinalities.
  | ZeroOrOne
  | Many
  | ZeroOrMany
  | OneOrMany
  deriving stock (Eq, Show)

type Segment :: Type
data Segment
  = LineTo Point
  | CurveTo Point Point Point
    -- ^ Two control points and the end.
  deriving stock (Eq, Show)

type Anchor :: Type
data Anchor = Start | Middle | End
  deriving stock (Eq, Show)

type Font :: Type
data Font = Font {size :: Double, heavy :: Bool, mono :: Bool}
  deriving stock (Eq, Show)

regular :: Double -> Font
regular s = Font {size = s, heavy = False, mono = False}

bold :: Double -> Font
bold s = Font {size = s, heavy = True, mono = False}

type Size :: Type
data Size = Size {w :: Double, h :: Double}
  deriving stock (Eq, Show)

-- | How big a text is in a font; layouts ask before placing it. In the
-- window it is Pango; tests give an estimate.
type Measure :: (Type -> Type) -> Type
type Measure m = Font -> Text -> m Size

-- | The words in lines no wider than @width@.
wrap :: Monad m => Measure m -> Font -> Double -> Text -> m [Text]
wrap measure font width text = go [] (T.words text)
  where
    go acc [] = pure (reverse acc)
    go acc (w : ws) = do
      let fits line rest = case rest of
            [] -> pure (line, [])
            x : xs -> do
              s <- measure font (line <> " " <> x)
              if s.w <= width then fits (line <> " " <> x) xs else pure (line, rest)
      (line, rest) <- fits w ws
      go (line : acc) rest

type Item :: Type
data Item
  = Rect {at :: Point, size :: Size, radius :: Double, fill :: Maybe Role, edge :: Maybe Pen}
  | Polygon {points :: [Point], fill :: Maybe Role, edge :: Maybe Pen}
  | Ellipse {center :: Point, rx :: Double, ry :: Double, fill :: Maybe Role, edge :: Maybe Pen}
  | Wedge {center :: Point, r :: Double, from :: Double, to :: Double, fill :: Maybe Role, edge :: Maybe Pen}
    -- ^ A pie slice; angles in radians, clockwise from three o'clock.
  | Path {start :: Point, segments :: [Segment], line :: Pen, begin :: Head, end :: Head}
  | Text {at :: Point, text :: Text, font :: Font, color :: Role, anchor :: Anchor}
    -- ^ @at@ is the middle of the text's height, at its left, centre or
    -- right as 'anchor' says. Lines break at "\n".
  deriving stock (Eq, Show)

type Scene :: Type
data Scene = Scene {size :: Size, items :: [Item]}
  deriving stock (Eq, Show)

-- | The items with a margin around them, the size from what they cover.
scene :: Double -> [Item] -> Scene
scene margin items = Scene
  { size = Size {w = right - left + 2 * margin, h = bottom - top + 2 * margin}
  , items = map (translate (margin - left) (margin - top)) items
  }
  where
    boxes = concatMap extent items
    left = minimum (0 : map fst boxes)
    top = minimum (0 : map snd boxes)
    right = maximum (0 : map fst boxes)
    bottom = maximum (0 : map snd boxes)

-- | The corners an item covers. Text counts only its anchor point here;
-- the window measures it and makes room.
extent :: Item -> [(Double, Double)]
extent item = case item of
  Rect {at = p, size = s} -> [(p.x, p.y), (p.x + s.w, p.y + s.h)]
  Polygon {points = ps} -> [(p.x, p.y) | p <- ps]
  Ellipse {center = c, rx = a, ry = b} -> [(c.x - a, c.y - b), (c.x + a, c.y + b)]
  Wedge {center = c, r = a} -> [(c.x - a, c.y - a), (c.x + a, c.y + a)]
  Path {start = p, segments = ss} -> (p.x, p.y) : concatMap ends ss
  Text {at = p} -> [(p.x, p.y)]
  where
    ends s = case s of
      LineTo p -> [(p.x, p.y)]
      CurveTo a b c -> [(a.x, a.y), (b.x, b.y), (c.x, c.y)]

translate :: Double -> Double -> Item -> Item
translate dx dy item = case item of
  Rect {at = p, size = s, radius = r', fill = f, edge = e} -> Rect {at = move p, size = s, radius = r', fill = f, edge = e}
  Polygon {points = ps, fill = f, edge = e} -> Polygon {points = map move ps, fill = f, edge = e}
  Ellipse {center = c, rx = a, ry = b, fill = f, edge = e} -> Ellipse {center = move c, rx = a, ry = b, fill = f, edge = e}
  Wedge {center = c, r = a, from = f0, to = f1, fill = f, edge = e} -> Wedge {center = move c, r = a, from = f0, to = f1, fill = f, edge = e}
  Path {start = p, segments = ss, line = l, begin = b, end = e} -> Path {start = move p, segments = map segment ss, line = l, begin = b, end = e}
  Text {at = p, text = t, font = f, color = c, anchor = a} -> Text {at = move p, text = t, font = f, color = c, anchor = a}
  where
    move p = Point {x = p.x + dx, y = p.y + dy}
    segment s = case s of
      LineTo p -> LineTo (move p)
      CurveTo a b c -> CurveTo (move a) (move b) (move c)
