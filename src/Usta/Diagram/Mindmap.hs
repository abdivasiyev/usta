{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid mindmaps: a tree by indentation, the root in the middle and
its branches spread to either side, each branch in its own color.
-}
module Usta.Diagram.Mindmap
    ( Mindmap (..)
    , parse
    , draw
    )
where

import Data.Char (isSpace)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Traversable (for)

import Data.Text qualified as T

import Usta.Diagram.Scene

type Shape :: Type
data Shape = Plain | Square | Rounded | Circle | Bang | Cloud | Hexagon
    deriving stock (Eq)

type Tree :: Type
data Tree = Tree {label :: Text, shape :: Shape, children :: [Tree]}

type Mindmap :: Type
newtype Mindmap = Mindmap {root :: Tree}

parse :: Text -> Either Text Mindmap
parse source = case items of
    [] -> Left "The mindmap is empty."
    (d, t) : rest -> case build d rest of
        (kids, []) -> Right (Mindmap (t {children = kids}))
        _ -> Left "The mindmap has more than one root."
    where
        items =
            [ (T.length (T.takeWhile isSpace l), node (T.strip l))
            | l <- drop 1 (T.lines source)
            , not (T.null (T.strip l))
            , not ("::" `T.isPrefixOf` T.strip l)
            , not ("%%" `T.isPrefixOf` T.strip l)
            ]
        -- The items deeper than @depth@, as the children of the one before.
        build depth xs = case xs of
            (d, t) : rest
                | d > depth ->
                    let
                        (kids, after) = build d rest
                        (siblings, remaining) = build depth after
                    in
                        (t {children = kids} : siblings, remaining)
            _ -> ([], xs)

-- | "id((text))", "id[text]", "id)cloud(" and the rest, or plain text.
node :: Text -> Tree
node t = case [(s, inner) | (open, close, s) <- shapes, Just inner <- [between open close]] of
    (s, inner) : _ -> Tree (clean inner) s []
    [] -> Tree (clean t) Plain []
    where
        shapes = [("((", "))", Circle), ("))", "((", Bang), ("{{", "}}", Hexagon), (")", "(", Cloud), ("(", ")", Rounded), ("[", "]", Square)]
        between open close = do
            let
                (_, rest) = T.breakOn open t
            inner <- T.stripPrefix open rest >>= T.stripSuffix close
            if T.null rest then Nothing else Just inner
        clean = T.replace "<br/>" "\n" . T.replace "<br>" "\n" . T.dropAround (`elem` ("\"`" :: String)) . T.strip

type Sized :: Type
data Sized = Sized {tree :: Tree, size :: Size, kids :: [Sized], span' :: Double}

draw :: Monad m => Measure m -> Mindmap -> m Scene
draw measure m = do
    sized <- sizeOf m.root
    let
        rootC = Point 0 0
        place dir c t k =
            let
                x = c.x + dir * (t.size.w / 2 + 50)
                heights = map (.span') t.kids
                total = sum heights + gapY * fromIntegral (max 0 (length t.kids - 1))
                tops = scanl (\a h -> a + h + gapY) (c.y - total / 2) heights
            in
                concat
                    [ let
                        cc = Point (x + dir * kid.size.w / 2) (top + kid.span' / 2)
                      in
                        edge c t cc kid dir k : place dir cc kid k <> shape cc kid k
                    | (kid, top) <- zip t.kids tops
                    ]
        side dir kidsHere =
            concat
                [ let
                    total = sum (map ((.span') . snd) kidsHere) + gapY * fromIntegral (max 0 (length kidsHere - 1))
                    tops = scanl (\a h -> a + h + gapY) (negate total / 2) (map ((.span') . snd) kidsHere)
                    top = tops !! i
                    c = Point (dir * (sized.size.w / 2 + 70 + kid.size.w / 2)) (top + kid.span' / 2)
                  in
                    edge rootC sized c kid dir k : place dir c kid k <> shape c kid k
                | (i, (k, kid)) <- zip [0 ..] kidsHere
                ]
        -- Alternate branches go right and left, so both sides stay even.
        indexed = zip [0 ..] sized.kids
        rights = [x | x@(i, _) <- indexed, even i]
        lefts = [x | x@(i, _) <- indexed, odd i]
    pure (scene 16 (side 1 rights <> side (-1) lefts <> shape rootC sized (-1)))
    where
        gapY = 14
        sizeOf t = do
            s <- measure (if null t.children then regular 12 else bold 13) t.label
            kids <- for t.children sizeOf
            let
                box = case t.shape of
                    Circle -> let d = max s.w s.h + 26 in Size d d
                    _ -> Size (s.w + 28) (max 30 (s.h + 16))
                below = sum (map (.span') kids) + gapY * fromIntegral (max 0 (length kids - 1))
            pure Sized {tree = t, size = box, kids = kids, span' = max box.h below}
        edge c t cc kid dir k =
            let
                a = Point (c.x + dir * t.size.w / 2) c.y
                b = Point (cc.x - dir * kid.size.w / 2) cc.y
                mid = (a.x + b.x) / 2
            in
                Path {start = a, segments = [CurveTo (Point mid a.y) (Point mid b.y) b], line = pen (Series k) 2, begin = NoHead, end = NoHead}
        shape c t k =
            let
                z = t.size
                (fill', line') = if k < 0 then (Tint 0, Series 0) else (Tint k, Series k)
                l = c.x - z.w / 2
                top = c.y - z.h / 2
                body = case t.tree.shape of
                    Circle -> [Ellipse {center = c, rx = z.w / 2, ry = z.h / 2, fill = Just fill', edge = Just (pen line' 1.4)}]
                    Square -> [Rect {at = Point l top, size = z, radius = 2, fill = Just fill', edge = Just (pen line' 1.4)}]
                    Hexagon -> [Polygon {points = [Point (l + 12) top, Point (l + z.w - 12) top, Point (l + z.w) c.y, Point (l + z.w - 12) (top + z.h), Point (l + 12) (top + z.h), Point l c.y], fill = Just fill', edge = Just (pen line' 1.4)}]
                    Bang -> [Polygon {points = star c z, fill = Just fill', edge = Just (pen line' 1.4)}]
                    Cloud -> [Rect {at = Point l top, size = z, radius = z.h / 2, fill = Just fill', edge = Just (dashed (pen line' 1.4))}]
                    Rounded -> [Rect {at = Point l top, size = z, radius = 10, fill = Just fill', edge = Just (pen line' 1.4)}]
                    Plain
                        | null t.kids -> [Path {start = Point l (top + z.h), segments = [LineTo (Point (l + z.w) (top + z.h))], line = pen line' 2, begin = NoHead, end = NoHead}]
                        | otherwise -> [Rect {at = Point l top, size = z, radius = z.h / 2, fill = Just fill', edge = Just (pen line' 1.4)}]
            in
                body <> [Text {at = c, text = t.tree.label, font = if null t.kids then regular 12 else bold 13, color = Ink, anchor = Middle}]
        star c z = [Point (c.x + (z.w / 2 + bump i) * cos a) (c.y + (z.h / 2 + bump i) * sin a) | i <- [0 .. 23 :: Int], let
                                                                                                                            a = 2 * pi * fromIntegral i / 24]
            where
                bump i = if even i then 6 else -2
