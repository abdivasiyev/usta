{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid sankey diagrams: flows from "source,target,value" rows, nodes
in columns by how far along they are, each as tall as what passes
through it, joined by bands as wide as their flow.
-}
module Usta.Diagram.Sankey
    ( Sankey (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.List (nub)
import Data.Text (Text)
import Text.Read (readMaybe)

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Usta.Diagram.Scene

type Sankey :: Type
newtype Sankey = Sankey {flows :: [(Text, Text, Double)]}

parse :: Text -> Either Text Sankey
parse source = do
    flows <- traverse row [l | l <- drop 1 (map T.strip (T.lines source)), not (T.null l), not ("%%" `T.isPrefixOf` l)]
    if null flows then Left "The sankey diagram has no flows." else pure (Sankey flows)
    where
        row l = case csv l of
            [a, b, v] | Just x <- readMaybe (T.unpack (T.strip v)), x > (0 :: Double) -> Right (a, b, x)
            _ -> Left ("usta could not read \"" <> T.take 40 l <> "\"; a flow is source,target,value.")
        -- Fields split at commas outside double quotes.
        csv l = map (T.strip . T.replace "\"\"" "\"" . T.dropAround (== '"') . T.strip) (go l)
            where
                go t
                    | T.null t = []
                    | "\"" `T.isPrefixOf` t = let (q, rest) = T.breakOn "\"," (T.drop 1 t) in ("\"" <> q <> "\"") : go (T.drop 2 rest)
                    | otherwise = let (f, rest) = T.breakOn "," t in f : (if T.null rest then [] else go (T.drop 1 rest))

draw :: Monad m => Measure m -> Sankey -> m Scene
draw _ s = pure (scene 12 (bands <> boxes <> labels))
    where
        names = nub (concat [[a, b] | (a, b, _) <- s.flows])
        outs n = sum [v | (a, _, v) <- s.flows, a == n]
        ins n = sum [v | (_, b, v) <- s.flows, b == n]
        through n = max (outs n) (ins n)
        -- Columns: how many flows lead to a node, at most.
        column = fixpoint (\m -> foldl' (\acc (a, b, _) -> Map.insert b (max (acc Map.! b) (acc Map.! a + 1)) acc) m s.flows) (Map.fromList [(n, 0 :: Int) | n <- names])
        columns = maximum (0 : Map.elems column)
        -- Nodes with nowhere to go sit in the last column.
        colOf n = if outs n == 0 then columns else column Map.! n
        height = 360 :: Double
        gap = 12 :: Double
        inColumn c = [n | n <- names, colOf n == c]
        scale = minimum [(height - gap * fromIntegral (length (inColumn c) - 1)) / max 0.0001 (sum (map through (inColumn c))) | c <- [0 .. columns]]
        tops =
            Map.fromList
                ( concat
                    [ zip ns (scanl (\y n -> y + through n * scale + gap) 0 ns)
                    | c <- [0 .. columns]
                    , let
                        ns = inColumn c
                    ]
                )
        xOf n = fromIntegral (colOf n) * 200
        nodeW = 14
        colorIx n = length (takeWhile (/= n) names)
        -- Where each flow leaves its source and enters its target, stacked.
        leaving = Map.fromList (concat [zip [(a, b) | (a', b, _) <- s.flows, a' == a] (scanl (+) 0 [v * scale | (a', _, v) <- s.flows, a' == a]) | a <- names])
        entering = Map.fromList (concat [zip [(a, b) | (a, b', _) <- s.flows, b' == b] (scanl (+) 0 [v * scale | (_, b', v) <- s.flows, b' == b]) | b <- names])
        bands =
            [ let
                x0 = xOf a + nodeW
                x1 = xOf b
                y0 = tops Map.! a + leaving Map.! (a, b)
                y1 = tops Map.! b + entering Map.! (a, b)
                h = v * scale
                curve ya yb =
                    [ Point (x0 + (x1 - x0) * t) (ya + (yb - ya) * (3 * t * t - 2 * t * t * t))
                    | k <- [0 .. 20 :: Int]
                    , let
                        t = fromIntegral k / 20
                    ]
              in
                Polygon {points = curve y0 y1 <> reverse (curve (y0 + h) (y1 + h)), fill = Just (Tint (colorIx a)), edge = Nothing}
            | (a, b, v) <- s.flows
            ]
        boxes = [Rect {at = Point (xOf n) (tops Map.! n), size = Size nodeW (max 2 (through n * scale)), radius = 2, fill = Just (Series (colorIx n)), edge = Nothing} | n <- names]
        labels =
            [ Text {at = Point (if colOf n == columns then xOf n - 6 else xOf n + nodeW + 6) (tops Map.! n + through n * scale / 2), text = n <> "  " <> number (through n), font = regular 12, color = Ink, anchor = if colOf n == columns then End else Start}
            | n <- names
            ]
        -- At most two decimals, without trailing zeros.
        number v =
            let
                cents = round (v * 100) :: Integer
                (whole, part) = cents `divMod` 100
            in
                T.pack (show whole) <> if part == 0 then "" else "." <> T.dropWhileEnd (== '0') (T.justifyRight 2 '0' (T.pack (show part)))

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f a = let b = f a in if b == a then a else fixpoint f b
