{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid packet diagrams: a network packet's fields as boxes in rows
of 32 bits, with each field's first and last bit numbered above it.
-}
module Usta.Diagram.Packet
    ( Packet (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Traversable (for)

import Data.Text qualified as T
import Data.Text.Read qualified as R

import Usta.Diagram.Scene

-- | A field's first and last bit, and its name.
type Field :: Type
data Field = Field {from :: Int, to :: Int, name :: Text}

type Packet :: Type
data Packet = Packet {title :: Maybe Text, fields :: [Field]}

parse :: Text -> Either Text Packet
parse source = do
    fields <- go 0 [l | l <- lines', not ("title" `T.isPrefixOf` l)]
    if null fields then Left "The packet has no fields." else pure Packet {title, fields}
    where
        lines' = [l | l <- drop 1 (map T.strip (T.lines source)), not (T.null l), not ("%%" `T.isPrefixOf` l)]
        title = case [T.strip t | l <- lines', Just t <- [T.stripPrefix "title" l]] of
            t : _ -> Just t
            [] -> Nothing
        go _ [] = pure []
        go next (l : ls) = do
            let
                (bits, rest) = T.breakOn ":" l
                name = T.dropAround (`elem` ("\" " :: String)) (T.drop 1 rest)
                problem = "usta could not read \"" <> T.take 40 l <> "\"; a field is 0-15: \"Name\" or +16: \"Name\"."
            (a, b) <- case T.splitOn "-" (T.strip bits) of
                _ | Just n <- T.stripPrefix "+" (T.strip bits) -> maybe (Left problem) (\k -> pure (next, next + k - 1)) (number n)
                [x] -> maybe (Left problem) (\k -> pure (k, k)) (number x)
                [x, y] -> maybe (Left problem) pure ((,) <$> number x <*> number y)
                _ -> Left problem
            if T.null rest || b < a then Left problem else (Field a b name :) <$> go (b + 1) ls
        number t = case R.decimal (T.strip t) of
            Right (n, "") -> Just n
            _ -> Nothing

draw :: Monad m => Measure m -> Packet -> m Scene
draw measure p = do
    boxes <- for (concatMap pieces p.fields) $ \(row, a, b, f) -> do
        let
            x = fromIntegral (a - row * perRow) * bitW
            w = fromIntegral (b - a + 1) * bitW
            y = top + fromIntegral row * rowH + 14
        (label, font) <- fitting measure (w - 4) f.name
        pure
            $ [ Rect {at = Point x y, size = Size w boxH, radius = 0, fill = Just Paper, edge = Just (pen Line 1)}
              , Text {at = Point (x + w / 2) (y + boxH / 2), text = label, font = font, color = Ink, anchor = Middle}
              , Text {at = Point (x + 2) (y - 6), text = T.pack (show a), font = regular 9, color = Muted, anchor = Start}
              ]
                <> [Text {at = Point (x + w - 2) (y - 6), text = T.pack (show b), font = regular 9, color = Muted, anchor = End} | b > a]
    let
        titleItem = [Text {at = Point (fromIntegral perRow * bitW / 2) 12, text = t, font = bold 14, color = Ink, anchor = Middle} | Just t <- [p.title]]
    pure (scene 12 (concat boxes <> titleItem))
    where
        perRow = 32
        bitW = 26 :: Double
        boxH = 32
        rowH = boxH + 18
        top = maybe 0 (const 30) p.title
        -- A field split where it crosses into the next row.
        pieces f =
            [ (row, max f.from (row * perRow), min f.to (row * perRow + perRow - 1), f)
            | row <- [f.from `div` perRow .. f.to `div` perRow]
            ]

{- | The text in a font that fits @width@: smaller for a narrow field, and
cut short with an ellipsis when even that is too wide.
-}
fitting :: Monad m => Measure m -> Double -> Text -> m (Text, Font)
fitting measure width text = do
    s <- measure (regular 12) text
    if s.w <= width then pure (text, regular 12) else (,regular 9) <$> go (T.length text)
    where
        go n
            | n <= 0 = pure ""
            | otherwise = do
                let
                    t = if n == T.length text then text else T.take n text <> "…"
                s <- measure (regular 9) t
                if s.w <= width then pure t else go (n - 1)
