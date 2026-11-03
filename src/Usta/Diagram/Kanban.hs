{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid kanban boards: columns side by side with their cards, each
-- card's text wrapped, and its ticket, assignee and priority under it.
module Usta.Diagram.Kanban
  ( Kanban (..)
  , parse
  , draw
  ) where

import Data.Char (isSpace)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)

import Usta.Diagram.Scene

type Card :: Type
data Card = Card {text :: Text, ticket :: Maybe Text, assigned :: Maybe Text, priority :: Maybe Text}

type Kanban :: Type
newtype Kanban = Kanban {columns :: [(Text, [Card])]}

parse :: Text -> Either Text Kanban
parse source = case items of
  [] -> Left "The board has no columns."
  _ -> Right (Kanban (group items))
  where
    items =
      [ (T.length (T.takeWhile isSpace l), T.strip l)
      | l <- drop 1 (T.lines source), not (T.null (T.strip l)), not ("%%" `T.isPrefixOf` T.strip l) ]
    columnDepth = minimum (map fst items)
    group xs = case xs of
      [] -> []
      (_, c) : rest ->
        let (cards, more) = span ((> columnDepth) . fst) rest
         in (shown c, map (card . snd) cards) : group more
    -- "id[Text]" shows Text; plain text shows itself.
    shown t = let body = fst (T.breakOn "@{" t) in case T.breakOn "[" body of
      (_, b) | not (T.null b) -> T.dropAround (`elem` ("[]\" " :: String)) b
      _ -> T.strip body
    card t =
      let meta = snd (T.breakOn "@{" t)
          field k = case T.breakOn (k <> ":") meta of
            (_, rest) | not (T.null rest) -> Just (T.dropAround (`elem` ("'\" " :: String)) (T.takeWhile (\c -> c /= ',' && c /= '}') (T.drop (T.length k + 1) rest)))
            _ -> Nothing
       in Card {text = shown t, ticket = field "ticket", assigned = field "assigned", priority = field "priority"}

draw :: Monad m => Measure m -> Kanban -> m Scene
draw measure k = do
  laid <- for k.columns $ \(name, cards) -> do
    cs <- for cards $ \c -> do
      ls <- wrap measure (regular 12) (cardW - 24) c.text
      pure (c, ls)
    pure (name, cs)
  let colX i = fromIntegral i * (cardW + 40)
      columnItems = concat
        [ let heights = [fromIntegral (length ls) * 17 + 22 + (if hasMeta c then 20 else 0) | (c, ls) <- cs]
              tops = scanl (\y h -> y + h + 10) 40 heights
              bottom = last tops + 6
           in [ Rect {at = Point (colX i) 0, size = Size (cardW + 20) (max 80 bottom), radius = 10, fill = Just (Tint i), edge = Nothing}
              , Text {at = Point (colX i + 14) 20, text = name, font = bold 13, color = Ink, anchor = Start}
              , Text {at = Point (colX i + cardW + 6) 20, text = T.pack (show (length cs)), font = regular 11, color = Muted, anchor = End} ]
                <> concat
                  [ [ Rect {at = Point (colX i + 10) top, size = Size cardW h, radius = 6, fill = Just Paper, edge = Just (pen Border 1)}
                    , Rect {at = Point (colX i + 10) top, size = Size 4 h, radius = 2, fill = Just (priorityRole c.priority), edge = Nothing} ]
                      <> [Text {at = Point (colX i + 22) (top + 14 + 17 * fromIntegral n), text = l, font = regular 12, color = Ink, anchor = Start} | (n, l) <- zip [0 :: Int ..] ls]
                      <> [ Text {at = Point (colX i + 22) (top + h - 12), text = T.intercalate " · " meta, font = regular 10, color = Muted, anchor = Start}
                         | let meta = [x | Just x <- [c.ticket, c.assigned, c.priority]], not (null meta) ]
                  | ((c, ls), top, h) <- zip3 cs tops heights ]
        | (i, (name, cs)) <- zip [0 ..] laid ]
  pure (scene 12 columnItems)
  where
    cardW = 220 :: Double
    hasMeta c = any (/= Nothing) [c.ticket, c.assigned, c.priority]
    priorityRole p = case fmap T.toLower p of
      Just "very high" -> Danger
      Just "high" -> Series 2
      Just "low" -> Series 1
      Just "very low" -> Series 6
      _ -> Border

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
