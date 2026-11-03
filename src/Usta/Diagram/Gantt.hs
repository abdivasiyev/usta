{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid Gantt charts: tasks as bars on a date axis, grouped in
-- sections, done, active, critical or milestones, starting on a date or
-- after other tasks.
module Usta.Diagram.Gantt
  ( Gantt (..)
  , parse
  , draw
  ) where

import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Traversable (for)
import Text.Read (readMaybe)

import Usta.Diagram.Scene

type Tag :: Type
data Tag = Done | Active | Critical | Milestone
  deriving stock (Eq)

type Task :: Type
data Task = Task {name :: Text, section :: Text, tags :: [Tag], from :: LocalTime, to :: LocalTime}

type Gantt :: Type
data Gantt = Gantt {title :: Maybe Text, tasks :: [Task]}

type Reading :: Type
data Reading = Reading
  { format :: String
  , weekends :: Bool
  , section' :: Text
  , ends :: Map.Map Text (LocalTime, LocalTime)
  , previous :: Maybe LocalTime
  , done :: [Task]
  }

parse :: Text -> Either Text Gantt
parse source = do
  let lines' = drop 1 (filter (not . T.null) (map T.strip (T.lines source)))
      title = case [T.strip t | l <- lines', Just t <- [T.stripPrefix "title " l]] of
        t : _ -> Just t
        [] -> Nothing
  r <- foldl' (\acc l -> acc >>= \x -> line x l) (Right (Reading "%Y-%m-%d" False "" Map.empty Nothing [])) lines'
  if null r.done then Left "The Gantt chart has no tasks." else pure Gantt {title = title, tasks = reverse r.done}

line :: Reading -> Text -> Either Text Reading
line r l
  | Just f <- T.stripPrefix "dateFormat " l = Right r {format = strftime (T.strip f)}
  | Just e <- T.stripPrefix "excludes " l = Right r {weekends = "weekends" `T.isInfixOf` e}
  | Just s <- T.stripPrefix "section " l = Right r {section' = T.strip s}
  | any (`T.isPrefixOf` l) ["title ", "axisFormat", "tickInterval", "todayMarker", "includes", "weekday", "inclusiveEndDates", "topAxis", "displayMode", "click ", "%%"] = Right r
  | (name, spec) <- T.breakOn ":" l, not (T.null spec) = task r (T.strip name) (map T.strip (T.splitOn "," (T.drop 1 spec)))
  | otherwise = Left ("usta could not read \"" <> T.take 40 l <> "\" in the Gantt chart.")

-- | "done, a1, 2024-01-01, 3d": tags first, then an id, a start and an end
-- (a date, a length, or "until"), each optional but the end.
task :: Reading -> Text -> [Text] -> Either Text Reading
task r name parts = do
  let tagOf p = case p of
        "done" -> Just Done
        "active" -> Just Active
        "crit" -> Just Critical
        "milestone" -> Just Milestone
        _ -> Nothing
      tags = mapMaybe tagOf parts
      rest = filter (\p -> tagOf p == Nothing && not (T.null p)) parts
      (key, timing) = case rest of
        [i, s, e] -> (Just i, (Just s, e))
        [s, e] | isStart s -> (Nothing, (Just s, e))
               | otherwise -> (Just s, (Nothing, e))
        [e] -> (Nothing, (Nothing, e))
        _ -> (Nothing, (Nothing, "1d"))
      isStart s = "after " `T.isPrefixOf` s || maybe False (const True) (date s)
      date s = parseTimeM True defaultTimeLocale r.format (T.unpack s) :: Maybe LocalTime
  start <- case fst timing of
    Just s | Just ids <- T.stripPrefix "after " s ->
      case [snd e | i <- T.words ids, Just e <- [Map.lookup i r.ends]] of
        [] -> Left ("No task " <> ids <> " comes before \"" <> name <> "\".")
        es -> Right (maximum es)
    Just s -> maybe (Left ("usta could not read the date \"" <> s <> "\".")) Right (date s)
    Nothing -> maybe (Left ("\"" <> name <> "\" needs a start date.")) Right r.previous
  end <- case snd timing of
    e | Just i <- T.stripPrefix "until " e -> maybe (Left ("No task " <> i <> " for \"" <> name <> "\".")) (Right . fst) (Map.lookup (T.strip i) r.ends)
      | Just d <- date e -> Right d
      | Just span' <- duration e -> Right (advance r.weekends span' start)
      | otherwise -> Left ("usta could not read \"" <> e <> "\" as a date or a length.")
  let t = Task {name = name, section = r.section', tags = tags, from = start, to = max start end}
  pure r {ends = maybe r.ends (\k -> Map.insert k (t.from, t.to) r.ends) key, previous = Just t.to, done = t : r.done}

-- | "3d", "2w", "6h", "30m" as days and seconds.
duration :: Text -> Maybe (Integer, Integer)
duration t = do
  let (n, unit) = T.span (\c -> c `elem` ['0' .. '9'] || c == '.') t
  v <- readMaybe (T.unpack n) :: Maybe Double
  case unit of
    "d" -> Just (round v, 0)
    "w" -> Just (round (v * 7), 0)
    "h" -> Just (0, round (v * 3600))
    "m" -> Just (0, round (v * 60))
    "s" -> Just (0, round v)
    "ms" -> Just (0, 0)
    _ -> Nothing

-- | @start@ plus the days and seconds, the days skipping weekends when
-- they are excluded.
advance :: Bool -> (Integer, Integer) -> LocalTime -> LocalTime
advance skipWeekends (days, seconds) start =
  addLocalTime (fromIntegral seconds) start {localDay = go (localDay start) days}
  where
    go d 0 = d
    go d n
      | skipWeekends && dayOfWeek (addDays 1 d) `elem` [Saturday, Sunday] = go (addDays 1 d) n
      | otherwise = go (addDays 1 d) (n - 1)

-- | Mermaid's date tokens as a format for "Data.Time".
strftime :: Text -> String
strftime f = T.unpack (foldl' (\t (a, b) -> T.replace a b t) f [("YYYY", "%Y"), ("MM", "%m"), ("DD", "%d"), ("HH", "%H"), ("mm", "%M"), ("ss", "%S")])

draw :: Monad m => Measure m -> Gantt -> m Scene
draw measure g = do
  names <- for g.tasks $ \t -> measure (regular 12) t.name
  sections <- for (sectionsOf g.tasks) $ \s -> measure (bold 12) s
  let labelW = maximum (60 : map (.w) names <> map (.w) sections) + 40
      first = minimum (map (.from) g.tasks)
      last' = maximum (map (.to) g.tasks)
      days = max 1 (realToFrac (diffLocalTime last' first) / 86400) :: Double
      perDay = max 4 (min 48 (720 / days))
      xOf t = labelW + realToFrac (diffLocalTime t first) / 86400 * perDay
      width = labelW + days * perDay
      rowH = 28
      top = 30 + maybe 0 (const 30) g.title
      -- Rows: a header row per section, then its tasks.
      rows = concat [Left s : map Right ts | (s, ts) <- grouped g.tasks]
      bottom = top + rowH * fromIntegral (length rows)
      sectionBands = concat
        [ [ Rect {at = Point 0 (top + rowH * fromIntegral i), size = Size width rowH, radius = 0, fill = Just (Tint (k `mod` 2 * 6)), edge = Nothing}
          , Text {at = Point 10 (top + rowH * fromIntegral i + rowH / 2), text = s, font = bold 12, color = Ink, anchor = Start} ]
        | (k, (i, s)) <- zip [0 :: Int ..] [(i, s) | (i, Left s) <- zip [0 :: Int ..] rows], not (T.null s) ]
      step' = tickDays days
      ticks = takeWhile (\d -> diffDays d (localDay last') <= 0) (iterate (addDays step') (localDay first))
      grid = concat
        [ [ Path {start = Point x (top - 4), segments = [LineTo (Point x bottom)], line = pen Border 0.8, begin = NoHead, end = NoHead}
          , Text {at = Point x (top - 14), text = T.pack (formatTime defaultTimeLocale "%b %-d" d), font = regular 10, color = Muted, anchor = Middle} ]
        | d <- ticks, let x = xOf (LocalTime d midnight) ]
      bars = concat
        [ let y = top + rowH * fromIntegral i + rowH / 2
              x0 = xOf t.from
              x1 = max (x0 + 3) (xOf t.to)
              (fill', edge') = colorsOf t.tags
           in Text {at = Point 22 y, text = t.name, font = regular 12, color = Ink, anchor = Start}
                : if Milestone `elem` t.tags
                    then [Polygon {points = [Point x0 (y - 9), Point (x0 + 9) y, Point x0 (y + 9), Point (x0 - 9) y], fill = Just edge', edge = Nothing}]
                    else [Rect {at = Point x0 (y - 9), size = Size (x1 - x0) 18, radius = 4, fill = Just fill', edge = Just (pen edge' 1.2)}]
        | (i, Right t) <- zip [0 :: Int ..] rows ]
      titleItem = [Text {at = Point (width / 2) 12, text = t, font = bold 14, color = Ink, anchor = Middle} | Just t <- [g.title]]
  pure (scene 12 (sectionBands <> grid <> bars <> titleItem))
  where
    sectionsOf ts = map fst (grouped ts)
    grouped ts = foldr (\t acc -> case acc of
                          (s, xs) : more | s == t.section -> (s, t : xs) : more
                          _ -> (t.section, [t]) : acc) [] ts
    colorsOf tags
      | Critical `elem` tags = (DangerTint, Danger)
      | Done `elem` tags = (Tint 0, Border)
      | Active `elem` tags = (Tint 0, Series 0)
      | otherwise = (Series 0, Series 0)

-- | Days between grid lines, so there are about eight.
tickDays :: Double -> Integer
tickDays days = head' [d | d <- [1, 2, 7, 14, 30, 61, 91, 182, 365], fromIntegral d * 8 >= days]
  where
    head' xs = case xs of
      x : _ -> x
      [] -> 365
