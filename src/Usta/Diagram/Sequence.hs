{-# LANGUAGE OverloadedStrings #-}

{- | Mermaid sequence diagrams: participants side by side with their
lifelines, messages between them top to bottom, activations, notes, and
loop / alt / opt / par / critical / break / rect blocks around them.
-}
module Usta.Diagram.Sequence
    ( Sequence (..)
    , Who (..)
    , Event (..)
    , Line (..)
    , Tip (..)
    , parse
    , draw
    )
where

import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Traversable (for)

import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Usta.Diagram.Scene

type Who :: Type
data Who = Who {name :: Text, label :: Text, actor :: Bool}

type Line :: Type
data Line = Plain | Dashes
    deriving stock (Eq)

type Tip :: Type
data Tip = NoTip | Filled | Crossed | Async
    deriving stock (Eq)

type Side :: Type
data Side = LeftOf | RightOf | Over
    deriving stock (Eq)

type Event :: Type
data Event
    = {- | 'activate' is 'Just' 'True' for "+" on the target, 'Just'
      'False' for "-" on the source.
      -}
      Message {from :: Text, to :: Text, text :: Text, line :: Line, tip :: Tip, both :: Bool, activate :: Maybe Bool}
    | Activate Text Bool
    | Note Side [Text] Text
    | -- | A block's kind ("loop", "alt", …) and its text.
      Open Text Text
    | -- | "else", "and" or "option", with its text.
      Divide Text Text
    | Close
    | Number

type Sequence :: Type
data Sequence = Sequence {who :: [Who], events :: [Event]}

parse :: Text -> Either Text Sequence
parse source = do
    events <- traverse event body
    let
        declared = [w | Just w <- map participant body]
        named = nub (map (.name) declared <> concatMap mentions events)
        whoFor n = fromMaybe Who {name = n, label = n, actor = False} (lookup n [(w.name, w) | w <- reverse declared])
    pure Sequence {who = map whoFor named, events = mapMaybe id events}
    where
        body = drop 1 (filter (not . T.null) (map T.strip (T.lines source)))
        mentions e = case e of
            Just Message {from = a, to = b} -> [a, b]
            Just (Note _ ws _) -> ws
            _ -> []

participant :: Text -> Maybe Who
participant line = case T.words line of
    (kind : rest)
        | kind `elem` ["participant", "actor"]
        , not (null rest) ->
            let
                spec = T.unwords rest
                (n, alias) = T.breakOn " as " spec
                clean = T.takeWhile (/= '@') (T.strip n)
            in
                Just Who {name = clean, label = if T.null alias then clean else T.strip (T.drop 4 alias), actor = kind == "actor"}
    _ -> Nothing

event :: Text -> Either Text (Maybe Event)
event line
    | Just _ <- participant line = Right Nothing
    | any (`T.isPrefixOf` line) ["create ", "destroy ", "box ", "links ", "link ", "title ", "accTitle", "accDescr"] = Right Nothing
    | line == "autonumber" || "autonumber " `T.isPrefixOf` line = Right (Just Number)
    | line == "end" = Right (Just Close)
    | Just rest <- T.stripPrefix "activate " line = Right (Just (Activate (T.strip rest) True))
    | Just rest <- T.stripPrefix "deactivate " line = Right (Just (Activate (T.strip rest) False))
    | (kind : rest) <- T.words line, kind `elem` ["loop", "alt", "opt", "par", "critical", "break", "rect"] = Right (Just (Open kind (T.unwords rest)))
    | (kind : rest) <- T.words line, kind `elem` ["else", "and", "option"] = Right (Just (Divide kind (T.unwords rest)))
    | "note " `T.isPrefixOf` T.toLower line = Just <$> note line
    | otherwise = Just <$> message line

note :: Text -> Either Text Event
note line =
    let
        (spec, text) = T.breakOn ":" (T.drop 5 line)
        ws = T.words (T.toLower spec)
        side = case ws of
            "left" : _ -> LeftOf
            "right" : _ -> RightOf
            _ -> Over
        target = T.strip (T.unwords (drop (if side == Over then 1 else 2) (T.words spec)))
    in
        Right (Note side (map T.strip (T.splitOn "," target)) (breaks (T.strip (T.drop 1 text))))

-- | "A->>+B: text" and every arrow Mermaid has.
message :: Text -> Either Text Event
message line =
    case [(a, T.drop (T.length arrow) rest, arrow) | arrow <- arrows, let
                                                                        (a, rest) = T.breakOn arrow spec, not (T.null rest), not (T.null (T.strip a))] of
        (a, b, arrow) : _ ->
            let
                (sign, target) = case T.uncons (T.strip b) of
                    Just ('+', t) -> (Just True, t)
                    Just ('-', t) -> (Just False, t)
                    _ -> (Nothing, T.strip b)
            in
                Right
                    Message
                        { from = T.strip a
                        , to = T.strip target
                        , text = breaks (T.strip (T.drop 1 text))
                        , line = if "--" `T.isPrefixOf` T.dropWhile (== '<') arrow then Dashes else Plain
                        , tip = tipOf arrow
                        , both = "<<" `T.isPrefixOf` arrow
                        , activate = sign
                        }
        [] -> Left ("usta could not read \"" <> T.take 40 line <> "\" in the sequence diagram.")
    where
        (spec, text) = T.breakOn ":" line
        -- Longest first, so "-->>" is not read as "->".
        arrows = ["<<-->>", "<<->>", "-->>", "->>", "--x", "-x", "--)", "-)", "-->", "->"]
        tipOf arrow
            | ">>" `T.isSuffixOf` arrow = Filled
            | "x" `T.isSuffixOf` arrow = Crossed
            | ")" `T.isSuffixOf` arrow = Async
            | otherwise = NoTip

breaks :: Text -> Text
breaks = T.replace "<br>" "\n" . T.replace "<br/>" "\n" . T.replace "<br />" "\n"

-- | Rows from the top, one per message, note or block edge.
draw :: Monad m => Measure m -> Sequence -> m Scene
draw measure sq = do
    boxes <- for sq.who $ \w -> (,) w.name <$> measure (regular 13) w.label
    texts <- for sq.events $ \e -> case e of
        Message {text = t} -> Just <$> measure (regular 12) t
        Note _ _ t -> Just <$> measure (regular 12) t
        Open _ t -> Just <$> measure (bold 11) t
        Divide _ t -> Just <$> measure (regular 11) t
        _ -> pure Nothing
    let
        widthOf n = maybe 80 (\s -> max 80 (s.w + 28)) (lookup n boxes)
        order = map (.name) sq.who
        indexOf n = fromMaybe 0 (lookup n (zip order [0 :: Int ..]))
        -- Room between neighbours for the widest message between them.
        needs =
            Map.fromListWith
                max
                [ (min (indexOf a) (indexOf b), s.w + 40)
                | (Message {from = a, to = b}, Just s) <- zip sq.events texts
                , a /= b
                , abs (indexOf a - indexOf b) == 1
                ]
        selfRoom = Map.fromListWith max [(indexOf a, s.w + 60) | (Message {from = a, to = b}, Just s) <- zip sq.events texts, a == b]
        columns = snd (foldl' place (0, []) (zip [0 ..] order))
        place (x, acc) (i, n) =
            let
                w = widthOf n
                center = x + w / 2
                gap = maximum [40, Map.findWithDefault 0 i needs - w / 2, Map.findWithDefault 0 i selfRoom]
            in
                (center + w / 2 + gap, acc <> [(n, center)])
        xOf n = fromMaybe 0 (lookup n columns)
        headerH = 40
        lefts = [c - widthOf n / 2 | (n, c) <- columns]
        rights = [c + widthOf n / 2 | (n, c) <- columns]
        span' = (minimum (0 : lefts) - 10, maximum (0 : rights) + 10)
        -- Walk the events, keeping the y, open activations and blocks.
        start = Walk {y = headerH + 24, items = [], active = Map.empty, blocks = [], count = Nothing}
        done = foldl' (step xOf span') start (zip sq.events texts)
        bottom = done.y + 8
        heads =
            concat
                [ participantBox w (xOf w.name) (widthOf w.name) top
                | w <- sq.who
                , top <- [0, bottom]
                ]
        lifelines =
            [ Path {start = Point (xOf w.name) headerH, segments = [LineTo (Point (xOf w.name) bottom)], line = dashed (pen Border 1), begin = NoHead, end = NoHead}
            | w <- sq.who
            ]
    pure (scene 12 (lifelines <> reverse done.items <> heads))

type Walk :: Type
data Walk = Walk
    { y :: Double
    , items :: [Item]
    -- ^ Newest first.
    , active :: Map.Map Text [Double]
    -- ^ Where each open activation of a participant began, innermost first.
    , blocks :: [(Text, Text, Double, [Double])]
    -- ^ Open blocks: kind, text, top, and where it was divided.
    , count :: Maybe Int
    }

step :: (Text -> Double) -> (Double, Double) -> Walk -> (Event, Maybe Size) -> Walk
step xOf (left, right) w (e, size') = case e of
    Number -> w {count = Just 0}
    Activate n True -> w {active = Map.insertWith (<>) n [w.y - 6] w.active}
    Activate n False -> closeActivation n w
    Message {from = a, to = b, text = t, line = l, tip = tp, both = two, activate = act} ->
        let
            h = maybe 16 (.h) size'
            numbered = case w.count of
                Just k -> (Just (k + 1), T.pack (show (k + 1)) <> ". " <> t)
                Nothing -> (Nothing, t)
            label = snd numbered
            y' = w.y + h + 6
            depth n = length (Map.findWithDefault [] n w.active)
            offset n toward = fromIntegral (depth n) * 5 * signum toward
            x1 = xOf a + offset a (xOf b - xOf a)
            x2 = xOf b - offset b (xOf b - xOf a)
            lineOf = if l == Dashes then dashed (pen Line 1.3) else pen Line 1.3
            headOf = case tp of
                Filled -> Arrow
                Crossed -> Cross
                Async -> Hollow
                NoTip -> NoHead
            shapes
                | a == b =
                    let
                        x = xOf a + offset a 1
                    in
                        [ Path {start = Point x y', segments = [LineTo (Point (x + 36) y'), LineTo (Point (x + 36) (y' + 22)), LineTo (Point (x + 2) (y' + 22))], line = lineOf, begin = NoHead, end = headOf}
                        , Text {at = Point (x + 44) (y' + 11 - h / 2 + 2), text = label, font = regular 12, color = Ink, anchor = Start}
                        ]
                | otherwise =
                    [ Path {start = Point x1 y', segments = [LineTo (Point x2 y')], line = lineOf, begin = if two then headOf else NoHead, end = headOf}
                    , Text {at = Point ((x1 + x2) / 2) (y' - 4 - h / 2), text = label, font = regular 12, color = Ink, anchor = Middle}
                    ]
            next = w {y = y' + (if a == b then 34 else 18), items = reverse shapes <> w.items, count = fst numbered}
        in
            case act of
                Just True -> next {active = Map.insertWith (<>) b [y' - 2] next.active}
                Just False -> closeActivation a next
                Nothing -> next
    Note side ns t ->
        let
            s = fromMaybe (Size 60 16) size'
            xs = map xOf ns
            (x0, x1) = case side of
                LeftOf -> (minimum xs - s.w - 34, minimum xs - 10)
                RightOf -> (maximum xs + 10, maximum xs + s.w + 34)
                Over -> let c = (minimum xs + maximum xs) / 2; half = max ((maximum xs - minimum xs) / 2 + 30) (s.w / 2 + 12) in (c - half, c + half)
            boxH = s.h + 14
            shapes =
                [ Rect {at = Point x0 (w.y + 4), size = Size (x1 - x0) boxH, radius = 4, fill = Just (Tint 4), edge = Just (pen (Series 4) 1)}
                , Text {at = Point ((x0 + x1) / 2) (w.y + 4 + boxH / 2), text = t, font = regular 12, color = Ink, anchor = Middle}
                ]
        in
            w {y = w.y + boxH + 16, items = reverse shapes <> w.items}
    Open kind t -> w {y = w.y + 30, blocks = (kind, t, w.y + 4, []) : w.blocks}
    Divide _ t -> case w.blocks of
        (k, t0, top, divs) : rest ->
            let
                line' = Path {start = Point left (w.y + 2), segments = [LineTo (Point right (w.y + 2))], line = dashed (pen Border 1), begin = NoHead, end = NoHead}
                tag = Text {at = Point (left + 10) (w.y + 14), text = "[" <> t <> "]", font = regular 11, color = Muted, anchor = Start}
            in
                w {y = w.y + 28, items = tag : line' : w.items, blocks = (k, t0, top, w.y : divs) : rest}
        [] -> w
    Close -> case w.blocks of
        (kind, t, top, _) : rest ->
            let
                inset = fromIntegral (length rest) * 6
                frame = case kind of
                    "rect" -> [Rect {at = Point (left + inset) top, size = Size (right - left - 2 * inset) (w.y - top), radius = 4, fill = Just (Tint 0), edge = Nothing}]
                    _ ->
                        [ Rect {at = Point (left + inset) top, size = Size (right - left - 2 * inset) (w.y - top), radius = 4, fill = Nothing, edge = Just (pen Border 1.1)}
                        , Rect {at = Point (left + inset) top, size = Size (fromIntegral (T.length kind) * 7 + 18) 18, radius = 4, fill = Just Border, edge = Nothing}
                        , Text {at = Point (left + inset + 8) (top + 9), text = kind, font = bold 11, color = Paper, anchor = Start}
                        , Text {at = Point (left + inset + fromIntegral (T.length kind) * 7 + 26) (top + 9), text = if T.null t then "" else "[" <> t <> "]", font = regular 11, color = Muted, anchor = Start}
                        ]
            in
                w {y = w.y + 10, items = w.items <> reverse frame, blocks = rest}
        [] -> w
    where
        closeActivation n walk = case Map.findWithDefault [] n walk.active of
            top : more ->
                let
                    depth' = fromIntegral (length more)
                    bar = Rect {at = Point (xOf n - 5 + depth' * 5) top, size = Size 10 (walk.y - 10 - top), radius = 1, fill = Just (Tint 0), edge = Just (pen (Series 0) 1)}
                in
                    walk {items = walk.items <> [bar], active = Map.insert n more walk.active}
            [] -> walk

-- | A participant's box, or an actor's figure, with its name.
participantBox :: Who -> Double -> Double -> Double -> [Item]
participantBox w x width top
    | w.actor =
        [ Ellipse {center = Point x (top + 7), rx = 6, ry = 6, fill = Just Paper, edge = Just (pen Line 1.3)}
        , Path {start = Point x (top + 13), segments = [LineTo (Point x (top + 26))], line = pen Line 1.3, begin = NoHead, end = NoHead}
        , Path {start = Point (x - 9) (top + 17), segments = [LineTo (Point (x + 9) (top + 17))], line = pen Line 1.3, begin = NoHead, end = NoHead}
        , Path {start = Point (x - 8) (top + 36), segments = [LineTo (Point x (top + 26)), LineTo (Point (x + 8) (top + 36))], line = pen Line 1.3, begin = NoHead, end = NoHead}
        , Text {at = Point x (top + 46), text = w.label, font = regular 12, color = Ink, anchor = Middle}
        ]
    | otherwise =
        [ Rect {at = Point (x - width / 2) top, size = Size width 36, radius = 6, fill = Just Paper, edge = Just (pen Border 1.2)}
        , Text {at = Point x (top + 18), text = w.label, font = regular 13, color = Ink, anchor = Middle}
        ]
