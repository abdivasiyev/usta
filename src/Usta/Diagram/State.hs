{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid state diagrams: states, start and end points, transitions
-- with their events, composite states as frames, forks, joins, choices
-- and notes, laid out in ranks like a flowchart.
module Usta.Diagram.State
  ( States (..)
  , parse
  , draw
  ) where

import Data.Kind (Type)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)

import Usta.Diagram.Layered (Box (..), Layout (..), Link (..), Placed (..), Routed (..), layered, spacing)
import qualified Usta.Diagram.Route as Route
import Usta.Diagram.Scene

type Kind :: Type
data Kind = Plain | Initial | Finish | Fork | Choice
  deriving stock (Eq)

type State :: Type
data State = State {name :: Text, label :: Text, kind :: Kind, details :: [Text]}

type Transition :: Type
data Transition = Transition {from :: Text, to :: Text, event :: Maybe Text}

type Composite :: Type
data Composite = Composite {name :: Text, title :: Text, members :: [Text]}

type Note :: Type
data Note = Note {target :: Text, onLeft :: Bool, text :: Text}

type States :: Type
data States = States
  { across :: Bool
  , states :: [State]
  , transitions :: [Transition]
  , composites :: [Composite]
  , notes :: [Note]
  }

type Reading :: Type
data Reading = Reading
  { found :: Map.Map Text State
  , order :: [Text]
  , moves :: [Transition]
  , scopes :: [(Text, Text, [Text])]
    -- ^ Open composites: name, title, members so far.
  , done :: [Composite]
  , notes' :: [Note]
  , pendingNote :: Maybe (Text, Bool, [Text])
  }

parse :: Text -> Either Text States
parse source = do
  r <- go (Reading Map.empty [] [] [] [] [] Nothing) (drop 1 (filter (not . T.null) (map T.strip (T.lines source))))
  let stateOf n = fromMaybe (State n n Plain []) (Map.lookup n r.found)
  pure States
    { across = any (\l -> T.strip l == "direction LR" || T.strip l == "direction RL") (T.lines source)
    , states = map stateOf (nub (reverse r.order))
    , transitions = reverse r.moves
    , composites = reverse r.done
    , notes = reverse r.notes'
    }
  where
    go r lines' = case lines' of
      [] -> Right r
      line : rest -> case r.pendingNote of
        Just (t, left, acc)
          | T.toLower line == "end note" -> go r {pendingNote = Nothing, notes' = Note t left (T.intercalate "\n" (reverse acc)) : r.notes'} rest
          | otherwise -> go r {pendingNote = Just (t, left, line : acc)} rest
        Nothing -> step r line >>= \r' -> go r' rest
    scope r = case r.scopes of
      (n, _, _) : _ -> n
      [] -> ""
    -- Start and end points are their own in each composite.
    point r end = "[*]" <> scope r <> (if end then "/end" else "/start")
    add n st r =
      let r1 = r {found = Map.insertWith keep n st r.found, order = n : r.order}
       in case r1.scopes of
            (s, t, ms) : outer | n /= s -> r1 {scopes = (s, t, if n `elem` ms then ms else ms <> [n]) : outer}
            _ -> r1
    keep new old = if old.kind == Plain && null old.details && old.label == old.name then new {details = old.details <> new.details} else old {details = old.details <> new.details}
    nameOf r t end
      | t == "[*]" = (point r end, State (point r end) "" (if end then Finish else Initial) [])
      | otherwise = (t, State t t Plain [])
    step r line
      | any (`T.isPrefixOf` line) ["direction ", "classDef ", "class ", "style ", "click ", "%%"] = Right r
      | line == "--" = Right r
      | line == "}" = case r.scopes of
          (n, t, ms) : outer -> Right r {scopes = outer, done = Composite n t ms : r.done}
          [] -> Right r
      | Just spec <- T.stripPrefix "state " line = Right (stateLine r spec)
      | "note " `T.isPrefixOf` T.toLower line =
          let (spec, text) = T.breakOn ":" (T.drop 5 line)
              ws = T.words spec
              left = take 1 ws == ["left"]
              t = T.unwords (drop 2 ws)
           in Right $ if T.null text
                then r {pendingNote = Just (t, left, [])}
                else r {notes' = Note t left (T.strip (T.drop 1 text)) : r.notes'}
      | (spec, event) <- T.breakOn ":" line, "-->" `T.isInfixOf` spec =
          let (a, b) = T.breakOn "-->" spec
              (na, sa) = nameOf r (T.strip a) False
              (nb, sb) = nameOf r (T.strip (T.drop 3 b)) True
              r' = add nb sb (add na sa r)
           in Right r' {moves = Transition na nb (if T.null event then Nothing else Just (T.strip (T.drop 1 event))) : r'.moves}
      | (n, d) <- T.breakOn ":" line, not (T.null d), [n'] <- T.words n =
          Right (add n' (State n' n' Plain [T.strip (T.drop 1 d)]) r)
      | [n] <- T.words line = Right (add n (State n n Plain []) r)
      | otherwise = Left ("usta could not read \"" <> T.take 40 line <> "\" in the state diagram.")
    stateLine r spec
      -- state "Long name" as L
      | "\"" `T.isPrefixOf` spec, (q, rest) <- T.breakOn "\"" (T.drop 1 spec), Just n <- T.stripPrefix " as " (T.drop 1 rest) =
          let n' = T.strip (T.takeWhile (/= '{') n)
              r' = add n' (State n' q Plain []) r
           in if "{" `T.isSuffixOf` spec then r' {scopes = (n', q, []) : r'.scopes} else r'
      | "{" `T.isSuffixOf` spec =
          let n = T.strip (T.dropWhileEnd (== '{') spec)
              r' = add n (State n n Plain []) r
           in r' {scopes = (n, n, []) : r'.scopes}
      | (n : rest) <- T.words spec =
          let kind = case rest of
                ["<<fork>>"] -> Fork
                ["<<join>>"] -> Fork
                ["<<choice>>"] -> Choice
                _ -> Plain
           in add n (State n n kind []) r
      | otherwise = r

-- | Each composite is laid out on its own first, then drawn as one box
-- in the level around it; a transition into or out of a composite meets
-- its frame. Notes are boxes of their own, joined to their state, so they
-- never cover another.
draw :: Monad m => Measure m -> States -> m Scene
draw measure d = do
  (_, items) <- level ""
  pure (scene 12 items)
  where
    composite n = [c | c <- d.composites, c.name == n]
    parentOf n = case [c.name | c <- d.composites, n `elem` c.members] of
      p : _ -> p
      [] -> ""
    -- The member of @scope@ that holds @n@, or nothing outside it.
    liftTo scope n
      | parentOf n == scope = Just n
      | parentOf n == "" = Nothing
      | otherwise = liftTo scope (parentOf n)
    noteKey i = "\0note" <> T.pack (show (i :: Int))
    level scope = do
      let here = [s | s <- d.states, parentOf s.name == scope]
          notesHere = [(i, n) | (i, n) <- zip [0 ..] d.notes, maybe False ((== scope) . parentOf) (Just n.target), any ((== n.target) . (.name)) here]
      children <- for here $ \s -> case composite s.name of
        c : _ -> do
          (inner, items) <- level c.name
          title <- measure (bold 12) c.title
          let z = Size (max (inner.w + 24) (title.w + 28)) (inner.h + 44)
          pure (s, z, Just (c, inner, items))
        [] -> do
          title <- measure (regular 13) s.label
          lines' <- for s.details (measure (regular 11))
          let z = case s.kind of
                Initial -> Size 16 16
                Finish -> Size 20 20
                Fork -> Size 72 8
                Choice -> Size 30 30
                Plain -> Size (maximum (title.w : map (.w) lines') + 30) (title.h + 18 + (if null lines' then 0 else sum (map (.h) lines') + 10))
          pure (s, z, Nothing)
      noteSizes <- for notesHere $ \(i, n) -> (,,) i n <$> measure (regular 12) n.text
      let moves =
            [ (a, b, t)
            | t <- d.transitions
            , Just a <- [liftTo scope t.from]
            , Just b <- [liftTo scope t.to]
            , a /= b || t.from == t.to ]
      events <- for moves $ \(_, _, t) -> traverse (measure (regular 11)) t.event
      let turn z = if d.across then Size z.h z.w else z
          boxes = [Box s.name (turn z) | (s, z, _) <- children] <> [Box (noteKey i) (turn (Size (z.w + 16) (z.h + 14))) | (i, _, z) <- noteSizes]
          links = [Link a b (maybe 0 (\z -> 10 + (if d.across then z.w else z.h)) e) | ((a, b, _), e) <- zip moves events]
            -- A note on the left comes before its state in the flow, one on
            -- the right after it.
            <> [if n.onLeft then Link (noteKey i) n.target 0 else Link n.target (noteKey i) 0 | (i, n, _) <- noteSizes]
          result = layered spacing boxes links
          place p = if d.across then Point p.y p.x else p
          centers = Map.fromList [(p.key, place p.center) | p <- result.placed]
          sizes = Map.fromList ([(s.name, z) | (s, z, _) <- children] <> [(noteKey i, Size (z.w + 16) (z.h + 14)) | (i, _, z) <- noteSizes])
          kinds = Map.fromList [(s.name, s.kind) | (s, _, _) <- children]
          routes = Map.fromListWith (flip (<>)) [((r.from, r.to), [map place r.points]) | r <- result.routed]
          outlineOf n = case Map.lookup n kinds of
            Just Choice -> Route.Rhombus
            Just Initial -> Route.Round
            Just Finish -> Route.Round
            _ -> Route.Boxy
          ends a b = do
            c0 <- Map.lookup a centers
            z0 <- Map.lookup a sizes
            c1 <- Map.lookup b centers
            z1 <- Map.lookup b sizes
            pure (c0, z0, c1, z1)
          path' a b ps extra = fromMaybe [] $ do
            (c0, z0, c1, z1) <- ends a b
            let inner = drop 1 (take (length ps - 1) ps)
                p0 = Route.clip (outlineOf a) c0 z0 (case inner of { x : _ -> x; [] -> c1 }) 0
                p1 = Route.clip (outlineOf b) c1 z1 (if null inner then c0 else last inner) extra
            pure (p0 : inner <> [p1])
          arrows = concat (snd (foldl take' (routes, []) (zip moves events)))
          take' (left, acc) ((a, b, t), e) = case Map.lookup (a, b) left of
            Just (ps : more) -> (Map.insert (a, b) more left, acc <> [arrow a b t e ps])
            _ -> (left, acc)
          arrow a b t e ps = case path' a b ps 2 of
            [] -> []
            path ->
              let mid = Route.middle path
               in Path {start = head' path, segments = Route.smooth path, line = pen Line 1.3, begin = NoHead, end = Arrow}
                    : concat [ [ Rect {at = Point (mid.x - z.w / 2 - 4) (mid.y - z.h / 2 - 1), size = Size (z.w + 8) (z.h + 2), radius = 3, fill = Just Paper, edge = Nothing}
                               , Text {at = mid, text = ev, font = regular 11, color = Muted, anchor = Middle} ]
                             | (Just ev, Just z) <- [(t.event, e)] ]
          joins = concat
            [ let (a, b) = if n.onLeft then (noteKey i, n.target) else (n.target, noteKey i)
               in case path' a b (fromMaybe [] (Map.lookup (a, b) routes >>= listToMaybe')) 0 of
                    [] -> []
                    path -> [Path {start = head' path, segments = Route.smooth path, line = dashed (pen Border 1), begin = NoHead, end = NoHead}]
            | (i, n, _) <- noteSizes ]
          listToMaybe' xs = case xs of
            x : _ -> Just x
            [] -> Nothing
          head' xs = case xs of
            x : _ -> x
            [] -> Point 0 0
          nodes = concat
            [ case inner of
                Just (c, size', items) ->
                  let l = p.x - z.w / 2
                      t = p.y - z.h / 2
                   in [ Rect {at = Point l t, size = z, radius = 10, fill = Just (Tint 0), edge = Just (pen Border 1.2)}
                      , Text {at = Point (l + 12) (t + 15), text = c.title, font = bold 12, color = Ink, anchor = Start}
                      , Path {start = Point l (t + 30), segments = [LineTo (Point (l + z.w) (t + 30))], line = pen Border 1, begin = NoHead, end = NoHead} ]
                        <> map (translate (p.x - size'.w / 2) (t + 38)) items
                Nothing -> shape p s z
            | (s, z, inner) <- children, Just p <- [Map.lookup s.name centers] ]
          notes = concat
            [ [ Rect {at = Point (p.x - z.w / 2) (p.y - z.h / 2), size = z, radius = 4, fill = Just (Tint 4), edge = Just (pen (Series 4) 1)}
              , Text {at = p, text = n.text, font = regular 12, color = Ink, anchor = Middle} ]
            | (i, n, _) <- noteSizes, Just p <- [Map.lookup (noteKey i) centers], Just z <- [Map.lookup (noteKey i) sizes] ]
          drawn = scene 0 (joins <> arrows <> nodes <> notes)
      pure (drawn.size, drawn.items)

shape :: Point -> State -> Size -> [Item]
shape c s z = case s.kind of
  Initial -> [Ellipse {center = c, rx = 7, ry = 7, fill = Just Ink, edge = Nothing}]
  Finish ->
    [ Ellipse {center = c, rx = 9, ry = 9, fill = Just Paper, edge = Just (pen Ink 1.3)}
    , Ellipse {center = c, rx = 5, ry = 5, fill = Just Ink, edge = Nothing} ]
  Fork -> [Rect {at = Point (c.x - z.w / 2) (c.y - z.h / 2), size = z, radius = 2, fill = Just Ink, edge = Nothing}]
  Choice -> [Polygon {points = [Point c.x (c.y - 15), Point (c.x + 15) c.y, Point c.x (c.y + 15), Point (c.x - 15) c.y], fill = Just Paper, edge = Just (pen Border 1.2)}]
  Plain ->
    [ Rect {at = Point (c.x - z.w / 2) (c.y - z.h / 2), size = z, radius = 10, fill = Just Paper, edge = Just (pen Border 1.2)} ]
      <> if null s.details
        then [Text {at = c, text = s.label, font = regular 13, color = Ink, anchor = Middle}]
        else
          [ Text {at = Point c.x (c.y - z.h / 2 + 15), text = s.label, font = regular 13, color = Ink, anchor = Middle}
          , Path {start = Point (c.x - z.w / 2) (c.y - z.h / 2 + 28), segments = [LineTo (Point (c.x + z.w / 2) (c.y - z.h / 2 + 28))], line = pen Border 1, begin = NoHead, end = NoHead}
          , Text {at = Point c.x (c.y - z.h / 2 + 28 + (z.h - 28) / 2), text = T.intercalate "\n" s.details, font = regular 11, color = Muted, anchor = Middle} ]
