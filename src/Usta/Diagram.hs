{-# LANGUAGE OverloadedStrings #-}

-- | Every Mermaid diagram type usta draws, by the word that starts it.
-- Front matter (a title between "---" lines) and @%%{init}%%@ directives
-- before it are read past; the title is drawn above the diagram. Each
-- type has its own module that reads and lays it out.
module Usta.Diagram
  ( Diagram (..)
  , Kind (..)
  , parse
  , draw
  ) where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Usta.Diagram.Class as Class
import qualified Usta.Diagram.Entity as Entity
import qualified Usta.Diagram.Flowchart as Flowchart
import qualified Usta.Diagram.Gantt as Gantt
import qualified Usta.Diagram.GitGraph as GitGraph
import qualified Usta.Diagram.Journey as Journey
import qualified Usta.Diagram.Kanban as Kanban
import qualified Usta.Diagram.Mindmap as Mindmap
import qualified Usta.Diagram.Packet as Packet
import qualified Usta.Diagram.Pie as Pie
import qualified Usta.Diagram.Quadrant as Quadrant
import qualified Usta.Diagram.Radar as Radar
import qualified Usta.Diagram.Requirement as Requirement
import qualified Usta.Diagram.Sankey as Sankey
import qualified Usta.Diagram.Sequence as Sequence
import qualified Usta.Diagram.State as State
import qualified Usta.Diagram.Timeline as Timeline
import qualified Usta.Diagram.Treemap as Treemap
import qualified Usta.Diagram.XyChart as XyChart
import Usta.Diagram.Scene
import qualified Usta.Mermaid as Mermaid

type Kind :: Type
data Kind
  = Flowchart Mermaid.Flowchart
  | Sequence Sequence.Sequence
  | Class Class.Classes
  | State State.States
  | Entity Entity.Entities
  | Pie Pie.Pie
  | Quadrant Quadrant.Quadrant
  | Gantt Gantt.Gantt
  | Journey Journey.Journey
  | Timeline Timeline.Timeline
  | XyChart XyChart.XyChart
  | Radar Radar.Radar
  | Mindmap Mindmap.Mindmap
  | GitGraph GitGraph.GitGraph
  | Sankey Sankey.Sankey
  | Kanban Kanban.Kanban
  | Packet Packet.Packet
  | Treemap Treemap.Treemap
  | Requirement Requirement.Requirements

type Diagram :: Type
data Diagram = Diagram
  { title :: Maybe Text
  , kind :: Kind
  }

-- | The diagram, or words for the user on why usta does not draw it.
parse :: Text -> Either Text Diagram
parse source = do
  let (title, body) = frontMatter (T.lines source)
      meaningful = filter (not . skippable) body
  case meaningful of
    [] -> Left "The diagram is empty."
    header : _ -> do
      let word = T.toLower (T.takeWhile (\c -> c /= ' ' && c /= ';') (T.strip header))
      kind <- case word of
        w | w `elem` ["graph", "flowchart"] -> Flowchart <$> Mermaid.parse (T.unlines meaningful)
        "sequencediagram" -> Sequence <$> Sequence.parse (T.unlines meaningful)
        w | w `elem` ["classdiagram", "classdiagram-v2"] -> Class <$> Class.parse (T.unlines meaningful)
        w | w `elem` ["statediagram", "statediagram-v2"] -> State <$> State.parse (T.unlines meaningful)
        "erdiagram" -> Entity <$> Entity.parse (T.unlines meaningful)
        "pie" -> Pie <$> Pie.parse (T.unlines meaningful)
        "quadrantchart" -> Quadrant <$> Quadrant.parse (T.unlines meaningful)
        "gantt" -> Gantt <$> Gantt.parse (T.unlines meaningful)
        "journey" -> Journey <$> Journey.parse (T.unlines meaningful)
        "timeline" -> Timeline <$> Timeline.parse (T.unlines meaningful)
        w | w `elem` ["xychart", "xychart-beta"] -> XyChart <$> XyChart.parse (T.unlines meaningful)
        w | w `elem` ["radar", "radar-beta"] -> Radar <$> Radar.parse (T.unlines meaningful)
        "mindmap" -> Mindmap <$> Mindmap.parse (T.unlines meaningful)
        "gitgraph" -> GitGraph <$> GitGraph.parse (T.unlines meaningful)
        w | w `elem` ["sankey", "sankey-beta"] -> Sankey <$> Sankey.parse (T.unlines meaningful)
        "kanban" -> Kanban <$> Kanban.parse (T.unlines meaningful)
        w | w `elem` ["packet", "packet-beta"] -> Packet <$> Packet.parse (T.unlines meaningful)
        w | w `elem` ["treemap", "treemap-beta"] -> Treemap <$> Treemap.parse (T.unlines meaningful)
        "requirementdiagram" -> Requirement <$> Requirement.parse (T.unlines meaningful)
        w -> Left ("usta does not draw " <> w <> " diagrams yet; it shows as text.")
      pure Diagram {title = title, kind = kind}
  where
    skippable line = let l = T.strip line in T.null l || "%%" `T.isPrefixOf` l

-- | A title from front matter, and the lines after it.
frontMatter :: [Text] -> (Maybe Text, [Text])
frontMatter ls = case dropWhile (T.null . T.strip) ls of
  first : rest | T.strip first == "---" ->
    let (meta, after) = break ((== "---") . T.strip) rest
        title = case [T.strip (T.drop 6 (T.strip l)) | l <- meta, "title:" `T.isPrefixOf` T.strip l] of
          t : _ -> Just (unquote t)
          [] -> Nothing
     in (title, drop 1 after)
  _ -> (Nothing, ls)
  where
    unquote t = T.dropAround (`elem` ("\"'" :: String)) t

draw :: Monad m => Measure m -> Diagram -> m Scene
draw measure d = do
  body <- case d.kind of
    Flowchart chart -> Flowchart.draw measure chart
    Sequence s -> Sequence.draw measure s
    Class c -> Class.draw measure c
    State s -> State.draw measure s
    Entity e -> Entity.draw measure e
    Pie p -> Pie.draw measure p
    Quadrant q -> Quadrant.draw measure q
    Gantt g -> Gantt.draw measure g
    Journey j -> Journey.draw measure j
    Timeline t -> Timeline.draw measure t
    XyChart x -> XyChart.draw measure x
    Radar r -> Radar.draw measure r
    Mindmap m -> Mindmap.draw measure m
    GitGraph g -> GitGraph.draw measure g
    Sankey s -> Sankey.draw measure s
    Kanban k -> Kanban.draw measure k
    Packet p -> Packet.draw measure p
    Treemap t -> Treemap.draw measure t
    Requirement r -> Requirement.draw measure r
  pure $ case d.title of
    Nothing -> body
    -- The title above, centred over the diagram.
    Just t ->
      Scene
        { size = Size {w = body.size.w, h = body.size.h + 28}
        , items = Text {at = Point (body.size.w / 2) 16, text = t, font = bold 14, color = Ink, anchor = Middle}
            : map (translate 0 28) body.items
        }
