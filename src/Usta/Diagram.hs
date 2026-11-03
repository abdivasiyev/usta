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
import qualified Usta.Diagram.Sequence as Sequence
import qualified Usta.Diagram.State as State
import Usta.Diagram.Scene
import qualified Usta.Mermaid as Mermaid

type Kind :: Type
data Kind
  = Flowchart Mermaid.Flowchart
  | Sequence Sequence.Sequence
  | Class Class.Classes
  | State State.States
  | Entity Entity.Entities

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
  pure $ case d.title of
    Nothing -> body
    -- The title above, centred over the diagram.
    Just t ->
      Scene
        { size = Size {w = body.size.w, h = body.size.h + 28}
        , items = Text {at = Point (body.size.w / 2) 16, text = t, font = bold 14, color = Ink, anchor = Middle}
            : map (translate 0 28) body.items
        }
