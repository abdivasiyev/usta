{-# LANGUAGE OverloadedStrings #-}

-- | Mermaid diagrams in Markdown, read by usta itself; "Usta.Diagram"
-- lays them out and the window draws them. Flowcharts (@graph@ and
-- @flowchart@) are read here; a type usta does not read yet is refused
-- with words for the user, who sees the source.
module Usta.Mermaid
  ( Direction (..)
  , Shape (..)
  , Node (..)
  , Stroke (..)
  , Edge (..)
  , Group (..)
  , Flowchart (..)
  , parse
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Kind (Type)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

type Direction :: Type
data Direction = TopDown | BottomUp | LeftRight | RightLeft
  deriving stock (Eq, Show)

type Shape :: Type
data Shape = Rectangle | Rounded | Stadium | Diamond | Hexagon | Circle | Database | Flag | Subroutine
  deriving stock (Eq, Show)

type Node :: Type
data Node = Node
  { name :: Text
    -- ^ The id the edges use.
  , label :: Text
  , shape :: Shape
  }
  deriving stock (Eq, Show)

type Stroke :: Type
data Stroke = Solid | Dotted | Thick
  deriving stock (Eq, Show)

type Edge :: Type
data Edge = Edge
  { from :: Text
  , to :: Text
  , label :: Maybe Text
  , stroke :: Stroke
  , arrow :: Bool
  }
  deriving stock (Eq, Show)

-- | A subgraph: a titled frame around the nodes first named inside it.
type Group :: Type
data Group = Group
  { title :: Text
  , members :: [Text]
  }
  deriving stock (Eq, Show)

type Flowchart :: Type
data Flowchart = Flowchart
  { direction :: Direction
  , nodes :: [Node]
    -- ^ In the order they first appear; a node only on an edge is a
    -- rectangle named by its id.
  , edges :: [Edge]
  , groups :: [Group]
  }
  deriving stock (Eq, Show)

-- | A flowchart, or why usta does not draw this diagram.
parse :: Text -> Either Text Flowchart
parse source = case filter (not . T.null) (map clean (T.lines source)) of
  [] -> Left "The diagram is empty."
  header : rest -> do
    direction <- headerOf header
    let lines' = concatMap (map T.strip . T.splitOn ";") rest
    statements <- traverse statement lines'
    let defined = concat [ns | (ns, _) <- statements]
        edges = concat [es | (_, es) <- statements]
        named = nub (map (.name) defined <> concat [[e.from, e.to] | e <- edges])
        nodeFor n = case [x | x <- reverse defined, x.name == n, x.label /= n || x.shape /= Rectangle] of
          x : _ -> x
          [] -> Node {name = n, label = n, shape = Rectangle}
    pure Flowchart
      { direction = direction
      , nodes = map nodeFor named
      , edges = edges
      , groups = grouped (zip lines' statements)
      }
  where
    clean line = T.strip (fst (T.breakOn "%%" line))

-- | Each subgraph with the nodes named between its "subgraph" and "end"
-- lines and not already in an earlier one.
grouped :: [(Text, ([Node], [Edge]))] -> [Group]
grouped = go [] [] []
  where
    go done open seen items = case items of
      [] -> reverse done <> reverse [Group t (reverse ms) | (t, ms) <- open]
      (line, (ns, es)) : rest
        | "subgraph" `T.isPrefixOf` line -> go done ((titleOf (T.strip (T.drop 8 line)), []) : open) seen rest
        | line == "end" -> case open of
            (t, ms) : outer -> go (Group t (reverse ms) : done) outer seen rest
            [] -> go done open seen rest
        | otherwise -> case open of
            (t, ms) : outer ->
              let fresh = nub [n | n <- map (.name) ns <> concat [[e.from, e.to] | e <- es], n `notElem` seen, n `notElem` ms]
               in go done ((t, reverse fresh <> ms) : outer) (seen <> fresh) rest
            [] -> go done open (seen <> map (.name) ns <> concat [[e.from, e.to] | e <- es]) rest
    -- "id [Title]", "id[\"Title\"]" or just "Title".
    titleOf text = case T.breakOn "[" text of
      (_, bracket) | not (T.null bracket) -> T.dropAround (`elem` ("[]\" " :: String)) bracket
      _ -> text

headerOf :: Text -> Either Text Direction
headerOf header = case T.words header of
  (kind : rest) | kind `elem` ["graph", "flowchart"] -> Right $ case rest of
    ("LR" : _) -> LeftRight
    ("RL" : _) -> RightLeft
    ("BT" : _) -> BottomUp
    _ -> TopDown
  (kind : _) -> Left ("usta draws flowcharts; a " <> kind <> " diagram shows as text.")
  [] -> Left "The diagram has no type."

-- | One line: nodes, and the edges between them in a chain such as
-- @A --> B -->|yes| C & D@. Lines that only style or group are skipped.
statement :: Text -> Either Text ([Node], [Edge])
statement line
  | T.null line || any (`T.isPrefixOf` line) skipped = Right ([], [])
  | otherwise = chain line
  where
    skipped = ["classDef ", "class ", "style ", "linkStyle ", "click ", "subgraph", "end", "direction "]

chain :: Text -> Either Text ([Node], [Edge])
chain text = do
  (first, rest) <- group text
  go first rest
  where
    go previous rest
      | T.null (T.strip rest) = Right (previous, [])
      | otherwise = do
          (link, after) <- connector (T.stripStart rest)
          (next, more) <- group (T.stripStart after)
          (ns, es) <- go next more
          let made = [Edge {from = a.name, to = b.name, label = link.label, stroke = link.stroke, arrow = link.arrow} | a <- previous, b <- next]
          pure (previous <> ns, made <> es)

-- | Nodes joined by "&".
group :: Text -> Either Text ([Node], Text)
group text = do
  (n, rest) <- node text
  case T.uncons (T.stripStart rest) of
    Just ('&', more) -> do
      (ns, after) <- group (T.stripStart more)
      pure (n : ns, after)
    _ -> pure ([n], rest)

node :: Text -> Either Text (Node, Text)
node text
  | T.null id' = Left ("usta could not read \"" <> T.take 40 text <> "\".")
  | otherwise = case firstShape of
      Just (shape, close, inside) -> case T.breakOn close inside of
        (body, after) | not (T.null after) -> Right (Node id' (labelText body) shape, T.drop (T.length close) after)
        _ -> Left ("A node's text is not closed: \"" <> T.take 40 text <> "\".")
      Nothing -> Right (Node id' id' Rectangle, rest)
  where
    (id', rest) = T.span (\c -> isAlphaNum c || c == '_') text
    firstShape = case [(s, c, inside) | (o, c, s) <- shapes, Just inside <- [T.stripPrefix o rest]] of
      x : _ -> Just x
      [] -> Nothing
    -- Longest openings first, so "((" is not read as "(".
    shapes =
      [ ("((", "))", Circle), ("([", "])", Stadium), ("[(", ")]", Database), ("[[", "]]", Subroutine)
      , ("{{", "}}", Hexagon), ("[", "]", Rectangle), ("(", ")", Rounded), ("{", "}", Diamond), (">", "]", Flag)
      ]
    labelText = T.replace "<br>" "\n" . T.replace "<br/>" "\n" . unquote . T.strip
    unquote t = maybe t (T.dropWhileEnd (== '"')) (T.stripPrefix "\"" t)

type Link :: Type
data Link = Link {label :: Maybe Text, stroke :: Stroke, arrow :: Bool}

-- | "-->", "---", "-.->", "==>", with "|text|" after or "-- text -->".
connector :: Text -> Either Text (Link, Text)
connector text
  | any (`T.isPrefixOf` text) ["--", "==", "-."] =
      let (symbols, rest) = T.span (`elem` ("-.=>ox" :: String)) text
       in if symbols `elem` ["--", "=="] && maybe False (isSpace . fst) (T.uncons rest)
            then textForm symbols rest
            else Right (withPipe (linkOf symbols) (T.stripStart rest))
  | otherwise = Left ("usta could not read the link in \"" <> T.take 40 text <> "\".")
  where
    linkOf symbols =
      Link { label = Nothing
           , stroke = if T.any (== '.') symbols then Dotted else if T.any (== '=') symbols then Thick else Solid
           , arrow = ">" `T.isSuffixOf` symbols
           }
    withPipe link rest = case T.uncons rest of
      Just ('|', more) -> let (l, after) = T.breakOn "|" more in (labelled (T.strip l) link, T.drop 1 after)
      _ -> (link, rest)
    -- "A -- text --> B": the text runs to the rest of the connector.
    textForm symbols rest =
      let closers = [c | c <- ["-->", "---", "==>", "==="], c `T.isInfixOf` rest]
       in case closers of
            c : _ -> let (l, after) = T.breakOn c rest
                      in Right (labelled (T.strip l) (linkOf (symbols <> T.drop 2 c)), T.drop (T.length c) after)
            [] -> Left "A link's text is not closed."

labelled :: Text -> Link -> Link
labelled l link = Link {label = Just l, stroke = link.stroke, arrow = link.arrow}
