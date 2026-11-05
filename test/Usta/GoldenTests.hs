{-# LANGUAGE OverloadedStrings #-}

-- | Output kept as it was agreed: every diagram's layout, and syntax
-- colors. A change shows as a diff of test/golden; when it is meant,
-- @cabal test --test-options=--accept@ keeps the new one.
module Usta.GoldenTests (tests) where

import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Numeric (showFFloat)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import qualified Usta.Diagram as Diagram
import Usta.Diagram.Scene (Item (..), Point (..), Scene (..), Segment (..), Size (..))
import Usta.DiagramTests (examples, measure)
import Usta.UI.Highlight (Palette (..), languageNamed, markup)

tests :: TestTree
tests =
  testGroup
    "Golden"
    [ goldenVsString "every diagram's layout" "test/golden/diagrams.txt" (pure (text (T.unlines (map layout examples))))
    , goldenVsString "syntax colors" "test/golden/highlight.txt" (pure (text highlighted))
    ]

text :: Text -> BL.ByteString
text = BL.fromStrict . encodeUtf8

layout :: (String, Text) -> Text
layout (name, source) =
  "## " <> T.pack name <> "\n" <> case Diagram.parse source of
    Left e -> "refused: " <> e <> "\n"
    Right d ->
      let s = runIdentity (Diagram.draw Map.empty measure d)
       in "size " <> size s.size <> "\n" <> T.unlines (map item s.items)

-- | One line an item, numbers to a tenth: enough to see a move.
item :: Item -> Text
item i = case i of
  Rect {at, size = z, radius} -> "rect " <> point at <> " " <> size z <> " r" <> num radius
  Polygon {points} -> "polygon " <> T.unwords (map point points)
  Ellipse {center, rx, ry} -> "ellipse " <> point center <> " " <> num rx <> "x" <> num ry
  Wedge {center, r, from, to} -> "wedge " <> point center <> " r" <> num r <> " " <> num from <> ".." <> num to
  Path {start, segments} -> "path " <> point start <> T.concat (map segment segments)
  Text {at, text = t} -> "text " <> point at <> " " <> T.pack (show t)
  Handle {key, at, size = z} -> "node " <> key <> " " <> point at <> " " <> size z
  where
    segment s = case s of
      LineTo p -> " L" <> point p
      CurveTo a b c -> " C" <> point a <> " " <> point b <> " " <> point c

point :: Point -> Text
point p = num p.x <> "," <> num p.y

size :: Size -> Text
size z = num z.w <> "x" <> num z.h

num :: Double -> Text
num d = T.pack (showFFloat (Just 1) d "")

highlighted :: Text
highlighted =
  T.unlines
    [ "## " <> name <> " " <> T.pack (show palette) <> "\n" <> T.unlines (markup palette (languageNamed name) code)
    | (name, code) <- samples
    , palette <- [LightPalette, DarkPalette]
    ]
  where
    samples =
      [ ("haskell", "module Main (main) where\n\n-- | Says hello.\nmain :: IO ()\nmain = putStrLn \"hello\" >> print (1 + 2.5 :: Double)")
      , ("nix", "{ pkgs, ... }: {\n  # The app.\n  home.packages = [ pkgs.usta ];\n  programs.usta.settings.theme = \"Dark\";\n}")
      , ("yaml", "on:\n  push:\n    tags: [\"v*\"]\njobs:\n  release: # a comment\n    runs-on: ubuntu-24.04")
      , ("diff", "@@ -1,2 +1,2 @@\n-old line\n+new line\n context")
      ]
