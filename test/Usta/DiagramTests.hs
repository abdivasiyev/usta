{-# LANGUAGE OverloadedStrings #-}

-- | Every Mermaid diagram type usta draws: each example reads and lays
-- out, and no source text, however broken, makes the reader or the
-- layout fail.
module Usta.DiagramTests (tests, examples, measure) where

import Control.Exception (SomeException, evaluate, try)
import Data.Either (isRight)
import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Hedgehog (Property, annotate, annotateShow, assert, evalIO, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

import qualified Usta.Diagram as Diagram
import Usta.Diagram.Scene (Font (..), Item (..), Measure, Point (..), Scene (..), Segment (..), Size (..))

tests :: TestTree
tests =
  testGroup
    "Diagram"
    [ testGroup "every example reads and lays out" [testCase name (laysOut source) | (name, source) <- examples]
    , testCase "an empty diagram is refused" $ assertBool "drawn" (not (isRight (Diagram.parse "  \n%% only a comment\n")))
    , testCase "an unknown type is refused" $ assertBool "drawn" (not (isRight (Diagram.parse "nosuchDiagram\n  a")))
    , testProperty "broken sources never fail" (withTests 500 brokenSources)
    , testProperty "flowchart nodes never overlap" nodesApart
    ]

-- | The examples in test/diagrams.md, by their headings.
examples :: [(String, Text)]
examples = unsafePerformIO (fromMarkdown <$> T.readFile "test/diagrams.md")
  where
    fromMarkdown md =
      [ (T.unpack (T.strip name), T.unlines (takeWhile (/= "```") (drop 1 (dropWhile (/= "```mermaid") (T.lines body)))))
      | section <- drop 1 (T.splitOn "\n## " md)
      , let (name, body) = T.breakOn "\n" section
      ]
{-# NOINLINE examples #-}

-- | Text as wide as its characters, 0.6 of the font's size each.
measure :: Measure Identity
measure font text = Identity Size {w = 0.6 * font.size * fromIntegral (maximum (0 : map T.length (T.lines text))), h = 1.3 * font.size * fromIntegral (max 1 (length (T.lines text)))}

layout :: Text -> Either Text Scene
layout source = runIdentity . Diagram.draw Map.empty measure <$> Diagram.parse source

laysOut :: Text -> IO ()
laysOut source = case layout source of
  Left e -> assertFailure (T.unpack e)
  Right s -> do
    assertBool "nothing drawn" (not (null s.items))
    assertBool ("a size that is no number: " <> show s.size) (finite s.size.w && finite s.size.h && s.size.w > 0 && s.size.h > 0)
    assertBool "a point that is no number" (all (all finite . coordinates) s.items)

finite :: Double -> Bool
finite d = not (isNaN d || isInfinite d)

coordinates :: Item -> [Double]
coordinates item = case item of
  Rect {at = p, size = s, radius = r} -> [p.x, p.y, s.w, s.h, r]
  Polygon {points = ps} -> concatMap xy ps
  Ellipse {center = c, rx = a, ry = b} -> xy c <> [a, b]
  Wedge {center = c, r = a, from = f, to = t} -> xy c <> [a, f, t]
  Path {start = p, segments = ss} -> xy p <> concatMap segment ss
  Text {at = p} -> xy p
  Handle {at = p, size = s} -> xy p <> [s.w, s.h]
  where
    xy p = [p.x, p.y]
    segment s = case s of
      LineTo p -> xy p
      CurveTo a b c -> concatMap xy [a, b, c]

-- | An example with lines dropped, repeated, cut short or swapped: the
-- reader may refuse it, but neither it nor the layout may throw.
brokenSources :: Property
brokenSources = property $ do
  (name, source) <- forAll (Gen.element examples)
  annotate name
  let ls = T.lines source
  edits <- forAll (Gen.list (Range.linear 1 4) (Gen.int (Range.linear 0 (length ls - 1))))
  how <- forAll (Gen.element [0 :: Int .. 3])
  cut <- forAll (Gen.int (Range.linear 0 20))
  let broken = case how of
        0 -> [l | (i, l) <- zip [0 ..] ls, i `notElem` edits || i == 0]
        1 -> concat [if i `elem` edits then [l, l] else [l] | (i, l) <- zip [0 ..] ls]
        2 -> [if i `elem` edits && i > 0 then T.take cut l else l | (i, l) <- zip [0 ..] ls]
        _ -> take 1 ls <> reverse (drop 1 ls)
  result <- evalIO (try (evaluate (either T.length (length . show) (layout (T.unlines broken)))))
  case result of
    Left (e :: SomeException) -> annotate (show e) >> assert False
    Right _ -> pure ()

-- | Any flowchart: the boxes the reader may drag never cover each other.
nodesApart :: Property
nodesApart = property $ do
  n <- forAll (Gen.int (Range.linear 1 12))
  direction <- forAll (Gen.element ["TD", "LR", "BT", "RL"])
  edges <- forAll (Gen.list (Range.linear 0 20) ((,) <$> Gen.int (Range.linear 1 n) <*> Gen.int (Range.linear 1 n)))
  labels <- forAll (Gen.list (Range.singleton n) (Gen.text (Range.linear 1 14) (Gen.element ("abc xyz" :: String))))
  let node i = "n" <> T.pack (show i)
      source =
        T.unlines $
          ("flowchart " <> direction)
            : [ "  " <> node i <> "[" <> T.strip l <> "x]" | (i, l) <- zip [1 :: Int ..] labels]
            <> ["  " <> node a <> " --> " <> node b | (a, b) <- edges]
  case layout source of
    Left e -> annotate (T.unpack e) >> assert False
    Right s -> do
      let boxes = [(p, z) | Handle {at = p, size = z} <- s.items]
      annotateShow boxes
      length boxes === n
      assert (and [not (covers a b) | (i, a) <- zip [0 :: Int ..] boxes, (j, b) <- zip [0 ..] boxes, i < j])
  where
    covers (p, a) (q, b) = p.x < q.x + b.w - 0.5 && q.x < p.x + a.w - 0.5 && p.y < q.y + b.h - 0.5 && q.y < p.y + a.h - 0.5
