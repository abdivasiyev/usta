{-# LANGUAGE OverloadedStrings #-}

-- | The commit graph's lanes.
module Usta.GraphTests (tests) where

import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), fromGregorian)
import Hedgehog (Gen, Property, annotateShow, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Usta.Graph (Row (..), rows)
import Usta.Types.Insights (Node (..))

tests :: TestTree
tests =
  testGroup
    "Graph"
    [ testCase "a straight line" $
        map (.column) (rows [commit "c" ["b"], commit "b" ["a"], commit "a" []]) @?= [0, 0, 0]
    , testCase "a merge opens a lane and closes it" $
        map (\r -> (r.column, r.isMerge)) (rows [commit "m" ["a", "f"], commit "f" ["a"], commit "a" []])
          @?= [(0, True), (1, False), (0, False)]
    , testCase "the branch joins its parent's lane below it" $
        (rows [commit "m" ["a", "f"], commit "f" ["a"], commit "a" []] !! 1).below @?= [(0, 0), (1, 0)]
    , testCase "no commits" $ rows [] @?= []
    , testProperty "lanes carry on from row to row" lanesConnect
    , testProperty "every line stays inside its row" insideRows
    ]

commit :: Text -> [Text] -> Node
commit sha parents = Node {sha = sha, parents = parents, author = "a", at = UTCTime (fromGregorian 2026 1 1) 0, subject = "", refs = []}

-- | A history, newest first: each commit's parents come later in it, and
-- some fall outside it, as in a page of history.
history :: Gen [Node]
history = do
  n <- Gen.int (Range.linear 1 30)
  let name i = "c" <> T.pack (show (i :: Int))
  traverse
    ( \i -> do
        count <- Gen.frequency [(1, pure 0), (6, pure 1), (2, pure 2), (1, pure 3)]
        parents <- Gen.list (Range.singleton count) (Gen.int (Range.linear (i + 1) (n + 3)))
        pure (commit (name i) (map name (nub parents)))
    )
    [0 .. n - 1]

-- | Where one row's lines leave its bottom, the next row's start at its top.
lanesConnect :: Property
lanesConnect = property $ do
  nodes <- forAll history
  let rs = rows nodes
  annotateShow rs
  sequence_
    [ sort (nub (map snd upper.below)) === sort (nub (map fst lower.above))
    | (upper, lower) <- zip rs (drop 1 rs)
    ]

insideRows :: Property
insideRows = property $ do
  nodes <- forAll history
  let rs = rows nodes
  length rs === length nodes
  assert $
    and
      [ r.column < r.width && all (\(a, b) -> a >= 0 && b >= 0 && a < r.width && b < r.width) (r.above <> r.below)
      | r <- rs
      ]
