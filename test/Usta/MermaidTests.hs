{-# LANGUAGE OverloadedStrings #-}

-- | Reading flowcharts: nodes, their shapes, and the links between them.
module Usta.MermaidTests (tests) where

import Data.Either (isLeft)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Usta.Mermaid (Direction (..), Edge (..), End (..), Flowchart (..), Group (..), Node (..), Shape (..), Stroke (..), chain, node, parse)

tests :: TestTree
tests =
    testGroup
        "Mermaid"
        [ testGroup
            "node"
            [ shape "A[Box]" Rectangle "Box"
            , shape "A(Round)" Rounded "Round"
            , shape "A([Stadium])" Stadium "Stadium"
            , shape "A[[Sub]]" Subroutine "Sub"
            , shape "A[(Store)]" Database "Store"
            , shape "A((Circle))" Circle "Circle"
            , shape "A>Flag]" Flag "Flag"
            , shape "A{Ask?}" Diamond "Ask?"
            , shape "A{{Hex}}" Hexagon "Hex"
            , shape "A[\"Quoted [text]\"]" Rectangle "Quoted [text"
            , shape "A[one<br>two]" Rectangle "one\ntwo"
            , testCase "a bare id" $ node "A --> B" @?= Right (Node "A" "A" Rectangle, " --> B")
            , testCase "an open shape" $ assertBool "read" (isLeft (node "A[never closed"))
            , testCase "no id" $ assertBool "read" (isLeft (node "[x]"))
            ]
        , testGroup
            "links"
            [ link "A --> B" (Edge "A" "B" Nothing Solid Open Pointed)
            , link "A --- B" (Edge "A" "B" Nothing Solid Open Open)
            , link "A -.-> B" (Edge "A" "B" Nothing Dotted Open Pointed)
            , link "A ==> B" (Edge "A" "B" Nothing Thick Open Pointed)
            , link "A -->|yes| B" (Edge "A" "B" (Just "yes") Solid Open Pointed)
            , link "A -- yes --> B" (Edge "A" "B" (Just "yes") Solid Open Pointed)
            , link "A == big ==> B" (Edge "A" "B" (Just "big") Thick Open Pointed)
            , link "A -. maybe .-> B" (Edge "A" "B" (Just "maybe") Dotted Open Pointed)
            , link "A -->|\"quoted\"| B" (Edge "A" "B" (Just "quoted") Solid Open Pointed)
            , link "A <--> B" (Edge "A" "B" Nothing Solid Pointed Pointed)
            , link "A --o B" (Edge "A" "B" Nothing Solid Open Round)
            , link "A --x B" (Edge "A" "B" Nothing Solid Open Crossed)
            , link "A o--o B" (Edge "A" "B" Nothing Solid Round Round)
            , link "A x==x B" (Edge "A" "B" Nothing Thick Crossed Crossed)
            , link "A <-.-> B" (Edge "A" "B" Nothing Dotted Pointed Pointed)
            , link "A <-- both --> B" (Edge "A" "B" (Just "both") Solid Pointed Pointed)
            , testCase "a chain"
                $ fmap snd (chain "A --> B --> C") @?= Right [Edge "A" "B" Nothing Solid Open Pointed, Edge "B" "C" Nothing Solid Open Pointed]
            , testCase "nodes joined by &"
                $ fmap snd (chain "A & B --> C") @?= Right [Edge "A" "C" Nothing Solid Open Pointed, Edge "B" "C" Nothing Solid Open Pointed]
            , testCase "a link's text not closed" $ assertBool "read" (isLeft (chain "A -- never"))
            ]
        , testGroup
            "parse"
            [ testCase "directions"
                $ map (fmap (.direction) . parse) ["graph TD\nA", "flowchart LR\nA", "graph BT\nA", "graph RL\nA", "graph\nA"]
                    @?= map Right [TopDown, LeftRight, BottomUp, RightLeft, TopDown]
            , testCase "a node keeps its shape where it is only named later"
                $ fmap (.nodes) (parse "graph TD\nA{Ask}\nA --> B")
                    @?= Right [Node "A" "Ask" Diamond, Node "B" "B" Rectangle]
            , testCase "statements split by ;" $ fmap (length . (.edges)) (parse "graph TD; A-->B; B-->C") @?= Right 2
            , testCase "comments and styles are skipped"
                $ fmap (length . (.nodes)) (parse "graph TD\n%% a comment\nA --> B %% and here\nstyle A fill:#f00\nclassDef x fill:#0f0\nclass A x")
                    @?= Right 2
            , testCase "subgraphs"
                $ fmap (.groups) (parse "graph TD\nsubgraph one [First]\nA --> B\nend\nsubgraph two\nB --> C\nend")
                    @?= Right [Group "First" ["A", "B"], Group "two" ["C"]]
            , testCase "an empty diagram" $ assertBool "read" (isLeft (parse "\n  \n"))
            , testCase "another type" $ assertBool "read" (isLeft (parse "pie\n\"a\": 1"))
            ]
        ]
    where
        shape text s label = testCase (show text) (fmap fst (node text) @?= Right (Node "A" label s))
        link text edge = testCase (show text) (fmap snd (chain text) @?= Right [edge])
