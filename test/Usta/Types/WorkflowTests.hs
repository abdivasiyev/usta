{-# LANGUAGE OverloadedStrings #-}

-- | Reading which workflows can be started by hand, and what they ask.
module Usta.Types.WorkflowTests (tests) where

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.ByteString.Char8 qualified as B

import Usta.Types.Workflow (Input (..), Kind (..), inputsOf)

tests :: TestTree
tests =
    testGroup
        "Workflow"
        [ testCase "no workflow_dispatch" $ inputsOf "on: push\njobs: {}\n" @?= Right Nothing
        , testCase "a list of events" $ inputsOf "on: [push, workflow_dispatch]\n" @?= Right (Just [])
        , testCase "workflow_dispatch alone" $ inputsOf "on: workflow_dispatch\n" @?= Right (Just [])
        , testCase "workflow_dispatch without inputs" $ inputsOf "on:\n  workflow_dispatch:\n  push:\n" @?= Right (Just [])
        , testCase "events without it" $ inputsOf "on:\n  push:\n  pull_request:\n" @?= Right Nothing
        , testCase "every kind of input, in the file's order"
            $ inputsOf deploy
                @?= Right
                    ( Just
                        [ Input "version" "What to ship" TextInput True Nothing
                        , Input "dry" "" BooleanInput False (Just "true")
                        , Input "level" "" (ChoiceInput ["debug", "info", "warn"]) False (Just "info")
                        , Input "count" "" NumberInput False (Just "3")
                        , Input "target" "" EnvironmentInput False Nothing
                        ]
                    )
        , testCase "required as a string"
            $ fmap (fmap (map (.required))) (inputsOf "on:\n  workflow_dispatch:\n    inputs:\n      a:\n        required: 'true'\n")
                @?= Right (Just [True])
        , testCase "an input that is only a name"
            $ inputsOf "on:\n  workflow_dispatch:\n    inputs:\n      a:\n"
                @?= Right (Just [Input "a" "" TextInput False Nothing])
        , testCase "not YAML" $ assertBool "read" (isLeft (inputsOf "on: [push\n"))
        , testCase "not a workflow" $ assertBool "read" (isLeft (inputsOf "- a\n- b\n"))
        ]

deploy :: ByteString
deploy =
    B.unlines
        [ "name: Deploy"
        , "on:"
        , "  workflow_dispatch:"
        , "    inputs:"
        , "      version:"
        , "        description: What to ship"
        , "        required: true"
        , "      dry:"
        , "        type: boolean"
        , "        default: true"
        , "      level:"
        , "        type: choice"
        , "        options: [debug, info, warn]"
        , "        default: info"
        , "      count:"
        , "        type: number"
        , "        default: 3"
        , "      target:"
        , "        type: environment"
        ]
