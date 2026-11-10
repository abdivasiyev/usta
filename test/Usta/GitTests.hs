{-# LANGUAGE OverloadedStrings #-}

-- | What usta reads out of git itself.
module Usta.GitTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Usta.Git (noreplyLogin)

tests :: TestTree
tests =
    testGroup
        "Git"
        [ testGroup
            "noreplyLogin"
            [ testCase "GitHub's with an id" $ noreplyLogin "12345+octocat@users.noreply.github.com" @?= Just "octocat"
            , testCase "GitHub's without" $ noreplyLogin "octocat@users.noreply.github.com" @?= Just "octocat"
            , testCase "in capitals" $ noreplyLogin "OctoCat@Users.NoReply.GitHub.com" @?= Just "octocat"
            , testCase "Forgejo's" $ noreplyLogin "tester@noreply.localhost" @?= Just "tester"
            , testCase "a real address" $ noreplyLogin "someone@example.com" @?= Nothing
            , testCase "no local part" $ noreplyLogin "@noreply.localhost" @?= Nothing
            , testCase "no address" $ noreplyLogin "" @?= Nothing
            ]
        ]
