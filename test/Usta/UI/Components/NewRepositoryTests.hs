{-# LANGUAGE OverloadedStrings #-}

-- | The names a new repository may have.
module Usta.UI.Components.NewRepositoryTests (tests) where

import Data.Either (isLeft)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Usta.UI.Components.NewRepository (nameOf)

tests :: TestTree
tests =
  testGroup
    "NewRepository"
    [ testCase "a plain name" $ nameOf "made-by-usta" @?= Right "made-by-usta"
    , testCase "spaces around it" $ nameOf "  usta.nix_2 " @?= Right "usta.nix_2"
    , testCase "empty" $ nameOf "   " @?= Left "Give it a name."
    , testCase "git's own names" $ (nameOf ".", nameOf "..") @?= (Left "That name is taken by git.", Left "That name is taken by git.")
    , testCase "a slash" $ assertBool "accepted" (isLeft (nameOf "a/b"))
    , testCase "a space inside" $ assertBool "accepted" (isLeft (nameOf "a b"))
    , testCase "letters that are not ASCII" $ assertBool "accepted" (isLeft (nameOf "café"))
    ]
