-- | usta's tests: unit, property, behaviour, contract and golden tests,
-- everything that runs without a window.
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Usta.Types.ParseTests

main :: IO ()
main =
  defaultMain $
    testGroup
      "usta"
      [ Usta.Types.ParseTests.tests
      ]
