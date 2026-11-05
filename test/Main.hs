-- | usta's tests: unit, property, behaviour, contract and golden tests,
-- everything that runs without a window.
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Usta.Types.ParseTests
import qualified Usta.Types.LinkTests
import qualified Usta.UI.FormatTests

main :: IO ()
main =
  defaultMain $
    testGroup
      "usta"
      [ Usta.Types.ParseTests.tests
      , Usta.Types.LinkTests.tests
      , Usta.UI.FormatTests.tests
      ]
