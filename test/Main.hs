-- | usta's tests: unit, property, behaviour, contract and golden tests,
-- everything that runs without a window.
module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Usta.Types.ParseTests
import qualified Usta.Types.LinkTests
import qualified Usta.UI.FormatTests
import qualified Usta.UpdatesTests
import qualified Usta.Types.DiffTests
import qualified Usta.Types.WorkflowTests
import qualified Usta.GraphTests
import qualified Usta.UI.Pages.PullRequest.ChecksTests
import qualified Usta.UI.Components.FileIconTests
import qualified Usta.PreferencesTests
import qualified Usta.GitTests
import qualified Usta.UI.Components.NewRepositoryTests
import qualified Usta.DiagramTests
import qualified Usta.MermaidTests

main :: IO ()
main =
  defaultMain $
    testGroup
      "usta"
      [ Usta.Types.ParseTests.tests
      , Usta.Types.LinkTests.tests
      , Usta.UI.FormatTests.tests
      , Usta.UpdatesTests.tests
      , Usta.Types.DiffTests.tests
      , Usta.Types.WorkflowTests.tests
      , Usta.GraphTests.tests
      , Usta.UI.Pages.PullRequest.ChecksTests.tests
      , Usta.UI.Components.FileIconTests.tests
      , Usta.PreferencesTests.tests
      , Usta.GitTests.tests
      , Usta.UI.Components.NewRepositoryTests.tests
      , Usta.DiagramTests.tests
      , Usta.MermaidTests.tests
      ]
