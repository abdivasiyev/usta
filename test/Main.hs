-- | usta's tests: unit, property, behaviour, contract and golden tests,
-- everything that runs without a window.
module Main (main) where

import System.Environment (getEnv, setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
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
import qualified Usta.UI.HighlightTests
import qualified Usta.SyncTests
import qualified Usta.CacheTests
import qualified Usta.LocalFilesTests
import qualified Usta.Contract.ForgeTests
import qualified Usta.SecurityTests
import qualified Usta.OAuthTests
import qualified Usta.AccountsTests

-- | The tests run with a home of their own: what usta keeps beside its
-- cache (drafts, viewed files, the log) never touches the real one.
main :: IO ()
main = withSystemTempDirectory "usta-test" $ \home -> do
  -- Recording GitHub's answers runs gh, which needs the user's own.
  getEnv "HOME" >>= setEnv "USTA_REAL_HOME"
  setEnv "HOME" home
  setEnv "XDG_DATA_HOME" (home </> "data")
  setEnv "XDG_CONFIG_HOME" (home </> "config")
  setEnv "XDG_CONFIG_DIRS" (home </> "etc")
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
      , Usta.UI.HighlightTests.tests
      , Usta.SyncTests.tests
      , Usta.CacheTests.tests
      , Usta.LocalFilesTests.tests
      , Usta.Contract.ForgeTests.tests
      , Usta.SecurityTests.tests
      , Usta.OAuthTests.tests
      , Usta.AccountsTests.tests
      ]
