{- | usta's tests: unit, property, behaviour, contract and golden tests,
everything that runs without a window.
-}
module Main (main) where

import System.Environment (getEnv, setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (DependencyType (..), defaultMain, dependentTestGroup, testGroup)

import Usta.AccountsTests qualified
import Usta.CacheTests qualified
import Usta.Contract.ForgeTests qualified
import Usta.Contract.WritesTests qualified
import Usta.DiagramTests qualified
import Usta.GitMirrorTests qualified
import Usta.GitTests qualified
import Usta.GoldenTests qualified
import Usta.GraphTests qualified
import Usta.HandoffTests qualified
import Usta.LocalFilesTests qualified
import Usta.MermaidTests qualified
import Usta.MigrationTests qualified
import Usta.MirrorsTests qualified
import Usta.OAuthTests qualified
import Usta.PreferencesTests qualified
import Usta.SecurityTests qualified
import Usta.SyncTests qualified
import Usta.Types.DiffTests qualified
import Usta.Types.LinkTests qualified
import Usta.Types.ParseTests qualified
import Usta.Types.WorkflowTests qualified
import Usta.UI.Components.FileIconTests qualified
import Usta.UI.Components.MarkdownTests qualified
import Usta.UI.Components.NewRepositoryTests qualified
import Usta.UI.FormatTests qualified
import Usta.UI.HighlightTests qualified
import Usta.UI.Pages.PullRequest.ChecksTests qualified
import Usta.UpdatesTests qualified
import Usta.WaitingTests qualified

{- | The tests run with a home of their own: what usta keeps beside its
cache (drafts, viewed files, the log) never touches the real one.
-}
main :: IO ()
main = withSystemTempDirectory "usta-test" $ \home -> do
    -- Recording GitHub's answers runs gh, which needs the user's own.
    getEnv "HOME" >>= setEnv "USTA_REAL_HOME"
    setEnv "HOME" home
    setEnv "XDG_DATA_HOME" (home </> "data")
    setEnv "XDG_CONFIG_HOME" (home </> "config")
    setEnv "XDG_CONFIG_DIRS" (home </> "etc")
    defaultMain
        $ testGroup
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
            , -- One after the other: both use the file of what waits.
              dependentTestGroup "Beside the cache" AllFinish [Usta.LocalFilesTests.tests, Usta.WaitingTests.tests]
            , Usta.Contract.ForgeTests.tests
            , Usta.SecurityTests.tests
            , -- One after the other: each runs GLib's one main loop.
              dependentTestGroup "On GLib's loop" AllFinish [Usta.OAuthTests.tests, Usta.HandoffTests.tests]
            , Usta.AccountsTests.tests
            , Usta.MigrationTests.tests
            , Usta.GoldenTests.tests
            , Usta.GitMirrorTests.tests
            , Usta.MirrorsTests.tests
            , Usta.Contract.WritesTests.tests
            , Usta.UI.Components.MarkdownTests.tests
            ]
