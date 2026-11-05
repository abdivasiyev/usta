{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | usta's window under test: its pages built from the sample data,
-- drawn in light and dark, and driven with the keys and clicks a person
-- would use. Needs a display; screenshots go to dist-newstyle/ui-shots.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (catch)
import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import System.Environment (setEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (DependencyType (..), TestTree, defaultMain, dependentTestGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Harness
import Usta.Preferences (Theme (..))
import qualified Usta.Preferences as Preferences
import qualified Usta.Types.Platform as Platform
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import qualified Usta.UI.Platform as UIPlatform
import Usta.UI.Sidebar (Page (..))
import qualified Usta.UI.Source as Source
import qualified Usta.UI.TextKeys as TextKeys

main :: IO ()
main = withSystemTempDirectory "usta-ui-test" $ \home -> do
  setEnv "HOME" home
  setEnv "XDG_DATA_HOME" (home </> "data")
  setEnv "XDG_CONFIG_HOME" (home </> "config")
  app <- GI.new Gtk.Application [#applicationId GI.:= "io.github.abdivasiyev.UstaUiTests", #flags GI.:= [Gio.ApplicationFlagsNonUnique]]
  finished <- newEmptyMVar
  void $ GI.on app #startup TextKeys.everywhere
  void $ GI.on app #activate $ do
    Gio.applicationHold app
    void $ forkIO $ do
      code <- (defaultMain (tests app) >> pure ExitSuccess) `catch` pure
      putMVar finished code
      onMain (Gio.applicationRelease app >> Gio.applicationQuit app)
  void (Gio.applicationRun app Nothing)
  takeMVar finished >>= exitWith

-- | One after another: there is one screen, and one keyboard.
tests :: Gtk.Application -> TestTree
tests app =
  dependentTestGroup "usta's window" AllFinish
    [ dependentTestGroup "every page shows, light and dark" AllFinish [testCase (show page) (pageShows app page) | page <- [minBound .. maxBound]]
    , testCase "the search opens with the platform's own shortcut" $ withWindow app Source.samples $ \w -> do
        triggers <- onMain (triggersIn w)
        assertBool (show triggers) (UIPlatform.keys "<Primary>k" `elem` triggers)
        onMain (visibleWithClass "u-palette-backdrop" w) >>= (@?= False)
        onMain (fireShortcut w (UIPlatform.keys "<Primary>k")) >>= assertBool "no shortcut took it"
        eventually "the search opening" 2000 (visibleWithClass "u-palette-backdrop" w)
    , testCase "on macOS the search's shortcut is Command K" $
        if Platform.current == Platform.MacOS then UIPlatform.keys "<Primary>k" @?= "<Meta>k" else pure ()
    , testCase "Enter sends a comment, and Shift Enter starts a line" editorSends
    , testCase "Emacs keys in a text field" emacsKeys
    , testCase "the repositories' forge filter narrows the list" $ withWindow app Source.samples $ \w -> do
        onMain (showPage w Repositories)
        pause 600
        before <- onMain (length <$> widgetsOf Gtk.ListBoxRow w)
        downs <- onMain (widgetsOf Gtk.DropDown w)
        case downs of
          forge : _ -> onMain (Gtk.dropDownSetSelected forge 1)
          [] -> assertFailure "no filters"
        pause 400
        after <- onMain (length <$> widgetsOf Gtk.ListBoxRow w)
        assertBool ("rows " <> show before <> " then " <> show after) (after < before && after > 0)
    ]
  where
    editorSends = do
      (window, buffer, sent) <- onMain $ do
        w <- GI.new Gtk.Window [#defaultWidth GI.:= 400, #defaultHeight GI.:= 200]
        (editor, buffer) <- MarkdownEditor.new MarkdownEditor.defaults
        sent <- GI.new Gtk.Label [#label GI.:= ""]
        MarkdownEditor.sendOn editor True (Gtk.textBufferGetBounds buffer >>= \(a, b) -> Gtk.textBufferGetText buffer a b False >>= \t -> GI.set sent [#label GI.:= t])
        box <- GI.new Gtk.Box [#orientation GI.:= Gtk.OrientationVertical]
        #append box editor >> #append box sent
        GI.set w [#child GI.:= box]
        Gtk.widgetSetVisible w True
        pure (w, buffer, sent)
      pause 300
      onMain $ Gtk.textBufferSetText buffer "first" (-1)
      editors <- onMain (widgetsOf Gtk.Box window)
      forM_ editors $ \b -> onMain (pressKey b (fromIntegral Gdk.KEY_Return) [Gdk.ModifierTypeShiftMask])
      onMain (GI.get sent #label) >>= (@?= "")
      tookEnter <- onMain (or <$> mapM (\b -> pressKey b (fromIntegral Gdk.KEY_Return) []) editors)
      assertBool "Enter was not taken" tookEnter
      onMain (GI.get sent #label) >>= (@?= "first")
      onMain (Gtk.windowDestroy window)
    emacsKeys = do
      (window, entry) <- onMain $ do
        w <- GI.new Gtk.Window []
        e <- GI.new Gtk.Entry [#text GI.:= "hello world"]
        GI.set w [#child GI.:= e]
        Gtk.widgetSetVisible w True
        void (Gtk.widgetGrabFocus e)
        pure (w, e)
      pause 300
      let ctl k = onMain (pressKey window (fromIntegral k) [Gdk.ModifierTypeControlMask])
      if Platform.current /= Platform.MacOS
        then pure () -- GTK has them itself elsewhere.
        else do
          mapM_ ctl [Gdk.KEY_a, Gdk.KEY_k]
          onMain (Gtk.editableGetText entry) >>= (@?= "")
          mapM_ ctl [Gdk.KEY_y]
          onMain (Gtk.editableGetText entry) >>= (@?= "hello world")
      onMain (Gtk.windowDestroy window)

-- | The page shows rows or words in the light theme and the dark one,
-- chosen as in Settings.
pageShows :: Gtk.Application -> Page -> IO ()
pageShows app page = forM_ [(Light, "light"), (Dark, "dark")] $ \(theme, name) ->
  withWindow app (themed theme) $ \w -> do
    onMain (showPage w page)
    pause 500
    eventually ("drawing it " <> name) 4000 (screenshot w ("dist-newstyle/ui-shots" </> T.unpack (T.toLower (T.pack (show page))) <> "-" <> name <> ".png"))
    words' <- onMain (labelsIn w)
    assertBool "no words on the page" (length words' > 5)

-- | The sample data, in one theme whatever the system's.
themed :: Theme -> Source.Source
themed theme = Source.samples {Source.preferences = pure Preferences.defaults {Preferences.theme = theme}}
