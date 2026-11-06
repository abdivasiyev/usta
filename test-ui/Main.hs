{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | usta's window under test: its pages built from the sample data,
-- drawn in light and dark, and driven with the keys and clicks a person
-- would use. Needs a display; screenshots go to dist-newstyle/ui-shots.
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (catch)
import Control.Monad (forM_, void, when)
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
import World (World (..), withWorld)
import Data.IORef (readIORef)
import Usta.Preferences (Theme (..))
import qualified Usta.Preferences as Preferences
import Usta.Details (From (..))
import Usta.Types.Diff (parseDiff)
import qualified Usta.Types.Platform as Platform
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import qualified Data.Maybe
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
        -- As GTK writes it: "<Meta>k" on macOS, "<Control>k" elsewhere.
        wanted <- onMain (Gtk.shortcutTriggerParseString (UIPlatform.keys "<Primary>k") >>= maybe (pure "") Gtk.shortcutTriggerToString)
        assertBool (show triggers) (wanted `elem` triggers)
        onMain (visibleWithClass "u-palette-backdrop" w) >>= (@?= False)
        onMain (fireShortcut w wanted) >>= assertBool "no shortcut took it"
        eventually "the search opening" 2000 (visibleWithClass "u-palette-backdrop" w)
    , testCase "on macOS the search's shortcut is Command K" $
        if Platform.current == Platform.MacOS then UIPlatform.keys "<Primary>k" @?= "<Meta>k" else pure ()
    , dependentTestGroup "dialogs open over their window"
        AllFinish
        [ testCase (T.unpack title) (dialogOverWindow app button title) | (button, title) <- [("+ New", "New repository"), ("⑂ Fork…", "Fork"), ("Clone…", "Clone")] ]
    , testCase "a pull request opens from the inbox, and its files show" $ withWindow app withDiff $ \w -> do
        onMain (click w "Open review") >>= assertBool "no Open review"
        -- Pages cross-fade: the inbox is on screen a moment longer.
        let only wanted = (\ls -> wanted `elem` ls && "Mark all done" `notElem` ls) <$> labelsIn w
        eventually "the pull request's tabs" 3000 (only "Conversation")
        onMain (click w "Files") >>= assertBool "no Files tab"
        eventually "the changed file" 3000 (only "usta.cabal")
        -- A picture shows what was last drawn: let a frame be.
        onMain (Gtk.windowPresent w) >> pause 800
        eventually "drawing it" 3000 (screenshot w "dist-newstyle/ui-shots/pull-request-files.png")
        -- Down the long file, its name stays on top of the diff.
        let nameAt = labelsReading "usta.cabal" w >>= mapM (`placeIn` w)
        scrolled <- onMain (scrollLongest w 600)
        assertBool "no diff to scroll" scrolled
        pause 400
        after <- onMain nameAt
        assertBool ("the file's name went with its lines: " <> show after) (any (maybe False (\(x, y) -> x > 280 && y > 150 && y < 320)) after)
    , dependentTestGroup "with accounts" AllFinish
        [ testCase "a pull request's checks" $ inWorld app $ \_ w -> do
            openReview w
            onMain (click w "Checks") >>= assertBool "no Checks tab"
            eventually "the checks" 3000 (onScreen w ["build (push)", "test (push)"])
        , testCase "a pull request's commits" $ inWorld app $ \_ w -> do
            openReview w
            onMain (click w "Commits") >>= assertBool "no Commits tab"
            eventually "the commit" 3000 (onScreen w ["Add the GTK 4 window"])
        , testCase "Enter in the comment box sends the comment" $ inWorld app $ \world w -> do
            openReview w
            views <- onMain (widgetsOf Gtk.TextView w)
            case views of
              v : _ -> onMain (Gtk.textViewGetBuffer v >>= \b -> Gtk.textBufferSetText b "Looks good" (-1))
              [] -> assertFailure "no comment box"
            boxes <- onMain (widgetsOf Gtk.Box w)
            _ <- onMain (or <$> mapM (\b -> pressKey b (fromIntegral Gdk.KEY_Return) []) boxes)
            eventually "the comment sent" 3000 (elem "comment #2: Looks good" <$> readIORef world.sent)
        , testCase "an issue opens from the inbox" $ inWorld app $ \_ w -> do
            rows <- onMain (widgetsOf Gtk.ListBoxRow w)
            issueRows <- onMain (filterM' (\r -> elem "Diff view: keep scroll position when switching files" <$> labelsIn r) rows)
            case issueRows of
              r : _ -> onMain (Gtk.widgetActivate r) >> pure ()
              [] -> assertFailure "no issue in the inbox"
            onMain (click w "Open issue") >>= assertBool "no Open issue"
            eventually "the issue's page" 3000 ((\ls -> "Conversation" `notElem` ls && "Mark all done" `notElem` ls && "Diff view: keep scroll position when switching files" `elem` ls) <$> labelsIn w)
        , testCase "a new issue is filed from its form" $ inWorld app $ \world w -> do
            onMain (showPage w Issues)
            pause 500
            onMain (click w "New issue") >>= assertBool "no New issue"
            eventually "the form" 3000 ((/= Nothing) <$> dialogTitled "New issue")
            Just form <- onMain (dialogTitled "New issue")
            entries <- onMain (widgetsOf Gtk.Entry form)
            titles <- onMain (filterM' (\e -> (== Just "Short summary of the problem or idea") <$> Gtk.entryGetPlaceholderText e) entries)
            case titles of
              e : _ -> onMain (Gtk.editableSetText e "Filed from the tests")
              [] -> assertFailure "no title field"
            onMain (click form "Create issue") >>= assertBool "no Create issue"
            eventually "the issue filed" 3000 (any ("issue: Filed from the tests" ==) <$> readIORef world.sent)
        , testCase "the search finds a pull request by its words, and Enter opens it" $ inWorld app $ \_ w -> do
            wanted <- onMain (Gtk.shortcutTriggerParseString (UIPlatform.keys "<Primary>k") >>= maybe (pure "") Gtk.shortcutTriggerToString)
            onMain (fireShortcut w wanted) >>= assertBool "no search"
            entries <- onMain (widgetsOf Gtk.SearchEntry w)
            palette <- onMain (filterM' (\e -> (== Just "Search everything, or type > for commands") <$> Gtk.searchEntryGetPlaceholderText e) entries)
            entry <- case palette of
              e : _ -> pure e
              [] -> assertFailure "no search field" >> fail "no search field"
            onMain (Gtk.editableSetText entry "gi-gtk4")
            eventually "the pull request chosen" 3000 (elem "feat: add gi-gtk4 dependency" <$> chosenLabels w)
            -- Enter runs the result chosen, the first.
            onMain (chosenResult w) >>= assertBool "nothing chosen"
            eventually "the pull request's page" 3000 ((\ls -> "Conversation" `elem` ls && "Mark all done" `notElem` ls) <$> labelsIn w)
        , testCase "the theme switches from the search, and is kept" $ inWorld app $ \world w -> do
            before <- (.theme) <$> world.source.preferences
            wanted <- onMain (Gtk.shortcutTriggerParseString (UIPlatform.keys "<Primary>k") >>= maybe (pure "") Gtk.shortcutTriggerToString)
            onMain (fireShortcut w wanted) >>= assertBool "no search"
            entries <- onMain (widgetsOf Gtk.SearchEntry w)
            palette <- onMain (filterM' (\e -> (== Just "Search everything, or type > for commands") <$> Gtk.searchEntryGetPlaceholderText e) entries)
            case palette of
              e : _ -> do
                onMain (Gtk.editableSetText e ">theme")
                eventually "the theme's command" 3000 (any ("theme" `T.isInfixOf`) <$> labelsIn w)
                eventually "the theme's command chosen" 3000 (any ("theme" `T.isInfixOf`) <$> chosenLabels w)
                onMain (chosenResult w) >>= assertBool "nothing chosen"
              [] -> assertFailure "no search field"
            eventually "the theme kept" 3000 ((/= before) . (.theme) <$> world.source.preferences)
        ]
    , testCase "Markdown draws every kind of block" markdownBlocks
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

-- | The button on the repository page opens its dialog on top of the
-- window, modal to it.
dialogOverWindow :: Gtk.Application -> T.Text -> T.Text -> IO ()
dialogOverWindow app button title = withWindow app Source.samples $ \w -> do
  onMain (showPage w Repositories)
  pause 600
  -- The repository's other actions are under its "⋯" menu.
  when (button /= "+ New") $ onMain (openMenu w "More") >>= assertBool "no menu"
  pause 300
  onMain (click w button) >>= assertBool ("no " <> T.unpack button)
  eventually ("the " <> T.unpack title <> " dialog") 3000 ((/= Nothing) <$> dialogTitled title)
  Just dialog <- onMain (dialogTitled title)
  parent <- onMain (Gtk.windowGetTransientFor dialog)
  mine <- onMain (Gtk.toWindow w)
  assertBool "not over the window" (parent == Just mine)
  onMain (Gtk.windowGetModal dialog) >>= assertBool "not modal"
  onMain (Gtk.windowDestroy dialog)

-- | The sample data, with a diff for every pull request: the long file
-- of "usta.cabal" and a short one.
withDiff :: Source.Source
withDiff = Source.samples {Source.pullFiles = \_ _ _ -> pure ((,) FromForge <$> parseDiff diff)}
  where
    diff = T.unlines $
      [ "diff --git a/usta.cabal b/usta.cabal", "--- a/usta.cabal", "+++ b/usta.cabal", "@@ -1,80 +1,81 @@" ]
        <> [" line " <> T.pack (show n) | n <- [1 .. 40 :: Int]]
        <> ["-  build-depends: base", "+  build-depends:", "+    base,"]
        <> [" line " <> T.pack (show n) | n <- [43 .. 80 :: Int]]
        <> [ "diff --git a/README.md b/README.md", "--- a/README.md", "+++ b/README.md", "@@ -1 +1 @@", "-usta", "+usta, a forge client" ]


-- | usta's window in a 'World', while @act@ runs.
inWorld :: Gtk.Application -> (World -> Gtk.ApplicationWindow -> IO ()) -> IO ()
inWorld app act = withWorld $ \world -> do
  withWindow app world.source (act world)
  -- Loads the window started finish before its cache closes.
  pause 1500

-- | Opens the inbox's first item, a pull request, and waits for its page.
openReview :: Gtk.ApplicationWindow -> IO ()
openReview w = do
  onMain (click w "Open review") >>= assertBool "no Open review"
  eventually "the pull request's page" 3000 ((\ls -> "Conversation" `elem` ls && "Mark all done" `notElem` ls) <$> labelsIn w)

-- | Whether every one of @texts@ is on screen.
onScreen :: Gtk.ApplicationWindow -> [T.Text] -> IO Bool
onScreen w texts = (\ls -> all (`elem` ls) texts) <$> labelsIn w

filterM' :: (a -> IO Bool) -> [a] -> IO [a]
filterM' p = fmap concat . mapM (\x -> p x >>= \keep -> pure [x | keep])

-- | The search's chosen result, its words.
chosenLabels :: Gtk.ApplicationWindow -> IO [T.Text]
chosenLabels w = paletteChosen w >>= maybe (pure []) labelsIn

-- | Runs the search's chosen result, as Enter in its field does.
chosenResult :: Gtk.ApplicationWindow -> IO Bool
chosenResult w = paletteChosen w >>= maybe (pure False) Gtk.widgetActivate

paletteChosen :: Gtk.ApplicationWindow -> IO (Maybe Gtk.ListBoxRow)
paletteChosen w = do
  palettes <- descendants w >>= filterM' (`Gtk.widgetHasCssClass` "u-palette")
  lists <- concat <$> mapM (widgetsOf Gtk.ListBox) palettes
  chosen <- concat <$> mapM (\l -> maybe [] pure <$> Gtk.listBoxGetSelectedRow l) lists
  pure (case chosen of
    row : _ -> Just row
    [] -> Nothing)

-- | A document of every block Markdown has, drawn in a window: each as
-- its own widget, in light and dark.
markdownBlocks :: IO ()
markdownBlocks = do
  window <- onMain $ do
    w <- GI.new Gtk.Window [#defaultWidth GI.:= 900, #defaultHeight GI.:= 1400]
    doc <- Markdown.new document
    scroller <- GI.new Gtk.ScrolledWindow [#child GI.:= doc]
    GI.set w [#child GI.:= scroller]
    Gtk.widgetSetVisible w True
    pure w
  pause 800
  ls <- onMain (textsIn window)
  mapM_ (\t -> assertBool ("missing " <> T.unpack t <> " in " <> show ls) (any (t `T.isInfixOf`) ls))
    ["Getting started", "Install", "nix run", "Name", "usta", "Diagram · mermaid", "a quote", "Done already", "Still to do"]
  checks <- onMain (descendants window >>= fmap Data.Maybe.catMaybes . mapM (GI.castTo Gtk.CheckButton) >>= mapM Gtk.checkButtonGetActive)
  checks @?= [True, False]
  grids <- onMain (widgetsOf Gtk.Grid window)
  assertBool "no table" (not (null grids))
  expanders <- onMain (descendants window >>= fmap Data.Maybe.catMaybes . mapM (GI.castTo Gtk.Expander) >>= mapM Gtk.expanderGetLabel)
  expanders @?= [Just "More"]
  eventually "drawing it" 3000 (screenshot window "dist-newstyle/ui-shots/markdown.png")
  onMain (Gtk.windowDestroy window)
  where
    document = T.unlines
      [ "# Getting started", "", "Some *emphasis*, **strong**, `code` and a [link](https://usta.azizovich.uz).", ""
      , "## Install", "", "```sh", "nix run", "```", ""
      , "| Name | Kind |", "|------|------|", "| usta | app |", ""
      , "- [x] Done already", "- [ ] Still to do", ""
      , "> a quote", "", "---", ""
      , "```mermaid", "graph LR", "  A --> B", "```", ""
      , "<details><summary>More</summary>", "", "Hidden text", "", "</details>" ]
