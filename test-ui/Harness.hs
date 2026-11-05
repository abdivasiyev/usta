{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Driving usta's GTK window from tests. GTK lives on the main thread,
-- where its main loop runs; the tests run on their own and hand it work
-- with 'onMain'. Keys reach widgets as GTK would send them: through
-- their shortcut and key controllers.
module Harness
  ( onMain
  , pause
  , eventually
  , descendants
  , widgetsOf
  , labelsIn
  , visibleWithClass
  , fireShortcut
  , triggersIn
  , pressKey
  , screenshot
  , withWindow
  , showPage
  , click
  , openMenu
  , dialogTitled
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (filterM, forM, forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base.BasicTypes (GObject, ManagedPtr)
import Data.GI.Base.ManagedPtr (withManagedPtr)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Graphene as Graphene
import qualified GI.Gsk as Gsk
import qualified GI.Gtk as Gtk
import System.Directory (createDirectoryIfMissing, getFileSize)
import System.FilePath (takeDirectory)
import Test.Tasty.HUnit (assertFailure)

import Usta.UI.Sidebar (Page, pageName)
import Usta.UI.Source (Source)
import qualified Usta.UI.Window as Window

-- | Runs @act@ on GTK's thread and waits for it; what it throws comes
-- back here.
onMain :: IO a -> IO a
onMain act = do
  done <- newEmptyMVar
  void $ GLib.idleAdd GLib.PRIORITY_DEFAULT (try act >>= putMVar done >> pure False)
  takeMVar done >>= either (\(e :: SomeException) -> throwIO e) pure

-- | Lets GTK work for @ms@.
pause :: Int -> IO ()
pause ms = threadDelay (ms * 1000)

-- | Waits up to @ms@ for @check@, asked on GTK's thread; fails saying
-- @what@ did not happen.
eventually :: String -> Int -> IO Bool -> IO ()
eventually what ms check = go (ms `div` 50)
  where
    go n = do
      ok <- onMain check
      if ok then pure () else if n <= 0 then assertFailure (what <> " did not happen") else pause 50 >> go (n - 1)

descendants :: Gtk.IsWidget w => w -> IO [Gtk.Widget]
descendants w = Gtk.widgetGetFirstChild w >>= go
  where
    go Nothing = pure []
    go (Just c) = do
      below <- descendants c
      next <- Gtk.widgetGetNextSibling c
      (c :) . (below <>) <$> go next

-- | The widgets of one type under @w@ that are on screen.
widgetsOf :: (Gtk.IsWidget w, GObject t, Gtk.IsWidget t) => (ManagedPtr t -> t) -> w -> IO [t]
widgetsOf kind w = descendants w >>= fmap catMaybes . mapM (GI.castTo kind) >>= filterM Gtk.widgetGetMapped

labelsIn :: Gtk.IsWidget w => w -> IO [Text]
labelsIn w = widgetsOf Gtk.Label w >>= mapM Gtk.labelGetLabel

-- | Whether a widget with the CSS class is visible under @w@.
visibleWithClass :: Gtk.IsWidget w => Text -> w -> IO Bool
visibleWithClass cls w = do
  found <- descendants w >>= filterM (`Gtk.widgetHasCssClass` cls)
  or <$> mapM Gtk.widgetGetVisible found

-- | Every shortcut trigger under @w@ (and on it), as GTK writes them.
triggersIn :: Gtk.IsWidget w => w -> IO [Text]
triggersIn w = do
  self <- Gtk.toWidget w
  ws <- (self :) <$> descendants w
  concat <$> forM ws (\x -> map fst <$> shortcutsOf x)

shortcutsOf :: Gtk.Widget -> IO [(Text, (Gtk.ShortcutAction, Gtk.Widget))]
shortcutsOf w = do
  ctrls <- Gtk.widgetObserveControllers w
  n <- Gio.listModelGetNItems ctrls
  fmap concat $ forM (takeWhile (< n) [0 ..]) $ \i -> do
    found <- Gio.listModelGetItem ctrls i >>= maybe (pure Nothing) (GI.castTo Gtk.ShortcutController)
    case found of
      Nothing -> pure []
      Just c -> do
        model <- Gio.toListModel c
        k <- Gio.listModelGetNItems model
        fmap catMaybes $ forM (takeWhile (< k) [0 ..]) $ \j -> do
          s <- Gio.listModelGetItem model j >>= maybe (pure Nothing) (GI.castTo Gtk.Shortcut)
          case s of
            Nothing -> pure Nothing
            Just sc -> do
              t <- Gtk.shortcutGetTrigger sc
              a <- Gtk.shortcutGetAction sc
              case (t, a) of
                (Just t', Just a') -> Gtk.shortcutTriggerToString t' >>= \str -> pure (Just (str, (a', w)))
                _ -> pure Nothing

-- | Activates the shortcut written @trigger@ under @w@, as pressing it
-- would; 'False' when there is none.
fireShortcut :: Gtk.IsWidget w => w -> Text -> IO Bool
fireShortcut w trigger = do
  self <- Gtk.toWidget w
  ws <- (self :) <$> descendants w
  found <- concat <$> mapM shortcutsOf ws
  case [x | (t, x) <- found, t == trigger] of
    (action, owner) : _ -> Gtk.shortcutActionActivate action [] owner Nothing
    [] -> pure False

-- | Sends a key to @w@'s key controllers until one takes it.
pressKey :: Gtk.IsWidget w => w -> Word32 -> [Gdk.ModifierType] -> IO Bool
pressKey w keyval mods = do
  model <- Gtk.widgetObserveControllers w
  n <- Gio.listModelGetNItems model
  ctrls <- catMaybes <$> forM (takeWhile (< n) [0 ..]) (\i -> Gio.listModelGetItem model i >>= maybe (pure Nothing) (GI.castTo Gtk.EventControllerKey))
  let state = fromIntegral (sum [fromEnum m | m <- mods])
      go [] = pure False
      go (c : rest) = do
        took <- withManagedPtr c $ \p -> c_press p keyval state
        if took /= 0 then pure True else go rest
  go ctrls

foreign import ccall "usta_test_press" c_press :: Ptr Gtk.EventControllerKey -> Word32 -> Word32 -> IO CInt

-- | The widget as drawn, saved to @path@; 'False' when nothing was drawn.
screenshot :: Gtk.IsWidget w => w -> FilePath -> IO Bool
screenshot target path = do
  w <- Gtk.toWidget target
  width <- Gtk.widgetGetWidth w
  height <- Gtk.widgetGetHeight w
  paintable <- Gtk.widgetPaintableNew (Just w)
  snap <- Gtk.snapshotNew
  Gdk.paintableSnapshot paintable snap (fromIntegral width) (fromIntegral height)
  node <- Gtk.snapshotToNode snap
  native <- Gtk.widgetGetNative w
  renderer <- maybe (pure Nothing) Gtk.nativeGetRenderer native
  case (node, renderer) of
    (Just n, Just r) -> do
      texture <- Gsk.rendererRenderTexture r n (Nothing :: Maybe Graphene.Rect)
      createDirectoryIfMissing True (takeDirectory path)
      saved <- Gdk.textureSaveToPng texture path
      -- An empty window's picture is a few hundred bytes.
      (saved &&) . (> 4000) <$> getFileSize path
    _ -> pure False

-- | usta's window on @source@, shown, while @act@ runs.
withWindow :: Gtk.Application -> Source -> (Gtk.ApplicationWindow -> IO a) -> IO a
withWindow app source act = do
  window <- onMain $ do
    w <- Window.build source app
    Gtk.windowSetDefaultSize w 1280 800
    Gtk.widgetSetVisible w True
    pure w
  pause 1200
  result <- try (act window)
  onMain (Gtk.windowDestroy window)
  either (\(e :: SomeException) -> throwIO e) pure result

-- | Shows a page, as a click in the sidebar does.
showPage :: Gtk.IsWidget w => w -> Page -> IO ()
showPage w page = do
  stacks <- descendants w >>= fmap catMaybes . mapM (GI.castTo Gtk.Stack)
  forM_ stacks $ \s -> do
    child <- Gtk.stackGetChildByName s (pageName page)
    when (maybe False (const True) child) $ Gtk.stackSetVisibleChildName s (pageName page)

-- | Clicks the button under @w@ that reads @text@, shown or not (as one
-- in a closed menu); 'False' when there is none.
click :: Gtk.IsWidget w => w -> Text -> IO Bool
click w text = do
  buttons <- descendants w >>= fmap catMaybes . mapM (GI.castTo Gtk.Button)
  found <- filterM (\b -> (== Just text) <$> Gtk.buttonGetLabel b) buttons
  case found of
    b : _ -> Gtk.widgetActivate b
    [] -> pure False

-- | The open window titled @title@, if there is one.
dialogTitled :: Text -> IO (Maybe Gtk.Window)
dialogTitled title = do
  model <- Gtk.windowGetToplevels
  n <- Gio.listModelGetNItems model
  windows <- catMaybes <$> forM (takeWhile (< n) [0 ..]) (\i -> Gio.listModelGetItem model i >>= maybe (pure Nothing) (GI.castTo Gtk.Window))
  found <- filterM (\x -> (== Just title) <$> Gtk.windowGetTitle x) windows
  pure (case found of
    x : _ -> Just x
    [] -> Nothing)

-- | Opens the menu button under @w@ whose tooltip is @tip@.
openMenu :: Gtk.IsWidget w => w -> Text -> IO Bool
openMenu w tip = do
  menus <- widgetsOf Gtk.MenuButton w
  found <- filterM (\m -> (== Just tip) <$> Gtk.widgetGetTooltipText m) menus
  case found of
    m : _ -> Gtk.menuButtonPopup m >> pure True
    [] -> pure False
