{-# LANGUAGE OverloadedStrings #-}

{- | The web browser, for every link usta does not open itself. On macOS
usta can be the default browser, so forge links from any app open here;
the rest then go on to the browser chosen in Settings, never back to
usta.
-}
module Usta.UI.Browser
    ( open
    , Browser (..)
    , browsers
    , isDefault
    , makeDefault
    )
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, mfilter, void)
import Data.Kind (Type)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekElemOff)
import System.Process.Typed (proc, runProcess_)

import Data.Text qualified as T
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import Usta.Preferences qualified as Preferences
import Usta.UI.Platform qualified as Platform

-- | An app that opens web links.
type Browser :: Type
data Browser = Browser {bundle :: Text, name :: Text}
    deriving stock (Eq, Show)

-- | usta's own bundle, which links must not go back to.
self :: Text
self = "io.github.abdivasiyev.Usta"

{- | Opens @url@ in a browser: the one chosen in Settings, else the
system's default unless that is usta, else Safari.
-}
open :: Text -> IO ()
open url
    | Platform.current == Platform.MacOS = do
        chosen <- (.browser) <$> Preferences.effective
        now <- defaultBrowser
        let
            target = fromMaybe "com.apple.Safari" (chosen <|> mfilter (/= self) now)
        void $ forkIO $ void (try (runProcess_ (proc "open" ["-b", T.unpack target, T.unpack url])) :: IO (Either SomeException ()))
    | otherwise = do
        launcher <- Gtk.uriLauncherNew (Just url)
        Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing

-- | Every browser but usta, with its name.
browsers :: IO [Browser]
browsers = alloca $ \out -> do
    n <- c_browsers out
    list <- peek out
    ids <- forM [0 .. fromIntegral n - 1] $ \i -> do
        p <- peekElemOff list i
        t <- T.pack <$> peekCString p
        free p
        pure t
    if list == nullPtr then pure () else free list
    named <- forM (filter (/= self) ids) $ \b -> fmap (Browser b) <$> nameOf b
    pure (catMaybes named)

nameOf :: Text -> IO (Maybe Text)
nameOf b = do
    c <- newCString (T.unpack b)
    found <- c_appName c
    free c
    owned found

-- | Whether macOS opens web links with usta now.
isDefault :: IO Bool
isDefault = (== Just self) <$> defaultBrowser

defaultBrowser :: IO (Maybe Text)
defaultBrowser = c_defaultBrowser >>= owned

-- | Asks macOS to open web links with usta; the system asks to confirm.
makeDefault :: IO ()
makeDefault = c_makeDefault

-- | A string the C side made, freed once read.
owned :: CString -> IO (Maybe Text)
owned p
    | p == nullPtr = pure Nothing
    | otherwise = do
        t <- T.pack <$> peekCString p
        free p
        pure (Just t)

foreign import ccall unsafe "usta_browsers" c_browsers :: Ptr (Ptr CString) -> IO CInt

foreign import ccall unsafe "usta_app_name" c_appName :: CString -> IO CString

foreign import ccall unsafe "usta_default_browser" c_defaultBrowser :: IO CString

foreign import ccall unsafe "usta_make_default_browser" c_makeDefault :: IO ()
