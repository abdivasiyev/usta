{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | One usta at a time: a later @usta <link>@ hands its links to the one
-- running, over a socket beside the cache, and quits. GApplication would
-- do this over D-Bus, which macOS does not have.
module Usta.Handoff
  ( send
  , listen
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.GObject as GObject
import System.Directory (removeFile)
import System.FilePath (takeDirectory, (</>))

import Usta.DB (databaseFile)

-- | Gives @links@, one a line, to the usta running; False when none is.
send :: [Text] -> IO Bool
send links = do
  path <- socketPath
  sent <- try $ do
    client <- Gio.socketClientNew
    address <- Gio.unixSocketAddressNew (T.pack path)
    connection <- Gio.socketClientConnect client address (Nothing :: Maybe Gio.Cancellable)
    output <- Gio.iOStreamGetOutputStream connection
    void $ Gio.outputStreamWriteAll output (encodeUtf8 (T.unlines links <> "\n")) (Nothing :: Maybe Gio.Cancellable)
    void $ Gio.iOStreamClose connection (Nothing :: Maybe Gio.Cancellable)
  pure (either (\(_ :: SomeException) -> False) (const True) sent)

-- | Takes what later ones send: @each@ gets each link, @shown@ runs for
-- every hand-off, links or not; both on the main thread.
listen :: (Text -> IO ()) -> IO () -> IO ()
listen each shown = do
  path <- socketPath
  -- A socket left by an usta that did not quit on its own answers no one.
  _ <- try (removeFile path) :: IO (Either SomeException ())
  service <- Gio.socketServiceNew
  address <- Gio.unixSocketAddressNew (T.pack path)
  bound <- try (Gio.socketListenerAddAddress service address Gio.SocketTypeStream Gio.SocketProtocolDefault (Nothing :: Maybe GObject.Object))
  case bound of
    Left (_ :: SomeException) -> pure ()
    Right _ -> do
      void $ GI.on service #incoming $ \connection _ -> do
        input <- Gio.iOStreamGetInputStream connection
        bytes <- Gio.inputStreamReadBytes input 65536 (Nothing :: Maybe Gio.Cancellable) >>= GLib.bytesGetData
        void (Gio.iOStreamClose connection (Nothing :: Maybe Gio.Cancellable))
        shown
        forM_ (filter (not . T.null) (map T.strip (T.lines (decodeUtf8Lenient (fromMaybe "" bytes))))) each
        pure True
      Gio.socketServiceStart service

socketPath :: IO FilePath
socketPath = (</> "usta.sock") . takeDirectory <$> databaseFile
