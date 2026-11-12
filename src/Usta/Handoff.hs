{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | One usta at a time: a later @usta <link>@ hands its links to the one
running, over a socket beside the cache, and quits. GApplication would
do this over D-Bus, which macOS does not have.
-}
module Usta.Handoff
    ( send
    , listen
    )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))

import Data.ByteString qualified as B
import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.GObject qualified as GObject
import GI.Gio qualified as Gio

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

{- | Takes what later ones send: @each@ gets each link, @shown@ runs for
every hand-off, links or not; both on the main thread.
-}
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

{- | Beside the cache; when that path is too long for a socket (macOS
takes 104 bytes), in the user's own runtime or temporary folder (not a
shared /tmp, where another user could answer), under a name made from
the cache's folder, so each usta's data has its own.
-}
socketPath :: IO FilePath
socketPath = do
    folder <- takeDirectory <$> databaseFile
    let
        beside = folder </> "usta.sock"
    if B.length (encodeUtf8 (T.pack beside)) <= 100
        then pure beside
        else do
            digest <- maybe "" (T.take 16) <$> GLib.computeChecksumForString GLib.ChecksumTypeSha256 (T.pack folder) (-1)
            temporary <- lookupEnv "XDG_RUNTIME_DIR" >>= maybe getTemporaryDirectory pure
            pure (temporary </> "usta-" <> T.unpack digest <> ".sock")
