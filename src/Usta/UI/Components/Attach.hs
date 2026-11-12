{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Files in a comment box: the toolbar's Attach button, an image pasted
from the clipboard, or files dropped on the box. Each goes to the forge,
and its link goes in at the cursor, as an image for an image.
-}
module Usta.UI.Components.Attach
    ( Uploader
    , attach
    )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.GObject qualified as GObject
import GI.Gdk qualified as Gdk
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import Usta.UI.Components.Icon qualified as Icon

{- | Sends a file, by name, to the forge and says where it is served; off
the main thread.
-}
type Uploader :: Type
type Uploader = Text -> ByteString -> IO (Either Text Text)

{- | Sets @view@ up for pasted images and dropped files, and gives the
toolbar button; @status@ says what is being sent, or why it was not.
-}
attach :: Uploader -> Gtk.TextView -> Gtk.Label -> IO Gtk.Button
attach upload view status = do
    buffer <- Gtk.textViewGetBuffer view
    let
        send name bytes = do
            GI.set status [#label := "Uploading " <> name <> "…", #visible := True]
            void $ forkIO $ do
                result <- try (upload name bytes)
                void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    case result of
                        Right (Right url) -> do
                            Gtk.textBufferInsertAtCursor buffer (linkOf name url <> " ") (-1)
                            GI.set status [#visible := False]
                        Right (Left problem) -> GI.set status [#label := "Could not attach " <> name <> ": " <> problem]
                        Left (e :: SomeException) -> GI.set status [#label := "Could not attach " <> name <> ": " <> T.pack (show e)]
                    pure False
        sendFile file = do
            name <- fromMaybe "file" <$> Gio.fileGetBasename file
            loaded <- try (Gio.fileLoadContents file (Nothing :: Maybe Gio.Cancellable))
            case loaded of
                Right (bytes, _) -> send (T.pack name) bytes
                Left (e :: SomeException) -> GI.set status [#label := "Could not read " <> T.pack name <> ": " <> T.pack (show e), #visible := True]

    -- The toolbar button opens the file chooser.
    image <- Icon.new 16 Icon.Chain
    button <- GI.new Gtk.Button [#child := image, #focusOnClick := False, #tooltipText := "Attach a file, or paste or drop one", #cssClasses := ["tool"]]
    void $ GI.on button #clicked $ do
        dialog <- GI.new Gtk.FileDialog [#title := "Attach a file", #modal := True]
        root <- Gtk.widgetGetRoot view
        window <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
        Gtk.fileDialogOpen dialog window (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
            chosen <- try (Gtk.fileDialogOpenFinish dialog result)
            case chosen of
                Right file -> sendFile file
                Left (_ :: SomeException) -> pure ()

    -- An image on the clipboard with no text is attached, not pasted.
    void $ GI.on view #pasteClipboard $ do
        clipboard <- Gtk.widgetGetClipboard view
        formats <- Gdk.clipboardGetFormats clipboard
        texture <- GI.glibType @Gdk.Texture
        isImage <- Gdk.contentFormatsContainGtype formats texture
        isText <- Gdk.contentFormatsContainMimeType formats "text/plain;charset=utf-8"
        when (isImage && not isText) $ do
            GObject.signalStopEmissionByName view "paste-clipboard"
            Gdk.clipboardReadTextureAsync clipboard (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
                read' <- try (Gdk.clipboardReadTextureFinish clipboard result)
                case read' of
                    Right (Just t) -> do
                        png <- Gdk.textureSaveToPngBytes t
                        bytes <- fromMaybe "" <$> GLib.bytesGetData png
                        send "pasted-image.png" bytes
                    Right Nothing -> pure ()
                    Left (_ :: SomeException) -> pure ()

    -- Files dropped on the box.
    fileType <- GI.glibType @Gio.File
    target <- Gtk.dropTargetNew fileType [Gdk.DragActionCopy]
    void $ GI.on target #drop $ \value _ _ -> do
        file <- GI.fromGValue value :: IO (Maybe Gio.File)
        forM_ file sendFile
        pure True
    Gtk.widgetAddController view target
    pure button

-- | An image shows in the comment; anything else is a link.
linkOf :: Text -> Text -> Text
linkOf name url
    | any (`T.isSuffixOf` T.toLower name) [".png", ".jpg", ".jpeg", ".gif", ".webp", ".svg"] = "![" <> name <> "](" <> url <> ")"
    | otherwise = "[" <> name <> "](" <> url <> ")"
