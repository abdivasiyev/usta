{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A PDF from a repository, drawn by poppler: the pages as thumbnails
on the left, the chosen one large, with page and zoom controls.
-}
module Usta.UI.Components.PdfView
    ( new
    , css
    )
where

import Control.Exception (try)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import Data.GI.Base (AttrOp ((:=)), GError)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import GI.Cairo.Render.Connector (renderWithContext)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))

import Data.ByteString qualified as B
import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.Cairo qualified as GICairo
import GI.Cairo.Render qualified as Cairo
import GI.GLib qualified as GLib
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Poppler qualified as Poppler

import Usta.UI.Components.Button qualified as Button

{- | @name@ is the file's name, for Open with…; 'Left' says why the bytes
are no PDF usta can read.
-}
new :: Text -> ByteString -> IO (Either Text Gtk.Widget)
new name bytes = do
    loaded <- try (GLib.bytesNew (Just bytes) >>= \b -> Poppler.documentNewFromBytes b Nothing)
    case loaded of
        Left (_ :: GError) -> pure (Left "usta cannot read this PDF.")
        Right document -> Right <$> viewer name bytes document

viewer :: Text -> ByteString -> Poppler.Document -> IO Gtk.Widget
viewer name bytes document = do
    count <- fromIntegral <$> Poppler.documentGetNPages document
    current <- newIORef (0 :: Int)
    zoom <- newIORef (1 :: Double)
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-pdf"]]

    toolbar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #cssClasses := ["toolbar"]]
    facts <- label (T.pack (show count) <> (if count == 1 then " page" else " pages")) ["muted"]
    GI.set facts [#hexpand := True]
    #append toolbar facts
    previous <- small "‹"
    pageLabel <- label "" ["page-number"]
    next <- small "›"
    zoomOut <- small "−"
    zoomLabel <- label "" ["zoom"]
    zoomIn <- small "+"
    fitWidth <- Button.new Button.defaults {Button.label = Just "Fit width", Button.variant = Button.Secondary, Button.small = True}
    openWith <- Button.new Button.defaults {Button.label = Just "Open with…", Button.variant = Button.Secondary, Button.small = True}
    forM_
        [Gtk.toWidget previous, Gtk.toWidget pageLabel, Gtk.toWidget next, Gtk.toWidget zoomOut, Gtk.toWidget zoomLabel, Gtk.toWidget zoomIn, Gtk.toWidget fitWidth, Gtk.toWidget openWith]
        (>>= #append toolbar)
    #append box toolbar

    body <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
    thumbs <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle, #cssClasses := ["thumbs"]]
    forM_ [0 .. count - 1] $ \i -> do
        page <- Poppler.documentGetPage document (fromIntegral i)
        (w, h) <- Poppler.pageGetSize page
        let
            scale = 84 / w
        area <-
            GI.new
                Gtk.DrawingArea
                [ #contentWidth := 84
                , #contentHeight := round (h * scale)
                , #cssClasses := ["sheet"]
                ]
        Gtk.drawingAreaSetDrawFunc area $ Just $ \_ context _ _ -> paint page scale context
        cell <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #halign := Gtk.AlignCenter]
        #append cell area
        label (T.pack (show (i + 1))) ["muted", "small"] >>= \l -> GI.set l [#halign := Gtk.AlignCenter] >> #append cell l
        #append thumbs cell
    thumbScroll <- GI.new Gtk.ScrolledWindow [#child := thumbs, #widthRequest := 124, #hscrollbarPolicy := Gtk.PolicyTypeNever]
    #append body thumbScroll

    sheet <- GI.new Gtk.DrawingArea [#halign := Gtk.AlignCenter, #valign := Gtk.AlignStart, #cssClasses := ["sheet", "large"]]
    stage <- GI.new Gtk.ScrolledWindow [#child := sheet, #hexpand := True, #vexpand := True, #cssClasses := ["stage"]]
    #append body stage
    #append box body

    let
        pageAt i = Poppler.documentGetPage document (fromIntegral i)
        redraw = do
            i <- readIORef current
            z <- readIORef zoom
            page <- pageAt i
            (w, h) <- Poppler.pageGetSize page
            GI.set sheet [#contentWidth := round (w * z), #contentHeight := round (h * z)]
            Gtk.drawingAreaSetDrawFunc sheet $ Just $ \_ context _ _ -> paint page z context
            Gtk.widgetQueueDraw sheet
            GI.set pageLabel [#label := T.pack (show (i + 1)) <> " of " <> T.pack (show count)]
            GI.set zoomLabel [#label := T.pack (show (round (z * 100) :: Int)) <> "%"]
            GI.set previous [#sensitive := i > 0]
            GI.set next [#sensitive := i < count - 1]
        go i = when (i >= 0 && i < count) $ do
            writeIORef current i
            row <- Gtk.listBoxGetRowAtIndex thumbs (fromIntegral i)
            Gtk.listBoxSelectRow thumbs row
            redraw
        zoomBy f = do
            z <- readIORef zoom
            writeIORef zoom (max 0.25 (min 4 (z * f)))
            redraw
    void $ GI.on thumbs #rowSelected $ \row -> forM_ row $ \r -> do
        i <- fromIntegral <$> Gtk.listBoxRowGetIndex r
        seen <- readIORef current
        when (i /= seen) $ writeIORef current i >> redraw
    void $ GI.on previous #clicked (readIORef current >>= go . subtract 1)
    void $ GI.on next #clicked (readIORef current >>= go . (+ 1))
    void $ GI.on zoomOut #clicked (zoomBy (1 / 1.25))
    void $ GI.on zoomIn #clicked (zoomBy 1.25)
    -- The page as wide as the space beside the thumbnails, less a margin.
    void $ GI.on fitWidth #clicked $ do
        room <- Gtk.widgetGetWidth stage
        (w, _) <- readIORef current >>= pageAt >>= Poppler.pageGetSize
        when (room > 0) $ writeIORef zoom ((fromIntegral room - 48) / w) >> redraw
    -- Another app gets a copy in the temporary folder.
    void $ GI.on openWith #clicked $ do
        folder <- (</> "usta") <$> getTemporaryDirectory
        createDirectoryIfMissing True folder
        let
            file = folder </> T.unpack (snd (T.breakOnEnd "/" name))
        B.writeFile file bytes
        launcher <- Gio.fileNewForPath file >>= Gtk.fileLauncherNew . Just
        Gtk.fileLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
    go 0
    Gtk.toWidget box
    where
        small text = GI.new Gtk.Button [#label := text, #cssClasses := ["small"]]

-- | White paper, then the page at @scale@.
paint :: Poppler.Page -> Double -> GICairo.Context -> IO ()
paint page scale context = do
    renderWithContext (Cairo.setSourceRGB 1 1 1 >> Cairo.paint >> Cairo.scale scale scale) context
    Poppler.pageRender page context

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css =
    mconcat
        [ "box.u-pdf box.toolbar { padding: 8px 12px; border-bottom: 1px solid var(--border); font-size: 13px; }"
        , "box.u-pdf box.toolbar button.small { min-width: 32px; min-height: 32px; padding: 0; }"
        , "box.u-pdf label.page-number { min-width: 64px; }"
        , "box.u-pdf label.zoom { min-width: 44px; }"
        , "box.u-pdf list.thumbs { background: var(--sidebar); padding: 8px 0; }"
        , "box.u-pdf list.thumbs row { padding: 6px; border-radius: 6px; margin: 0 8px; }"
        , "box.u-pdf list.thumbs row:selected { background: var(--selected); box-shadow: inset 0 0 0 2px var(--accent); }"
        , "box.u-pdf drawingarea.sheet { border: 1px solid var(--border); }"
        , "box.u-pdf scrolledwindow.stage { background: var(--neutral-bg); }"
        , "box.u-pdf drawingarea.large { margin: 24px; box-shadow: 0 6px 24px rgba(27, 29, 33, 0.16); border: none; }"
        ]
