{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Report a problem: a new issue on usta's GitHub, filled in the browser
with what the user wrote and what they chose to attach: the version and
system, the log's last lines, the macOS crash report. Nothing is sent
from here; the browser shows it all before the user submits it. The
banner says, at the next start, that usta quit unexpectedly.
-}
module Usta.UI.Pages.ReportProblem
    ( open
    , banner
    , css
    )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void)
import Data.GI.Base (AttrOp ((:=)))
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Data.Version (showVersion)
import Network.HTTP.Types (urlEncode)
import System.Directory (copyFile, createDirectoryIfMissing, getFileSize, getHomeDirectory, getModificationTime, listDirectory)
import System.FilePath ((</>))
import System.Info (compilerVersion)
import System.Process.Typed (proc, readProcessStdout_)

import Data.ByteString.Lazy qualified as BL
import Data.GI.Base qualified as GI
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Paths_usta (version)

import Usta.Log qualified as Log
import Usta.UI.Browser qualified as Browser
import Usta.UI.Components.Button qualified as Button
import Usta.UI.Platform qualified as Platform

{- | The dialog over the window @anchor@ is in, its title filled with
@title@.
-}
open :: Gtk.Widget -> Text -> IO ()
open anchor title = do
    root <- Gtk.widgetGetRoot anchor
    parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
    dialog <-
        GI.new
            Gtk.Window
            [ #title := "Report a problem"
            , #modal := True
            , #defaultWidth := 680
            , #destroyWithParent := True
            , #cssClasses := ["usta", "u-report"]
            ]
    Platform.dialogOver dialog parent
    header <- GI.new Gtk.HeaderBar []
    Platform.configureHeaderBar Platform.current header
    titles <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #valign := Gtk.AlignCenter]
    label "Report a problem" ["title"] >>= #append titles
    label "Opens a new issue on abdivasiyev/usta. You see everything before it is sent." ["subtitle"] >>= #append titles
    GI.set header [#titleWidget := titles]
    GI.set dialog [#titlebar := header]

    body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8, #cssClasses := ["form"]]
    label "Title" ["step"] >>= #append body
    titleEntry <- GI.new Gtk.Entry [#text := title]
    #append body titleEntry
    label "What happened" ["step"] >>= #append body
    happened <- GI.new Gtk.TextView [#wrapMode := Gtk.WrapModeWordChar, #heightRequest := 80, #cssClasses := ["happened"]]
    #append body happened

    system <- systemLine
    logLines <- Log.lastLines 200
    crash <- latestCrash
    label "Attached" ["step"] >>= #append body
    preview <- GI.new Gtk.Label [#xalign := 0, #yalign := 0, #selectable := True, #wrap := True, #cssClasses := ["preview"], #visible := False]
    let
        attachments =
            [ ("Version and system", system, system)
            , ("Last 200 log lines", "tokens, request bodies and your home folder left out", T.unlines logLines)
            ]
                <> [ ("macOS crash report", name <> " · " <> T.pack (show (size `div` 1024)) <> " KB", "Saved as a text file in Downloads, to drop into the issue.")
                   | Just (name, _, size) <- [crash]
                   ]
    checks <- forM attachments $ \(name, detail, shown) -> do
        row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["attached"]]
        check <- GI.new Gtk.CheckButton [#active := True]
        #append row check
        texts <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #hexpand := True]
        label name ["name"] >>= #append texts
        d <- label detail ["muted"]
        GI.set d [#ellipsize := Pango.EllipsizeModeEnd]
        #append texts d
        #append row texts
        showing <- Button.new Button.defaults {Button.label = Just "Show", Button.variant = Button.Flat, Button.small = True}
        void $ GI.on showing #clicked $ GI.set preview [#label := (if T.null shown then "The log is empty." else shown), #visible := True]
        #append row showing
        #append body row
        pure check
    scroll <- GI.new Gtk.ScrolledWindow [#child := preview, #maxContentHeight := 180, #propagateNaturalHeight := True, #hscrollbarPolicy := Gtk.PolicyTypeNever]
    #append body scroll

    footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["footer"]]
    note <- label "" ["muted"]
    GI.set note [#hexpand := True, #wrap := True]
    cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
    go <- Button.new Button.defaults {Button.label = Just "Open the issue form", Button.variant = Button.Primary}
    mapM_ (#append footer) =<< sequence [Gtk.toWidget note, Gtk.toWidget cancel, Gtk.toWidget go]
    content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
    mapM_ (#append content) [body, footer]
    GI.set dialog [#child := content]

    void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
    void $ GI.on go #clicked $ do
        t <- Gtk.editableGetText titleEntry
        buffer <- Gtk.textViewGetBuffer happened
        (s, e) <- Gtk.textBufferGetBounds buffer
        what <- Gtk.textBufferGetText buffer s e False
        chosen <- mapM (`GI.get` #active) checks
        let
            on' k = drop k chosen `startsWith` True
            withLog = on' 1
            logText = T.unlines logLines
        stamp <- T.pack . formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getZonedTime
        downloads <- (</> "Downloads") <$> getHomeDirectory
        -- A link carries only text, so the crash report and a long log become
        -- files to drop into the issue, in a folder of their own. GitHub takes
        -- no .ips, so the crash report goes as the text it is.
        let
            folder = downloads </> ("usta-report-" <> T.unpack stamp)
            inline = T.length logText < 5000
            crashFile = "usta-crash-" <> stamp <> ".txt"
            logFile = "usta-log-" <> stamp <> ".txt"
            saving =
                [(folder </> T.unpack crashFile, Left path) | on' 2, Just (_, path, _) <- [crash]]
                    <> [(folder </> T.unpack logFile, Right logText) | withLog, not inline]
        saved <- fmap concat $ forM saving $ \(to, what') -> do
            done <- try (createDirectoryIfMissing True folder >> either (`copyFile` to) (T.writeFile to) what') :: IO (Either SomeException ())
            pure [to | Right () <- [done]]
        let
            report =
                T.intercalate "\n\n"
                    $ [what | not (T.null (T.strip what))]
                        <> ["### Version and system\n\n" <> system | on' 0]
                        <> [ if inline then "### Log\n\n```\n" <> logText <> "```" else "### Log\n\nAttached: " <> logFile
                           | withLog
                           ]
                        <> ["### Crash report\n\nAttached: " <> crashFile | on' 2, crash /= Nothing]
            url = "https://github.com/abdivasiyev/usta/issues/new?title=" <> enc t <> "&body=" <> enc report
        GI.set note [#label := if null saved then "" else "Drop the files from the Finder window into the issue: a link carries only text."]
        Browser.open url
        -- The folder opens with the files chosen, ready to drag.
        forM_ (take 1 saved) $ \first -> do
            launcher <- Gtk.fileLauncherNew . Just =<< Gio.fileNewForPath first
            Gtk.fileLauncherOpenContainingFolder launcher parent (Nothing :: Maybe Gio.Cancellable) Nothing
    #present dialog
    where
        startsWith (x : _) y = x == y
        startsWith [] _ = False
        enc = decodeUtf8Lenient . urlEncode True . encodeUtf8

-- | "usta 0.1.0 · macOS 26.1 · GTK 4.20 · GHC 9.12".
systemLine :: IO Text
systemLine = do
    osVersion <- either (\(_ :: SomeException) -> "") (T.strip . decodeUtf8Lenient . BL.toStrict) <$> try (readProcessStdout_ (proc "sw_vers" ["-productVersion"]))
    major <- Gtk.getMajorVersion
    minor <- Gtk.getMinorVersion
    pure
        $ T.intercalate
            " · "
            [ "usta " <> T.pack (showVersion version)
            , if T.null osVersion then "macOS" else "macOS " <> osVersion
            , "GTK " <> T.pack (show major) <> "." <> T.pack (show minor)
            , "GHC " <> T.pack (showVersion compilerVersion)
            ]

-- | The newest crash report macOS wrote for usta: its name, path, size.
latestCrash :: IO (Maybe (Text, FilePath, Integer))
latestCrash = do
    folder <- (</> "Library/Logs/DiagnosticReports") <$> getHomeDirectory
    found <- try (listDirectory folder)
    case found of
        Left (_ :: SomeException) -> pure Nothing
        Right names -> do
            dated <- forM [n | n <- names, "usta" `T.isPrefixOf` T.pack n, ".ips" `T.isSuffixOf` T.pack n] $ \n -> do
                at <- getModificationTime (folder </> n)
                size <- getFileSize (folder </> n)
                pure (at, (T.pack n, folder </> n, size))
            pure (snd <$> safeHead (sortOn (Down . fst) dated))
    where
        safeHead (x : _) = Just x
        safeHead [] = Nothing

{- | "usta quit unexpectedly last time", with Not now and Report…; @report@
opens the dialog, and either button hides the banner.
-}
banner :: IO () -> IO Gtk.Widget
banner report = do
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["u-crash-banner"]]
    GI.new Gtk.Image [#iconName := "dialog-warning-symbolic"] >>= #append box
    texts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #hexpand := True]
    label "usta quit unexpectedly last time" ["title"] >>= #append texts
    label "Reporting it, with the log from then, helps fix it." [] >>= #append texts
    #append box texts
    later <- Button.new Button.defaults {Button.label = Just "Not now", Button.small = True}
    now <- Button.new Button.defaults {Button.label = Just "Report…", Button.small = True, Button.variant = Button.Primary}
    #append box later
    #append box now
    void $ GI.on later #clicked (GI.set box [#visible := False])
    void $ GI.on now #clicked (GI.set box [#visible := False] >> report)
    Gtk.toWidget box

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css =
    mconcat
        [ "window.u-report headerbar { background: var(--surface); border-bottom: 1px solid var(--border); box-shadow: none; }"
        , "window.u-report box.form { padding: 20px; background: var(--surface); }"
        , "window.u-report label.step { font-weight: 600; margin-top: 6px; }"
        , "window.u-report label.subtitle { font-size: 12px; color: var(--text-muted); }"
        , "window.u-report textview.happened { border: 1px solid var(--input-border); border-radius: 8px; padding: 8px; }"
        , "window.u-report box.attached { padding: 6px 10px; border: 1px solid var(--border); border-radius: 8px; }"
        , "window.u-report box.attached label.name { font-weight: 500; }"
        , "window.u-report label.preview { padding: 12px; border-radius: 8px; background: #1B1D21; color: #E8E9EC; font-family: 'JetBrains Mono', monospace; font-size: 12px; }"
        , "window.u-report box.footer { padding: 14px 20px; background: var(--window); border-top: 1px solid var(--border); }"
        , "window.u-report label.muted { color: var(--text-muted); font-size: 12px; }"
        , "box.u-crash-banner { margin: 10px 16px 0; padding: 10px 14px; border: 1px solid var(--danger); border-radius: 8px; background: var(--danger-bg); }"
        , "box.u-crash-banner label.title { font-weight: 600; color: var(--danger); }"
        , "box.u-crash-banner image { color: var(--danger); }"
        ]
