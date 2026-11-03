{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A repository's Files tab: the folders and files of its default branch
-- on the left, the chosen file on the right.
module Usta.UI.Pages.RepositoryFiles
  ( Place (..)
  , Actions (..)
  , Content (..)
  , contentOf
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as B
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.List (sortOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import System.FilePath (takeExtension)

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.CodeView as CodeView
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.ImageView as ImageView
import qualified Usta.UI.Components.PdfView as PdfView
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.Skeleton as Skeleton
import qualified Usta.UI.Highlight as Highlight
import Usta.Types.Tree (Entry (..))
import qualified Usta.Types.Tree as Tree

-- | The repository and branch shown.
type Place :: Type
data Place = Place
  { forge :: Forge.Kind
  , host :: Text
  , repo :: Text
  , branch :: Text
  }

-- | Each runs off the main thread.
type Actions :: Type
data Actions = Actions
  { loadTree :: IO (Either Text (Text, [Entry]))
    -- ^ The entries, with where they came from, e.g. "From the local mirror".
  , loadFile :: Text -> IO (Either Text B.ByteString)
  }

-- | What a file holds, as the viewer shows it.
type Content :: Type
data Content
  = Text Text
  | Image
  | Pdf
  | Binary
  | TooLarge
  deriving stock (Eq, Show)

-- | Text when it decodes as UTF-8 with no NUL in its first 8 KB;
-- images and PDFs by their names.
contentOf :: Text -> B.ByteString -> Content
contentOf path bytes
  | extension `elem` [".png", ".jpg", ".jpeg", ".gif", ".webp", ".bmp", ".ico", ".svg"] = Image
  | extension == ".pdf" = Pdf
  | B.length bytes > 2 * 1024 * 1024 = TooLarge
  | B.elem 0 (B.take 8192 bytes) = Binary
  | otherwise = either (const Binary) Text (decodeUtf8' bytes)
  where
    extension = map lower (takeExtension (T.unpack path))
    lower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

new :: Actions -> Place -> IO Gtk.Widget
new actions place = do
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True, #cssClasses := ["u-files-tab"]]

  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #widthRequest := 280
                         , #hexpand := False
                         , #cssClasses := ["tree"]
                         ]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["tree-top"]]
  label place.branch ["chip", "mono"] >>= #append top
  find <- GI.new Gtk.SearchEntry [#placeholderText := "Go to file", #hexpand := True]
  #append top find
  #append side top
  list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle, #cssClasses := ["tree-list"]]
  listScroll <- GI.new Gtk.ScrolledWindow [#child := list, #vexpand := True, #hscrollbarPolicy := Gtk.PolicyTypeNever]
  #append side listScroll
  #append page side

  viewer <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #hexpand := True, #cssClasses := ["viewer"]]
  viewerScroll <- GI.new Gtk.ScrolledWindow [#child := viewer, #hexpand := True, #vexpand := True]
  #append page viewerScroll

  entries <- newIORef ([] :: [Entry])
  open <- newIORef (Set.empty :: Set.Set Text)
  query <- newIORef ""
  let shownRows = do
        es <- readIORef entries
        expanded <- readIORef open
        q <- T.toLower <$> readIORef query
        pure $ if T.null q
          then [e | e <- es, all (`Set.member` expanded) (parents e.path)]
          else [e | e <- es, e.kind == Tree.File, q `T.isInfixOf` T.toLower e.path]
      redraw = do
        rows <- shownRows
        q <- readIORef query
        expanded <- readIORef open
        Gtk.listBoxRemoveAll list
        forM_ rows $ \e -> do
          row <- treeRow (not (T.null q)) (Set.member e.path expanded) e
          #append list row
      showFile e = do
        replace viewer =<< Skeleton.detail
        void $ forkIO $ do
          loaded <- actions.loadFile e.path
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            widget <- case loaded of
              Left problem -> note problem
              Right bytes -> fileView place e bytes
            replace viewer widget
            pure False
  void $ GI.on list #rowActivated $ \row -> do
    index <- fromIntegral <$> Gtk.listBoxRowGetIndex row
    rows <- shownRows
    when (index < length rows) $ do
      let e = rows !! index
      case e.kind of
        Tree.Folder -> do
          expanded <- readIORef open
          writeIORef open (if Set.member e.path expanded then Set.delete e.path expanded else Set.insert e.path expanded)
          redraw
        Tree.File -> showFile e
  void $ GI.on find #searchChanged $ do
    Gtk.editableGetText find >>= writeIORef query
    redraw

  replace viewer =<< Skeleton.detail
  void $ forkIO $ do
    loaded <- actions.loadTree
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      case loaded of
        Left problem -> note problem >>= replace viewer
        Right (_, es) -> do
          writeIORef entries (ordered es)
          redraw
          -- The README first, as the forge's page does.
          case [e | e <- es, e.kind == Tree.File, T.toLower e.path `elem` ["readme.md", "readme", "readme.txt"]] of
            e : _ -> showFile e
            [] -> note "Choose a file on the left." >>= replace viewer
      pure False
  Gtk.toWidget page
  where
    parents path = init' (map (T.intercalate "/") (drop 1 (inits' (T.splitOn "/" path))))
    inits' xs = [take n xs | n <- [0 .. length xs]]
    init' xs = if null xs then [] else take (length xs - 1) xs

-- | Folders before files in each folder, both by name, each folder's
-- contents right after it.
ordered :: [Entry] -> [Entry]
ordered es = walk ""
  where
    walk dir =
      concat [e : (if e.kind == Tree.Folder then walk e.path else []) | e <- sortOn rank (childrenOf dir)]
    childrenOf dir = [e | e <- es, parentOf e.path == dir]
    parentOf path = T.intercalate "/" (initOf (T.splitOn "/" path))
    initOf xs = take (length xs - 1) xs
    rank e = (e.kind /= Tree.Folder, T.toLower e.path)

treeRow :: Bool -> Bool -> Entry -> IO Gtk.Box
treeRow flat expanded e = do
  let depth = if flat then 0 else length (T.splitOn "/" e.path) - 1
      name = if flat then e.path else last (T.splitOn "/" e.path)
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #marginStart := fromIntegral (8 + 16 * depth)]
  -- A folder's chevron says whether it is open; files line up after it.
  chevron <- GI.new Gtk.Image [ #iconName := if expanded then "pan-down-symbolic" else "pan-end-symbolic"
                              , #opacity := if e.kind == Tree.Folder && not flat then 1 else 0
                              , #cssClasses := ["chevron"]
                              ]
  #append row chevron
  icon <- GI.new Gtk.Image [ #iconName := if e.kind == Tree.Folder then "folder-symbolic" else "text-x-generic-symbolic"
                           , #cssClasses := [if e.kind == Tree.Folder then "folder" else "file"]
                           ]
  #append row icon
  label name ["mono"] >>= #append row
  pure row

-- | The header and the content for one file.
fileView :: Place -> Entry -> B.ByteString -> IO Gtk.Widget
fileView place e bytes = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12]
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["file-header"]]
  let (folder, name) = T.breakOnEnd "/" e.path
      content = contentOf e.path bytes
  unless (T.null folder) $ label folder ["mono", "muted"] >>= #append header
  label name ["mono", "strong"] >>= #append header
  forM_ (Highlight.languageFor (T.unpack e.path)) $ \l -> do
    badge <- label (Highlight.languageName l) ["language"]
    GI.set badge [#valign := Gtk.AlignCenter]
    #append header badge
  dimensions <- case content of
    Image -> ImageView.texture bytes >>= traverse ImageView.size
    _ -> pure Nothing
  let kind = case content of
        Image -> [T.toUpper (T.drop 1 (T.pack (takeExtension (T.unpack e.path))))]
        _ -> []
      pixels = [T.pack (show w) <> " × " <> T.pack (show h) | Just (w, h) <- [dimensions]]
  facts <- label (T.intercalate " · " (kind <> linesOf content <> pixels <> [size (B.length bytes)])) ["muted"]
  GI.set facts [#hexpand := True]
  #append header facts
  case content of
    Text text -> do
      copy <- Button.new Button.defaults { Button.label = Just "Copy" }
      void $ GI.on copy #clicked $ do
        clipboard <- Gtk.widgetGetClipboard copy
        provider <- GI.toGValue (Just text) >>= Gdk.contentProviderNewForValue
        void (Gdk.clipboardSetContent clipboard (Just provider))
      #append header copy
    _ -> pure ()
  browser <- Button.new Button.defaults { Button.label = Just "Open in browser" }
  void $ GI.on browser #clicked $ do
    launcher <- Gtk.uriLauncherNew (Just (webUrl place e.path))
    Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
  #append header browser
  #append box header
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-card", "file-card"]]
  body <- case content of
    Text text | markdown -> do
      stack <- GI.new Gtk.Stack []
      rendered <- Markdown.new text
      Gtk.widgetAddCssClass rendered "rendered"
      void $ Gtk.stackAddNamed stack rendered (Just "rendered")
      source <- CodeView.new e.path text
      void $ Gtk.stackAddNamed stack source (Just "source")
      toggle <- GI.new Gtk.ToggleButton [#label := "Source"]
      void $ GI.on toggle #toggled $ do
        on <- GI.get toggle #active
        Gtk.stackSetVisibleChildName stack (if on then "source" else "rendered")
      Gtk.boxInsertChildAfter header toggle (Just facts)
      Gtk.toWidget stack
    Text text -> CodeView.new e.path text
    Image -> do
      image <- ImageView.texture bytes
      case image of
        Just t -> ImageView.new t
        Nothing -> note "This image is in a format usta cannot show."
    Pdf -> PdfView.new e.path bytes >>= either note pure
    TooLarge -> note "Too large to show here; open it in the browser."
    _ -> note "Binary file not shown."
  #append card body
  #append box card
  Gtk.toWidget box
  where
    markdown = any (`T.isSuffixOf` T.toLower e.path) [".md", ".markdown"]
    linesOf content = case content of
      Text text -> [T.pack (show (length (T.lines text))) <> " lines"]
      _ -> []

-- | Bytes as people read them: "870 B", "6.1 KB", "1.4 MB".
size :: Int -> Text
size n
  | n < 1024 = T.pack (show n) <> " B"
  | n < 1024 * 1024 = one (fromIntegral n / 1024) <> " KB"
  | otherwise = one (fromIntegral n / (1024 * 1024)) <> " MB"
  where
    one :: Double -> Text
    one x = T.pack (show (fromIntegral (round (x * 10) :: Int) / 10 :: Double))

webUrl :: Place -> Text -> Text
webUrl place path = "https://" <> place.host <> "/" <> place.repo <> middle <> place.branch <> "/" <> path
  where
    middle = case place.forge of
      Forge.GitHub -> "/blob/"
      Forge.GitLab -> "/-/blob/"
      Forge.Forgejo -> "/src/branch/"

replace :: Gtk.Box -> Gtk.Widget -> IO ()
replace box widget = do
  let clear = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear)
  clear
  #append box widget

note :: Text -> IO Gtk.Widget
note text = label text ["muted", "note"] >>= Gtk.toWidget

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-files-tab box.tree { background: var(--surface); border-right: 1px solid var(--border); }"
  , "box.u-files-tab box.tree-top { padding: 10px 12px; }"
  , "box.u-files-tab list.tree-list { background: var(--surface); }"
  , "box.u-files-tab list.tree-list row { min-height: 30px; padding: 0 8px 0 0; }"
  , "box.u-files-tab list.tree-list row:selected {"
  , "  background: var(--selected); color: var(--text); box-shadow: inset 3px 0 0 var(--accent);"
  , "}"
  , "box.u-files-tab image.folder { color: var(--warning); }"
  , "box.u-files-tab image.file { color: var(--text-muted); }"
  , "box.u-files-tab label.chip {"
  , "  background: var(--neutral-bg); padding: 2px 8px; border-radius: 10px; font-size: 12px;"
  , "}"
  , "box.u-files-tab box.viewer { padding: 20px 24px; }"
  , "box.u-files-tab box.file-header { font-size: 13px; }"
  , "box.u-files-tab label.strong { font-weight: 600; }"
  , "box.u-files-tab image.chevron { color: var(--text-muted); }"
  , "box.u-files-tab box.file-header > * { margin-top: 0; }"
  , "box.u-files-tab label.language {"
  , "  background: var(--accent-bg); color: var(--accent-soft); padding: 1px 8px; border-radius: 10px; font-size: 12px;"
  , "}"
  , "box.u-files-tab box.file-card { overflow: hidden; }"
  , "box.u-files-tab label.note { padding: 24px; }"
  , "box.u-files-tab .rendered { padding: 20px 24px; }"
  ]
