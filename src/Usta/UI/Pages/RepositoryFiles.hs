{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A repository's Files tab: the folders and files of its default branch
-- on the left, the chosen file on the right.
module Usta.UI.Pages.RepositoryFiles
  ( Place (..)
  , Actions (..)
  , Content (..)
  , contentOf
  , relativeTo
  , linkTarget
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (foldM, forM_, unless, void, when)
import qualified Data.ByteString as B
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import System.FilePath (takeExtension)
import Text.Read (readMaybe)

import qualified Usta.UI.Browser as Browser
import qualified Usta.UI.Components.FileIcon as FileIcon
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.CodeView as CodeView
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Diff as Diff
import qualified Usta.UI.Components.FileTools as FileTools
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.UI.Format (Clock)
import Usta.Types.Commit (Commit)
import Usta.Types.Insights (Blamed)
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
  , blame :: Text -> IO (Either Text [Blamed])
  , history :: Text -> IO (Either Text [Commit])
    -- ^ The commits on the branch that changed a file.
  , changeAt :: Commit -> Text -> IO (Either Text [Diff.File])
    -- ^ What a commit did to a file.
  , newest :: IO (Maybe Text)
    -- ^ The branch's newest commit, for permalinks.
  , clock :: IO Clock
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

-- | Where a link in the file at @from@ points: "../b.md" from "docs/a.md"
-- is "b.md", and "/x" starts at the top. 'Nothing' for a link to the
-- file's own headings, or one that climbs out of the repository.
relativeTo :: Text -> Text -> Maybe Text
relativeTo from link
  | T.null target = Nothing
  | otherwise = T.intercalate "/" <$> foldM step base (T.splitOn "/" target)
  where
    target = T.takeWhile (\c -> c /= '#' && c /= '?') link
    base = if "/" `T.isPrefixOf` target then [] else dropLast (T.splitOn "/" from)
    dropLast xs = take (length xs - 1) xs
    step acc part = case part of
      "" -> Just acc
      "." -> Just acc
      ".." -> if null acc then Nothing else Just (dropLast acc)
      p -> Just (acc <> [p])

-- | 'relativeTo' keeping the heading a link names: "../b.md#usage" from
-- "docs/a.md" is "b.md#usage".
linkTarget :: Text -> Text -> Maybe Text
linkTarget from link = (<> anchor) <$> relativeTo from link
  where
    anchor = case T.breakOn "#" link of
      (_, a) | T.length a > 1 -> a
      _ -> ""

-- | The tree and a file viewer; @start@ is the file or folder to open
-- first, as "path" or "path#heading", the README otherwise.
new :: Actions -> Place -> Maybe Text -> IO Gtk.Widget
new actions place start = do
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True, #cssClasses := ["u-files-tab"]]

  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #widthRequest := 280
                         , #hexpand := False
                         , #cssClasses := ["tree"]
                         ]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["tree-top"]]
  label place.branch ["chip", "mono"] >>= #append top
  find <- GI.new Gtk.SearchEntry [#placeholderText := "Go to file  (Ctrl P)", #hexpand := True]
  -- Ctrl P goes to a file by its name.
  goKeys <- GI.new Gtk.ShortcutController [#scope := Gtk.ShortcutScopeManaged]
  shortcut goKeys "<Primary>p" (void (Gtk.widgetGrabFocus find))
  Gtk.widgetAddController page goKeys
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
      showFile e anchor = do
        replace viewer =<< Skeleton.detail
        void $ forkIO $ do
          loaded <- actions.loadFile e.path
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            widget <- case loaded of
              Left problem -> note problem
              Right bytes -> fileView actions (mapM_ reveal . linkTarget e.path) anchor place e bytes
            replace viewer widget
            pure False
      -- Opens a file, or a folder in the tree, as a link to it does; a
      -- Markdown file scrolls to the heading after "#".
      reveal link = do
        let (path, anchor) = case T.breakOn "#" link of
              (p, a) -> (p, if T.length a > 1 then Just (T.drop 1 a) else Nothing)
        es <- readIORef entries
        case [e | e <- es, e.path == path] of
          e : _ -> do
            modifyIORef' open (\s -> foldr Set.insert s (parents path <> [path | e.kind == Tree.Folder]))
            Gtk.editableSetText find ""
            writeIORef query ""
            redraw
            rows <- shownRows
            forM_ (lookup path (zip (map (.path) rows) [0 ..])) $ \i ->
              Gtk.listBoxGetRowAtIndex list i >>= Gtk.listBoxSelectRow list
            when (e.kind == Tree.File) (showFile e anchor)
          [] -> note ("There is no " <> path <> " on " <> place.branch <> ".") >>= replace viewer
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
        Tree.File -> showFile e Nothing
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
          -- What was asked for, or the README, as the forge's page does.
          case (start, [e | e <- es, e.kind == Tree.File, T.toLower e.path `elem` ["readme.md", "readme", "readme.txt"]]) of
            (Just path, _) -> reveal path
            (Nothing, e : _) -> showFile e Nothing
            (Nothing, []) -> note "Choose a file on the left." >>= replace viewer
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
  icon <- if e.kind == Tree.Folder then FileIcon.folder 16 (expanded && not flat) e.path else FileIcon.file 16 e.path
  #append row icon
  label name ["mono"] >>= #append row
  pure row

-- | The header and the content for one file.
-- | @anchor@ is the heading a Markdown file opens at. A text file has Code,
-- Blame and History, a find bar (Ctrl F) and Copy permalink.
fileView :: Actions -> Markdown.Follow -> Maybe Text -> Place -> Entry -> B.ByteString -> IO Gtk.Widget
fileView actions follow anchor place e bytes = do
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
      copy <- Button.new Button.defaults { Button.label = Just "Copy", Button.small = True }
      void $ GI.on copy #clicked $ do
        clipboard <- Gtk.widgetGetClipboard copy
        provider <- GI.toGValue (Just text) >>= Gdk.contentProviderNewForValue
        void (Gdk.clipboardSetContent clipboard (Just provider))
      #append header copy
      permalink <- Button.new Button.defaults {Button.label = Just "Copy permalink", Button.small = True, Button.tooltip = Just "A link to the file at the branch's newest commit"}
      void $ GI.on permalink #clicked $ Button.busy permalink ([] :: [Gtk.Button]) "Copying…" actions.newest $ \case
        Nothing -> pure ()
        Just sha -> do
          clipboard <- Gtk.widgetGetClipboard permalink
          provider <- GI.toGValue (Just (permalinkOf place sha e.path)) >>= Gdk.contentProviderNewForValue
          void (Gdk.clipboardSetContent clipboard (Just provider))
      #append header permalink
    _ -> pure ()
  browser <- Button.new Button.defaults { Button.label = Just "Open in browser", Button.small = True }
  void $ GI.on browser #clicked $ do
    Browser.open ((webUrl place e.path))
  #append header browser
  #append box header
  -- Clipped, so the file's own background keeps the card's round corners.
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #overflow := Gtk.OverflowHidden, #cssClasses := ["u-card", "file-card"]]
  -- Code, Blame and History for a text file; the find bar searches the
  -- code.
  views <- GI.new Gtk.Stack []
  findSlot <- newIORef Nothing
  -- Under the file's name: Code, Blame and History, and Source.
  toolsRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["file-tools"]]
  case content of
    Text _ -> do
      (choices, choiceButtons) <- Segmented.new ["Code", "Blame", "History"] 0
      GI.set choices [#valign := Gtk.AlignCenter]
      #append toolsRow choices
      built <- newIORef (False, False)
      forM_ (zip [0 :: Int ..] choiceButtons) $ \(i, b) -> void $ GI.on b #toggled $ do
        active <- GI.get b #active
        when active $ do
          (blamed, histories) <- readIORef built
          c <- actions.clock
          when (i == 1 && not blamed) $ do
            writeIORef built (True, histories)
            FileTools.blameView c (actions.blame e.path) >>= \w -> void (Gtk.stackAddNamed views w (Just "blame"))
          when (i == 2 && not histories) $ do
            writeIORef built (blamed, True)
            FileTools.historyView c (actions.history e.path) (\x -> actions.changeAt x e.path) >>= \w -> void (Gtk.stackAddNamed views w (Just "history"))
          Gtk.stackSetVisibleChildName views (case i of 1 -> "blame"; 2 -> "history"; _ -> "code")
    _ -> pure ()
  body <- case content of
    Text text | markdown -> do
      stack <- GI.new Gtk.Stack []
      rendered <- Markdown.newAt follow (if isJust line then Nothing else anchor) text
      Gtk.widgetAddCssClass rendered "rendered"
      void $ Gtk.stackAddNamed stack rendered (Just "rendered")
      (source, view) <- CodeView.withView e.path text
      writeIORef findSlot (Just view)
      void $ Gtk.stackAddNamed stack source (Just "source")
      toggle <- GI.new Gtk.ToggleButton [#label := "Source", #valign := Gtk.AlignCenter, #cssClasses := ["u-toggle"]]
      void $ GI.on toggle #toggled $ do
        on <- GI.get toggle #active
        Gtk.stackSetVisibleChildName stack (if on then "source" else "rendered")
      #append toolsRow toggle
      forM_ line $ \n -> GI.set toggle [#active := True] >> goToLine view n
      Gtk.toWidget stack
    Text text -> do
      (source, view) <- CodeView.withView e.path text
      writeIORef findSlot (Just view)
      forM_ line (goToLine view)
      pure source
    Image -> do
      image <- ImageView.texture bytes
      case image of
        Just t -> ImageView.new t
        Nothing -> note "This image is in a format usta cannot show."
    Pdf -> PdfView.new e.path bytes >>= either note pure
    TooLarge -> note "Too large to show here; open it in the browser."
    _ -> note "Binary file not shown."
  void $ Gtk.stackAddNamed views body (Just "code")
  hasTools <- Gtk.widgetGetFirstChild toolsRow
  when (isJust hasTools) $ Gtk.boxInsertChildAfter box toolsRow (Just header)
  -- Ctrl F opens the find bar over the code.
  overlay <- GI.new Gtk.Overlay [#child := views]
  readIORef findSlot >>= mapM_ (\view -> do
    (bar, open) <- FileTools.findBar view
    Gtk.overlayAddOverlay overlay bar
    keys <- GI.new Gtk.ShortcutController [#scope := Gtk.ShortcutScopeManaged]
    shortcut keys "<Primary>f" (Gtk.stackSetVisibleChildName views "code" >> open)
    Gtk.widgetAddController box keys)
  #append card overlay
  #append box card
  Gtk.toWidget box
  where
    markdown = any (`T.isSuffixOf` T.toLower e.path) [".md", ".markdown"]
    -- "L12", as a forge link's #L12 asks.
    line = anchor >>= T.stripPrefix "L" >>= readMaybe . T.unpack
    linesOf content = case content of
      Text text -> [T.pack (show (length (T.lines text))) <> " lines"]
      _ -> []

-- | Scrolls to line @n@ (from 1) and selects it; a mark scrolls once the
-- view is laid out.
goToLine :: Gtk.TextView -> Int -> IO ()
goToLine view n = do
  buffer <- Gtk.textViewGetBuffer view
  start <- Gtk.textBufferGetIterAtLine buffer (fromIntegral (n - 1)) >>= \(_, it) -> pure it
  end <- Gtk.textIterCopy start
  void (Gtk.textIterForwardToLineEnd end)
  Gtk.textBufferSelectRange buffer start end
  mark <- Gtk.textBufferCreateMark buffer Nothing start True
  Gtk.textViewScrollToMark view mark 0 True 0 0.3

-- | Bytes as people read them: "870 B", "6.1 KB", "1.4 MB".
size :: Int -> Text
size n
  | n < 1024 = T.pack (show n) <> " B"
  | n < 1024 * 1024 = one (fromIntegral n / 1024) <> " KB"
  | otherwise = one (fromIntegral n / (1024 * 1024)) <> " MB"
  where
    one :: Double -> Text
    one x = T.pack (show (fromIntegral (round (x * 10) :: Int) / 10 :: Double))

-- | A link to a file at a commit, which stays true as the branch moves.
permalinkOf :: Place -> Text -> Text -> Text
permalinkOf place sha path = "https://" <> place.host <> "/" <> place.repo <> middle <> sha <> "/" <> path
  where
    middle = case place.forge of
      Forge.GitHub -> "/blob/"
      Forge.GitLab -> "/-/blob/"
      Forge.Forgejo -> "/src/commit/"

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
  Skeleton.appear widget
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
  , "box.u-files-tab label.note { padding: 24px; }"
  , "box.u-files-tab .rendered { padding: 20px 24px; }"
  ]
