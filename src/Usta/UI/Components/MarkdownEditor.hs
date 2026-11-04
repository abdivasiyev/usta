{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A text area for Markdown with Write and Preview tabs and a toolbar,
-- used for descriptions and comments.
--
-- Each toolbar button wraps the selection, or inserts a placeholder and
-- selects it; line styles (quote, lists, heading) apply to every selected
-- line, and pressing one again takes it off.
module Usta.UI.Components.MarkdownEditor
  ( Config (..)
  , defaults
  , new
  , css
  ) where

import Control.Monad (forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)), set)
import Data.Char (isDigit)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Components.Shortcodes as Shortcodes

type Config :: Type
data Config = Config
  { text :: Text
  , hint :: Maybe Text
    -- ^ Shown under the editor, e.g. "Markdown supported".
  , height :: Int32
  , compact :: Bool
    -- ^ For a narrow box, such as the review panel: the toolbar on its own
    -- row under the tabs, with the less used tools under ⋯.
  }

defaults :: Config
defaults = Config { text = "", hint = Nothing, height = 120, compact = False }

-- | The editor and its buffer, to read what was written.
new :: Config -> IO (Gtk.Box, Gtk.TextBuffer)
new cfg = do
  box <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                        , #spacing := 6
                        , #cssClasses := ["u-markdown-editor"]
                        ]
  (tabs, buttons) <- Segmented.new ["Write", "Preview"] 0
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["header"]]
  #append header tabs
  #append box header

  buffer <- GI.new Gtk.TextBuffer []
  Gtk.textBufferSetText buffer cfg.text (-1)
  view <- GI.new Gtk.TextView [ #buffer := buffer
                              , #wrapMode := Gtk.WrapModeWordChar
                              , #heightRequest := cfg.height
                              , #topMargin := 12
                              , #bottomMargin := 12
                              , #leftMargin := 12
                              , #rightMargin := 12
                              , #cssClasses := ["editor"]
                              ]
  rendered <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #heightRequest := cfg.height
                             , #cssClasses := ["preview"]
                             ]
  Shortcodes.attach view
  stack <- GI.new Gtk.Stack []
  void $ Gtk.stackAddNamed stack view (Just "write")
  void $ Gtk.stackAddNamed stack rendered (Just "preview")
  #append box stack

  tools <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 2, #cssClasses := ["tools"]]
  more <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 2, #cssClasses := ["tools"]]
  morePopover <- GI.new Gtk.Popover [#child := more, #hasArrow := False, #cssClasses := ["u-more-tools"]]
  forM_ (zip [0 :: Int ..] toolGroups) $ \(i, group) -> do
    let hidden = cfg.compact && i > 0
        into = if hidden then more else tools
    unless (i == 0 || (cfg.compact && i == 1)) $
      GI.new Gtk.Separator [#orientation := Gtk.OrientationVertical] >>= #append into
    forM_ group $ \tool -> do
      b <- toolButton view buffer tool
      when hidden $ void $ GI.on b #clicked (Gtk.popoverPopdown morePopover)
      #append into b
  GI.new Gtk.Separator [#orientation := Gtk.OrientationVertical] >>= #append tools
  emojiButton view >>= #append tools
  when cfg.compact $ do
    moreLabel <- GI.new Gtk.Label [#label := "⋯"]
    moreButton <- GI.new Gtk.MenuButton [#child := moreLabel, #popover := morePopover, #tooltipText := "More tools", #cssClasses := ["tool"]]
    #append tools moreButton
  GI.new Gtk.Label [#label := "Markdown", #hexpand := True, #xalign := 1, #cssClasses := ["muted"]] >>= #append header
  if cfg.compact
    then GI.set header [#marginBottom := 0] >> Gtk.boxInsertChildAfter box tools (Just header)
    else Gtk.boxInsertChildAfter header tools (Just tabs)

  keys <- GI.new Gtk.ShortcutController [#propagationPhase := Gtk.PropagationPhaseCapture]
  forM_ (concat toolGroups) $ \tool ->
    forM_ (toolKey tool) $ \k -> shortcut keys k (editWith buffer (toolEdit tool))
  case buttons of
    [write, preview] -> shortcut keys "<Primary><Shift>p" $ do
      writing <- GI.get write #active
      set (if writing then preview else write) [#active := True]
    _ -> pure ()
  Gtk.widgetAddController box keys

  -- Preview renders the text as the forge will show it.
  forM_ (zip ["write", "preview"] buttons) $ \(name, button) ->
    void $ GI.on button #toggled $ do
      active <- GI.get button #active
      when active $ do
        when (name == "preview") $ do
          start <- Gtk.textBufferGetStartIter buffer
          end <- Gtk.textBufferGetEndIter buffer
          current <- Gtk.textBufferGetText buffer start end False
          Gtk.widgetGetFirstChild rendered >>= mapM_ (Gtk.boxRemove rendered)
          shown <- if T.null (T.strip current)
            then GI.new Gtk.Label [#label := "Nothing to preview.", #xalign := 0, #cssClasses := ["muted"]] >>= Gtk.toWidget
            else Markdown.new current
          #append rendered shown
        Gtk.stackSetVisibleChildName stack name
        set tools [#sensitive := name == "write"]
        when (name == "write") $ void (Gtk.widgetGrabFocus view)

  forM_ cfg.hint $ \h ->
    GI.new Gtk.Label [#label := h, #xalign := 0, #cssClasses := ["hint"]] >>= #append box
  pure (box, buffer)

-- * Toolbar

type Tool :: Type
data Tool = Heading | Bold | Italic | Code | Link | Quote | Bullets | Numbers | Tasks | CodeBlock | Diagram
  deriving stock (Eq, Show, Enum, Bounded)

toolGroups :: [[Tool]]
toolGroups = [[Heading, Bold, Italic, Code, Link], [Quote, Bullets, Numbers, Tasks], [CodeBlock, Diagram]]

toolName :: Tool -> Text
toolName tool = case tool of
  Heading -> "Heading"
  Bold -> "Bold"
  Italic -> "Italic"
  Code -> "Code"
  Link -> "Link"
  Quote -> "Quote"
  Bullets -> "Bulleted list"
  Numbers -> "Numbered list"
  Tasks -> "Task list"
  CodeBlock -> "Code block"
  Diagram -> "Mermaid diagram"

-- | The key, as GTK parses it; @<Primary>@ is Cmd on macOS.
toolKey :: Tool -> Maybe Text
toolKey tool = case tool of
  Bold -> Just "<Primary>b"
  Italic -> Just "<Primary>i"
  Code -> Just "<Primary>e"
  Link -> Just "<Primary>k"
  _ -> Nothing

toolIcon :: Tool -> Icon.Icon
toolIcon tool = case tool of
  Heading -> Icon.Heading
  Bold -> Icon.Bold
  Italic -> Icon.Italic
  Code -> Icon.Code
  Link -> Icon.Link
  Quote -> Icon.Quote
  Bullets -> Icon.Bullets
  Numbers -> Icon.Numbers
  Tasks -> Icon.Tasks
  CodeBlock -> Icon.CodeBlock
  Diagram -> Icon.Diagram

toolEdit :: Tool -> Text -> (Int, Int) -> Edit
toolEdit tool = case tool of
  Heading -> lines' HeadingLine
  Bold -> wrap "**" "**" "bold text"
  Italic -> wrap "_" "_" "italic text"
  Code -> code
  Link -> link
  Quote -> lines' QuoteLine
  Bullets -> lines' Bullet
  Numbers -> lines' Numbered
  Tasks -> lines' Task
  CodeBlock -> block "```\n" "code"
  Diagram -> block "```mermaid\n" "flowchart LR\n  a --> b"

-- | A flat button that does not take focus, so the selection stays.
toolButton :: Gtk.TextView -> Gtk.TextBuffer -> Tool -> IO Gtk.Button
toolButton view buffer tool = do
  image <- Icon.new 16 (toolIcon tool)
  let hint = toolName tool <> maybe "" (\k -> " (" <> keyName k <> ")") (toolKey tool)
  button <- GI.new Gtk.Button [#child := image, #focusOnClick := False, #tooltipText := hint, #cssClasses := ["tool"]]
  void $ GI.on button #clicked $ do
    editWith buffer (toolEdit tool)
    void (Gtk.widgetGrabFocus view)
  pure button
  where
    keyName k = "Ctrl " <> T.toUpper (T.takeWhileEnd (/= '>') k)

-- | Opens GTK's emoji chooser at the cursor, as @Ctrl .@ does in the
-- text: it searches, groups by category and keeps recently used ones.
emojiButton :: Gtk.TextView -> IO Gtk.Button
emojiButton view = do
  image <- Icon.new 16 Icon.Smile
  button <- GI.new Gtk.Button [#child := image, #focusOnClick := False, #tooltipText := "Emoji (Ctrl .)", #cssClasses := ["tool"]]
  void $ GI.on button #clicked $ do
    void (Gtk.widgetGrabFocus view)
    void (Gtk.widgetActivateAction view "misc.insert-emoji" Nothing)
  pure button

-- | Applies an edit to the text and selection the buffer has now.
editWith :: Gtk.TextBuffer -> (Text -> (Int, Int) -> Edit) -> IO ()
editWith buffer f = do
  start <- Gtk.textBufferGetStartIter buffer
  end <- Gtk.textBufferGetEndIter buffer
  whole <- Gtk.textBufferGetText buffer start end False
  (_, a, b) <- Gtk.textBufferGetSelectionBounds buffer
  from <- Gtk.textIterGetOffset a
  to <- Gtk.textIterGetOffset b
  let e = f whole (fromIntegral from, fromIntegral to)
  Gtk.textBufferBeginUserAction buffer
  x <- Gtk.textBufferGetIterAtOffset buffer (fromIntegral e.from)
  y <- Gtk.textBufferGetIterAtOffset buffer (fromIntegral e.to)
  Gtk.textBufferDelete buffer x y
  at <- Gtk.textBufferGetIterAtOffset buffer (fromIntegral e.from)
  Gtk.textBufferInsert buffer at e.new (-1)
  s <- Gtk.textBufferGetIterAtOffset buffer (fromIntegral (fst e.select))
  t <- Gtk.textBufferGetIterAtOffset buffer (fromIntegral (snd e.select))
  Gtk.textBufferSelectRange buffer s t
  Gtk.textBufferEndUserAction buffer

-- * Edits, on the text as characters

-- | Replace the characters from @from@ to @to@ with @new@, then select
-- @select@.
type Edit :: Type
data Edit = Edit { from :: Int, to :: Int, new :: Text, select :: (Int, Int) }

slice :: Int -> Int -> Text -> Text
slice a b = T.take (b - a) . T.drop a

-- | Puts @before@ and @after@ around the selection, or around a selected
-- placeholder; takes them off when they are there already.
wrap :: Text -> Text -> Text -> Text -> (Int, Int) -> Edit
wrap before after placeholder whole (s, e)
  | wrapped = Edit (s - lb) (e + la) chosen (s - lb, e - lb)
  | s == e = Edit s s (before <> placeholder <> after) (s + lb, s + lb + T.length placeholder)
  | otherwise = Edit s e (before <> chosen <> after) (s + lb, e + lb)
  where
    chosen = slice s e whole
    lb = T.length before
    la = T.length after
    wrapped = s /= e && before `T.isSuffixOf` T.take s whole && after `T.isPrefixOf` T.drop e whole

-- | Inline code, or a code block when the selection spans lines.
code :: Text -> (Int, Int) -> Edit
code whole (s, e)
  | "\n" `T.isInfixOf` slice s e whole = block "```\n" "code" whole (s, e)
  | otherwise = wrap "`" "`" "code" whole (s, e)

-- | @[text](url)@: the selection becomes the text and the URL is
-- selected, to type over.
link :: Text -> (Int, Int) -> Edit
link whole (s, e)
  | s == e = Edit s s "[text](url)" (s + 1, s + 5)
  | otherwise = Edit s e ("[" <> chosen <> "](url)") (end - 4, end - 1)
  where
    chosen = slice s e whole
    end = s + T.length chosen + 7

-- | A fenced block on lines of its own.
block :: Text -> Text -> Text -> (Int, Int) -> Edit
block fence placeholder whole (s, e) = wrap (lead <> fence) ("\n```" <> trail) placeholder whole (s, e)
  where
    lead = if s == 0 || T.takeEnd 1 (T.take s whole) == "\n" then "" else "\n"
    trail = if e == T.length whole || T.take 1 (T.drop e whole) == "\n" then "" else "\n"

type LineStyle :: Type
data LineStyle = QuoteLine | Bullet | Numbered | Task | HeadingLine

marker :: LineStyle -> Int -> Text
marker style i = case style of
  QuoteLine -> "> "
  Bullet -> "- "
  Numbered -> T.pack (show (i + 1)) <> ". "
  Task -> "- [ ] "
  HeadingLine -> "## "

-- | The line without its marker, when it has one.
unmark :: LineStyle -> Text -> Maybe Text
unmark Numbered line = case T.span isDigit line of
  (digits, rest) | not (T.null digits) -> T.stripPrefix ". " rest
  _ -> Nothing
unmark style line = T.stripPrefix (marker style 0) line

-- | Marks every line the selection touches, or unmarks them when all have
-- the marker already.
lines' :: LineStyle -> Text -> (Int, Int) -> Edit
lines' style whole (s, e) = Edit first last' changed selection
  where
    first = T.length (T.dropWhileEnd (/= '\n') (T.take s whole))
    -- A selection that ends at a line's start leaves that line alone.
    e' = if e > s && T.takeEnd 1 (T.take e whole) == "\n" then e - 1 else e
    last' = e' + T.length (T.takeWhile (/= '\n') (T.drop e' whole))
    old = T.splitOn "\n" (slice first last' whole)
    changed = T.intercalate "\n" $ case traverse (unmark style) old of
      Just plain -> plain
      Nothing -> zipWith (\i l -> marker style i <> l) [0 ..] old
    grown = T.length changed - (last' - first)
    selection
      | s == e = let c = max first (s + grown) in (c, c)
      | otherwise = (first, first + T.length changed)

-- | With the shortcode list's, which only the editor uses.
css :: Text
css = Shortcodes.css <> mconcat
  [ "box.u-markdown-editor textview.editor {"
  , "  border: 1px solid var(--input-border); border-radius: 0 8px 8px 8px;"
  , "  font-family: 'JetBrains Mono', monospace; font-size: 13px;"
  , "}"
  , "box.u-markdown-editor textview.editor text { background: var(--surface); color: var(--text); }"
  , "box.u-markdown-editor box.preview {"
  , "  padding: 12px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface);"
  , "}"
  , "box.u-markdown-editor box.tools button.tool {"
  , "  min-height: 28px; min-width: 28px; padding: 0; border-radius: 6px;"
  , "  background: transparent; border: none; box-shadow: none; color: var(--text-soft);"
  , "}"
  , "box.u-markdown-editor box.tools button.tool:hover { background: var(--border-subtle); color: var(--text); }"
  , "box.u-markdown-editor box.tools separator { margin: 6px 4px; }"
  , "popover.u-more-tools > contents { padding: 4px; border-radius: 10px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: 0 8px 24px rgba(27,29,33,0.14); }"
  , "popover.u-more-tools button.tool, box.u-markdown-editor menubutton.tool > button { min-height: 28px; min-width: 28px; padding: 0; border: none; box-shadow: none; background: transparent; background-image: none; }"
  , "box.u-markdown-editor box.header label.muted { font-size: 12px; color: var(--text-muted); }"
  , "box.u-markdown-editor label.hint { font-size: 12px; color: var(--text-muted); }"
  ]
