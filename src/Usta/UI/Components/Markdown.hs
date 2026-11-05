{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Markdown as native widgets: GitHub-flavored (tables, strikethrough,
-- autolinks, task lists) through cmark-gfm. Code blocks are highlighted
-- like files; links with a scheme open in the browser, and a link to a
-- heading, "#usage", scrolls to it. Long words and links break where the
-- line ends; code blocks and tables scroll sideways on their own. A
-- "diff" block is drawn as a diff, and a "suggestion" block as the change
-- a reviewer suggests.
--
-- Raw HTML is shown as forges show it, within the safe part they allow:
-- inline styling (b, i, s, u, sub, sup, small, mark, code, kbd, br, a,
-- img), and blocks (details with a summary, centred p and div, h1 to h6,
-- hr). Other tags are left out, never shown as text.
module Usta.UI.Components.Markdown
  ( Follow
  , new
  , newIn
  , newAt
  , suggesting
  , openLinksWith
  , slug
  , css
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import System.IO.Unsafe (unsafePerformIO)
import CMarkGFM (ListAttributes (..), ListType (..), Node (..), NodeType (..), TableCellAlignment (..), commonmarkToNode, extAutolink, extStrikethrough, extTable)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Graphene as Graphene
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Scroll as Scroll
import qualified Usta.UI.Components.Mermaid as Mermaid
import qualified Usta.UI.Highlight as Highlight

-- | What a link without a scheme, such as "docs/a.md" or "#usage",
-- opens: it means something only where the text came from.
type Follow :: Type
type Follow = Text -> IO ()

-- | @text@ rendered, for a description or a comment; links without a
-- scheme do nothing.
new :: Text -> IO Gtk.Widget
new = newIn (const (pure ()))

-- | @text@ rendered, with links without a scheme going to @follow@, as a
-- file's links to the files beside it.
newIn :: Follow -> Text -> IO Gtk.Widget
newIn follow = newAt follow Nothing

-- | A review comment on @replaced@, the lines it was left on: its
-- suggestions show those lines going and the suggested ones coming.
suggesting :: [Text] -> Text -> IO Gtk.Widget
suggesting replaced = build (const (pure ())) Nothing replaced

-- | Like 'newIn', scrolled to the heading @anchor@ names once it shows,
-- as a link "a.md#usage" from another file asks.
newAt :: Follow -> Maybe Text -> Text -> IO Gtk.Widget
newAt follow anchor = build follow anchor []

build :: Follow -> Maybe Text -> [Text] -> Text -> IO Gtk.Widget
build follow anchor replaced text = do
  let root = commonmarkToNode [] [extTable, extStrikethrough, extAutolink] text
  anchors <- newIORef []
  let doc = Doc {follow, diagrams = diagramsOf root, anchors, replaced}
  w <- block doc root
  Gtk.widgetAddCssClass w "u-markdown"
  -- The heading has its place only once the page is laid out, and wrapped
  -- text settles over a few frames: this follows it for up to two seconds,
  -- until it stays where it was.
  forM_ anchor $ \a -> void $ GI.on w #map $ do
    tries <- newIORef (0 :: Int)
    last' <- newIORef Nothing
    void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 50 $ do
      at <- jump doc a
      before <- readIORef last'
      writeIORef last' at
      modifyIORef' tries (+ 1)
      n <- readIORef tries
      pure (n < 40 && (at == Nothing || at /= before))
  pure w

-- | What the blocks of one document share.
type Doc :: Type
data Doc = Doc
  { follow :: Follow
  , diagrams :: [Mermaid.Diagram]
    -- ^ Its Mermaid diagrams in order, for fullscreen to move between.
  , anchors :: IORef [(Text, Gtk.Widget)]
    -- ^ Each heading by its slug, in order.
  , replaced :: [Text]
    -- ^ The lines a review comment is on, which its suggestions replace;
    -- none elsewhere.
  }

-- | A heading's anchor as forges make it: lowercase, spaces to hyphens,
-- other punctuation dropped; "Getting started!" is "getting-started".
slug :: Text -> Text
slug = T.map (\c -> if isSpace c then '-' else c) . T.filter keep . T.toLower . T.strip
  where
    keep c = isAlphaNum c || isSpace c || c == '-' || c == '_'

-- | Scrolls the nearest scrolled window to the heading @name@ names; a
-- second heading of the same text is "@name@-1", as forges number them.
-- Where it scrolled to; 'Nothing' while the page is not laid out.
jump :: Doc -> Text -> IO (Maybe Double)
jump doc name = do
  headings <- readIORef doc.anchors
  let wanted = T.toLower (T.replace "%20" "-" name)
  case lookup wanted headings of
    Nothing -> pure (Just 0)
    Just heading -> scrollerOf heading >>= \case
      Nothing -> pure (Just 0)
      Just scroller -> Gtk.scrolledWindowGetChild scroller >>= \case
        Nothing -> pure (Just 0)
        Just child -> do
          origin <- Graphene.newZeroPoint
          (placed, at) <- Gtk.widgetComputePoint heading child origin
          y <- realToFrac <$> Graphene.getPointY at
          adjustment <- Gtk.scrolledWindowGetVadjustment scroller
          room <- (-) <$> Gtk.adjustmentGetUpper adjustment <*> Gtk.adjustmentGetPageSize adjustment
          let target = max 0 (y - 12)
          -- No room yet means the page is not laid out.
          if placed && room > 0
            then Gtk.adjustmentSetValue adjustment target >> pure (Just (min room target))
            else pure Nothing
  where
    scrollerOf w = Gtk.widgetGetParent w >>= \case
      Nothing -> pure Nothing
      Just parent -> GI.castTo Gtk.ScrolledWindow parent >>= maybe (scrollerOf parent) (pure . Just)

-- | The document's diagrams, each named after the heading above it.
diagramsOf :: Node -> [Mermaid.Diagram]
diagramsOf root = go "Diagram" (everything root)
  where
    everything n@(Node _ _ children) = n : concatMap everything children
    go title nodes = case nodes of
      [] -> []
      Node _ (HEADING _) children : rest -> go (plainOf (foldMap inline children)) rest
      Node _ (CODE_BLOCK info code) _ : rest
        | isMermaid info -> Mermaid.Diagram {title, source = code} : go title rest
      _ : rest -> go title rest

isMermaid :: Text -> Bool
isMermaid info = T.toLower (T.strip info) == "mermaid"

block :: Doc -> Node -> IO Gtk.Widget
block doc (Node _ kind children) = case kind of
  DOCUMENT -> column doc 12 children
  PARAGRAPH -> paragraph doc ["paragraph"] children
  HEADING level -> do
    w <- paragraph doc ["heading", "h" <> T.pack (show (min 6 level))] children
    let base = slug (plainOf (foldMap inline children))
    modifyIORef' doc.anchors $ \known ->
      let taken = length [() | (k, _) <- known, k == base || T.isPrefixOf (base <> "-") k && T.all (`elem` ['0' .. '9']) (T.drop (T.length base + 1) k)]
      in known <> [(if taken == 0 then base else base <> "-" <> T.pack (show taken), w)]
    pure w
  BLOCK_QUOTE -> do
    w <- column doc 8 children
    Gtk.widgetAddCssClass w "quote"
    pure w
  THEMATIC_BREAK -> GI.new Gtk.Separator [#orientation := Gtk.OrientationHorizontal] >>= Gtk.toWidget
  CODE_BLOCK info code
    | isMermaid info -> Mermaid.new doc.diagrams code
    | language `elem` ["diff", "patch"] -> diffBlock language Nothing (T.lines (T.stripEnd code))
    | language == "suggestion" -> diffBlock "Suggested change" (Just code)
        (map ("-" <>) doc.replaced <> map ("+" <>) (T.lines (T.stripEnd code)))
    | otherwise -> codeBlock language code
    where language = T.toLower (T.takeWhile (/= ' ') (T.strip info))
  HTML_BLOCK raw -> htmlBlock doc raw
  LIST attributes -> list doc attributes children
  TABLE alignments -> table doc alignments children
  _ -> column doc 8 children

-- | The blocks one under another; a <details> opened in one HTML block and
-- closed in a later one holds the blocks between.
column :: Doc -> Int -> [Node] -> IO Gtk.Widget
column doc gap children = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := fromIntegral gap]
  let go nodes = case nodes of
        [] -> pure ()
        Node _ (HTML_BLOCK raw) _ : rest | opens "details" raw && not (closes "details" raw) -> do
          let (inside, after) = untilClosed (1 :: Int) rest
          details doc raw inside >>= #append box
          go after
        n : rest -> block doc n >>= #append box >> go rest
      untilClosed depth nodes = case nodes of
        [] -> ([], [])
        n@(Node _ (HTML_BLOCK raw) _) : rest
          | closes "details" raw && depth == 1 -> ([], rest)
          | closes "details" raw -> first' n (untilClosed (depth - 1) rest)
          | opens "details" raw -> first' n (untilClosed (depth + 1) rest)
        n : rest -> first' n (untilClosed depth rest)
      first' n (a, b) = (n : a, b)
  go children
  Gtk.toWidget box

-- | A collapsible section: the summary from @raw@, which opened it, and
-- @inside@ under it.
details :: Doc -> Text -> [Node] -> IO Gtk.Widget
details doc raw inside = do
  let summary = T.strip (fst (T.breakOn "</summary>" (snd (T.breakOn ">" (snd (T.breakOn "<summary" raw))))))
      title = if T.null summary then "Details" else T.drop 1 summary
      rest = T.strip (snd (T.breakOn "</summary>" raw))
  body <- column doc 8 ([Node Nothing (HTML_BLOCK (T.drop 10 rest)) [] | T.length rest > 10] <> inside)
  GI.set body [#marginStart := 20, #marginTop := 6]
  expander <- GI.new Gtk.Expander [#useMarkup := True, #label := htmlMarkup title, #child := body, #cssClasses := ["details"]]
  Gtk.toWidget expander

opens, closes :: Text -> Text -> Bool
opens tag raw = ("<" <> tag) `T.isInfixOf` T.toLower raw
closes tag raw = ("</" <> tag <> ">") `T.isInfixOf` T.toLower raw

-- | An HTML block as a paragraph of its inline markup: a heading for h1
-- to h6, centred when it asks, a rule for hr, nothing for a comment.
htmlBlock :: Doc -> Text -> IO Gtk.Widget
htmlBlock doc raw
  | opens "details" raw = details doc raw []
  | all isRule ps && any isHr ps = GI.new Gtk.Separator [#orientation := Gtk.OrientationHorizontal] >>= Gtk.toWidget
  | T.null (T.strip (htmlMarkup raw)) = GI.new Gtk.Box [] >>= Gtk.toWidget
  | otherwise = do
      w <- text' doc (htmlMarkup raw) (heading <> ["paragraph"])
      when centred $ GI.set w [#halign := Gtk.AlignCenter]
      pure w
  where
    ps = pieces raw
    isHr p = case p of
      Tag {name = "hr"} -> True
      _ -> False
    isRule p = case p of
      Words w -> T.null (T.strip w)
      _ -> True
    heading = concat (take 1 [["heading", n] | Tag {name = n, closing = False} <- ps, n `elem` ["h1", "h2", "h3", "h4", "h5", "h6"]])
    centred = or [n == "center" || lookup "align" a == Just "center" | Tag {name = n, closing = False, attributes = a} <- ps]

paragraph :: Doc -> [Text] -> [Node] -> IO Gtk.Widget
paragraph doc classes children = text' doc (foldMap inline children) classes

-- | Selectable, wrapping text from Pango markup; markup that does not
-- parse, as from HTML left unclosed, shows as its plain text.
text' :: Doc -> Text -> [Text] -> IO Gtk.Widget
text' doc markup classes = do
  -- Links are the label's own markup, which Pango's parser does not know.
  parsed <- try (Pango.parseMarkup (withoutLinks markup) (-1) '\0')
  let shown = either (\(_ :: SomeException) -> Highlight.plain (plainOf markup)) (const markup) parsed
  l <- GI.new Gtk.Label [ #xalign := 0
                        , #wrap := True
                        , #wrapMode := Pango.WrapModeWordChar
                        , #selectable := True
                        , #useMarkup := True
                        , #label := shown
                        , #cssClasses := classes
                        ]
  links doc l
  Gtk.toWidget l

-- | What every text's links with a scheme go to first: True when it
-- opened one, as usta opens a forge's; otherwise GTK opens it.
forgeLinks :: IORef (Text -> IO Bool)
forgeLinks = unsafePerformIO (newIORef (const (pure False)))
{-# NOINLINE forgeLinks #-}

-- | Set once by the window, which knows the forges.
openLinksWith :: (Text -> IO Bool) -> IO ()
openLinksWith = writeIORef forgeLinks

-- | A label's links with a scheme (https:, mailto:) go to 'forgeLinks',
-- else open as GTK opens them; the others go to the document's 'Follow'.
links :: Doc -> Gtk.Label -> IO ()
links doc l = void $ GI.on l #activateLink $ \uri -> case T.stripPrefix "#" uri of
  Just name -> jump doc name >> pure True
  Nothing | hasScheme uri -> readIORef forgeLinks >>= \handled -> handled uri
          | otherwise -> doc.follow uri >> pure True
  where
    hasScheme u = case T.breakOn ":" u of
      (s, rest) -> not (T.null s) && not (T.null rest) && T.all (\c -> isAlphaNum c || c `elem` ("+-." :: String)) s

inline :: Node -> Text
inline (Node _ kind children) = case kind of
  TEXT t -> Highlight.plain t
  SOFTBREAK -> " "
  LINEBREAK -> "\n"
  CODE t -> "<span font_family=\"monospace\" background=\"#8080a0\" bgalpha=\"18%\">\x2009" <> Highlight.plain t <> "\x2009</span>"
  EMPH -> "<i>" <> inner <> "</i>"
  STRONG -> "<b>" <> inner <> "</b>"
  STRIKETHROUGH -> "<s>" <> inner <> "</s>"
  LINK url _ -> "<a href=\"" <> Highlight.plain url <> "\">" <> inner <> "</a>"
  IMAGE url _ -> "<a href=\"" <> Highlight.plain url <> "\">" <> (if T.null inner then "image" else inner) <> "</a>"
  HTML_INLINE raw -> htmlMarkup raw
  _ -> inner
  where
    inner = foldMap inline children

-- | A piece of raw HTML: a tag, with its name in lower case, or the words
-- between tags.
type Piece :: Type
data Piece
  = Tag {name :: Text, closing :: Bool, attributes :: [(Text, Text)]}
  | Words Text

pieces :: Text -> [Piece]
pieces t
  | T.null t = []
  | "<!--" `T.isPrefixOf` t = pieces (T.drop 3 (snd (T.breakOn "-->" t)))
  | "<" `T.isPrefixOf` t = case T.breakOn ">" (T.drop 1 t) of
      (inner, after) | not (T.null after), Just tag <- tagOf inner -> tag : pieces (T.drop 1 after)
      _ -> Words "<" : pieces (T.drop 1 t)
  | otherwise = let (words', rest) = T.breakOn "<" t in Words words' : pieces rest
  where
    tagOf inner =
      let body = T.dropWhileEnd (== '/') (T.strip inner)
          closing = "/" `T.isPrefixOf` body
          rest = if closing then T.drop 1 body else body
          name = T.toLower (T.takeWhile isAlphaNum rest)
       in if T.null name then Nothing else Just Tag {name = name, closing = closing, attributes = attributesOf (T.drop (T.length name) rest)}
    attributesOf a = case T.breakOn "=" a of
      (k, v) | not (T.null v) ->
        let key = T.toLower (T.takeWhileEnd (not . isSpace) (T.stripEnd k))
            after = T.stripStart (T.drop 1 v)
            (value, more) = case T.uncons after of
              Just (q, r) | q == '"' || q == '\'' -> let (x, y) = T.breakOn (T.singleton q) r in (x, T.drop 1 y)
              _ -> T.break isSpace after
         in (key, value) : attributesOf more
      _ -> []

-- | Raw HTML as Pango markup, keeping the tags forges allow.
htmlMarkup :: Text -> Text
htmlMarkup = foldMap one . pieces
  where
    one p = case p of
      Words w -> Highlight.plain (entities w)
      Tag {name = n, closing = c, attributes = a} -> case n of
        "br" -> "\n"
        _ | Just pango <- lookup n simple -> (if c then "</" else "<") <> pango <> ">"
          | n `elem` ["code", "tt", "samp", "kbd"] -> if c then "</span>" else "<span font_family=\"monospace\" background=\"#8080a0\" bgalpha=\"18%\">"
          | n == "mark" -> if c then "</span>" else "<span background=\"#FFE58A\" bgalpha=\"70%\">"
          | n == "a" -> if c then "</a>" else "<a href=\"" <> Highlight.plain (maybe "#" entities (lookup "href" a)) <> "\">"
          | n == "img" -> "<a href=\"" <> Highlight.plain (maybe "#" entities (lookup "src" a)) <> "\">" <> Highlight.plain (maybe "image" entities (lookup "alt" a)) <> "</a>"
          | otherwise -> ""
    simple = [("b", "b"), ("strong", "b"), ("i", "i"), ("em", "i"), ("s", "s"), ("del", "s"), ("strike", "s"), ("u", "u"), ("ins", "u"), ("sub", "sub"), ("sup", "sup"), ("small", "small"), ("big", "big")]
    entities = T.replace "&nbsp;" "\x00A0" . T.replace "&quot;" "\"" . T.replace "&#39;" "'" . T.replace "&lt;" "<" . T.replace "&gt;" ">" . T.replace "&amp;" "&"

-- | Markup with its <a> tags taken out, the rest as it was.
withoutLinks :: Text -> Text
withoutLinks t = case T.breakOn "<a " t of
  (before, rest) | not (T.null rest) -> T.replace "</a>" "" before <> withoutLinks (T.drop 1 (snd (T.breakOn ">" rest)))
  _ -> T.replace "</a>" "" t

-- | The text of Pango markup without its tags.
plainOf :: Text -> Text
plainOf markup = foldMap one (pieces markup)
  where
    one p = case p of
      Words w -> T.replace "&lt;" "<" (T.replace "&gt;" ">" (T.replace "&amp;" "&" w))
      Tag {} -> ""

-- | Bullets or numbers; an item starting "[ ]" or "[x]" is a task with a
-- check box.
list :: Doc -> ListAttributes -> [Node] -> IO Gtk.Widget
list doc attributes items = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #cssClasses := ["list"]]
  forM_ (zip [attributes.listStart ..] items) $ \(n, Node p _ content) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    let (task, rest) = taskOf content
    marker <- case task of
      Just done -> GI.new Gtk.CheckButton [#active := done, #sensitive := False, #valign := Gtk.AlignStart] >>= Gtk.toWidget
      Nothing -> do
        l <- GI.new Gtk.Label [ #label := if attributes.listType == ORDERED_LIST then T.pack (show n) <> "." else "•"
                              , #valign := Gtk.AlignStart
                              , #cssClasses := ["marker"]
                              ]
        Gtk.toWidget l
    #append row marker
    body <- block doc (Node p DOCUMENT rest)
    GI.set body [#hexpand := True]
    #append row body
    #append box row
  Gtk.toWidget box
  where
    taskOf content = case content of
      Node p PARAGRAPH (Node q (TEXT t) more : others) : blocks
        | "[ ] " `T.isPrefixOf` t -> (Just False, Node p PARAGRAPH (Node q (TEXT (T.drop 4 t)) more : others) : blocks)
        | "[x] " `T.isPrefixOf` T.toLower t -> (Just True, Node p PARAGRAPH (Node q (TEXT (T.drop 4 t)) more : others) : blocks)
      _ -> (Nothing, content)

table :: Doc -> [TableCellAlignment] -> [Node] -> IO Gtk.Widget
table doc alignments rows = do
  grid <- GI.new Gtk.Grid [#cssClasses := ["table"], #halign := Gtk.AlignStart]
  forM_ (zip [0 ..] rows) $ \(r, Node _ _ cells) ->
    forM_ (zip3 [0 ..] cells (alignments <> repeat NoAlignment)) $ \(c, Node _ _ content, alignment) -> do
      cell <- GI.new Gtk.Label [ #useMarkup := True
                               , #label := foldMap inline content
                               , #selectable := True
                               , #xalign := case alignment of
                                   RightAligned -> 1
                                   CenterAligned -> 0.5
                                   _ -> 0
                               , #cssClasses := ["cell"] <> ["header" | r == (0 :: Int)]
                               ]
      links doc cell
      Gtk.gridAttach grid cell c (fromIntegral r) 1 1
  -- A wide table scrolls by itself rather than widening the page.
  scroll <- GI.new Gtk.ScrolledWindow [ #child := grid, #vscrollbarPolicy := Gtk.PolicyTypeNever
                                      , #propagateNaturalHeight := True, #propagateNaturalWidth := True ]
  Scroll.sideways scroll
  Gtk.toWidget scroll

-- | Diff lines, each with its "+", "-", "@@" or " ", coloured as a diff;
-- @copied@ is what Copy takes, the whole block when 'Nothing'.
diffBlock :: Text -> Maybe Text -> [Text] -> IO Gtk.Widget
diffBlock title copied rows = do
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["code-block", "diff"]]
  bar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["code-bar"]]
  name <- GI.new Gtk.Label [#label := title, #hexpand := True, #xalign := 0]
  #append bar name
  copy <- GI.new Gtk.Button [#label := "Copy", #cssClasses := ["copy"]]
  void $ GI.on copy #clicked $ do
    clipboard <- Gtk.widgetGetClipboard copy
    provider <- GI.toGValue (Just (maybe (T.unlines rows) id copied)) >>= Gdk.contentProviderNewForValue
    void (Gdk.clipboardSetContent clipboard (Just provider))
  #append bar copy
  #append card bar
  lines' <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["diff-lines"]]
  forM_ rows $ \row -> do
    let kind
          | "@@" `T.isPrefixOf` row = "hunk"
          | any (`T.isPrefixOf` row) ["+++", "---", "diff ", "index "] = "file"
          | "+" `T.isPrefixOf` row = "add"
          | "-" `T.isPrefixOf` row = "del"
          | otherwise = "ctx"
    l <- GI.new Gtk.Label [#label := if T.null row then " " else row, #xalign := 0, #selectable := True, #cssClasses := ["line", kind]]
    #append lines' l
  scroll <- GI.new Gtk.ScrolledWindow [#child := lines', #vscrollbarPolicy := Gtk.PolicyTypeNever, #propagateNaturalHeight := True]
  Scroll.sideways scroll
  #append card scroll
  Gtk.toWidget card

-- | A fenced block: its language and Copy above, the code highlighted.
codeBlock :: Text -> Text -> IO Gtk.Widget
codeBlock language code = do
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["code-block"]]
  bar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["code-bar"]]
  name <- GI.new Gtk.Label [#label := if T.null language then "text" else language, #hexpand := True, #xalign := 0]
  #append bar name
  copy <- GI.new Gtk.Button [#label := "Copy", #cssClasses := ["copy"]]
  void $ GI.on copy #clicked $ do
    clipboard <- Gtk.widgetGetClipboard copy
    provider <- GI.toGValue (Just code) >>= Gdk.contentProviderNewForValue
    void (Gdk.clipboardSetContent clipboard (Just provider))
  #append bar copy
  #append card bar
  body <- GI.new Gtk.Label [#xalign := 0, #selectable := True, #cssClasses := ["code"]]
  Highlight.follow body $ \palette ->
    Gtk.labelSetMarkup body (T.intercalate "\n" (Highlight.markup palette (Highlight.languageNamed language) (T.stripEnd code)))
  scroll <- GI.new Gtk.ScrolledWindow [#child := body, #vscrollbarPolicy := Gtk.PolicyTypeNever, #propagateNaturalHeight := True]
  Scroll.sideways scroll
  #append card scroll
  Gtk.toWidget card

css :: Text
css = mconcat
  [ ".u-markdown label.paragraph { line-height: 1.5; }"
  , ".u-markdown label.heading { font-weight: 600; }"
  , ".u-markdown label.h1 { font-size: 24px; padding-bottom: 6px; border-bottom: 1px solid var(--border-subtle); }"
  , ".u-markdown label.h2 { font-size: 18px; margin-top: 4px; }"
  , ".u-markdown label.h3 { font-size: 16px; }"
  , ".u-markdown label.h4, .u-markdown label.h5, .u-markdown label.h6 { font-size: 14px; }"
  , ".u-markdown .quote { padding: 2px 0 2px 14px; border-left: 3px solid var(--input-border); color: var(--text-soft); }"
  , ".u-markdown label.marker { color: var(--text-muted); min-width: 16px; }"
  , ".u-markdown label.mono { font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , ".u-markdown grid.table { border: 1px solid var(--border); border-radius: 6px; }"
  , ".u-markdown grid.table label.cell { padding: 6px 12px; border-right: 1px solid var(--border); border-bottom: 1px solid var(--border); font-size: 13px; }"
  , ".u-markdown grid.table label.header { font-weight: 600; background: var(--window); }"
  , ".u-markdown box.code-block { border: 1px solid var(--border); border-radius: 8px; }"
  , ".u-markdown box.code-bar { padding: 4px 6px 4px 12px; background: var(--window); border-bottom: 1px solid var(--border); font-size: 12px; color: var(--text-muted); }"
  , ".u-markdown box.code-bar button.copy { min-height: 26px; padding: 0 10px; font-size: 12px; }"
  , ".u-markdown label.code { padding: 10px 14px; font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , ".u-markdown box.diff-lines { padding: 6px 0; }"
  , ".u-markdown box.diff-lines label.line { padding: 0 14px; font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , ".u-markdown box.diff-lines label.add { background: var(--success-bg); }"
  , ".u-markdown box.diff-lines label.del { background: var(--danger-bg); }"
  , ".u-markdown box.diff-lines label.hunk { color: var(--accent); background: var(--accent-bg); }"
  , ".u-markdown box.diff-lines label.file { color: var(--text-muted); font-weight: 600; }"
  , ".u-markdown expander.details > title { font-weight: 600; }"
  ]
