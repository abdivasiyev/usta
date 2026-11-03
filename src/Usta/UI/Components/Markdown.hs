{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Markdown as native widgets: GitHub-flavored (tables, strikethrough,
-- autolinks, task lists) through cmark-gfm. Code blocks are highlighted
-- like files; links with a scheme open in the browser.
module Usta.UI.Components.Markdown
  ( Follow
  , new
  , newIn
  , css
  ) where

import Control.Monad (forM_, void)
import Data.Char (isAlphaNum)
import Data.Kind (Type)
import CMarkGFM (ListAttributes (..), ListType (..), Node (..), NodeType (..), TableCellAlignment (..), commonmarkToNode, extAutolink, extStrikethrough, extTable)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

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
newIn follow text = do
  w <- block follow (commonmarkToNode [] [extTable, extStrikethrough, extAutolink] text)
  Gtk.widgetAddCssClass w "u-markdown"
  pure w

block :: Follow -> Node -> IO Gtk.Widget
block follow (Node _ kind children) = case kind of
  DOCUMENT -> column follow 12 children
  PARAGRAPH -> paragraph follow ["paragraph"] children
  HEADING level -> paragraph follow ["heading", "h" <> T.pack (show (min 6 level))] children
  BLOCK_QUOTE -> do
    w <- column follow 8 children
    Gtk.widgetAddCssClass w "quote"
    pure w
  THEMATIC_BREAK -> GI.new Gtk.Separator [#orientation := Gtk.OrientationHorizontal] >>= Gtk.toWidget
  CODE_BLOCK info code
    | T.toLower (T.strip info) == "mermaid" -> Mermaid.new code
    | otherwise -> codeBlock (T.takeWhile (/= ' ') (T.strip info)) code
  HTML_BLOCK raw -> text' follow (Highlight.plain (T.stripEnd raw)) ["mono"]
  LIST attributes -> list follow attributes children
  TABLE alignments -> table follow alignments children
  _ -> column follow 8 children

column :: Follow -> Int -> [Node] -> IO Gtk.Widget
column follow gap children = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := fromIntegral gap]
  forM_ children $ \c -> block follow c >>= #append box
  Gtk.toWidget box

paragraph :: Follow -> [Text] -> [Node] -> IO Gtk.Widget
paragraph follow classes children = text' follow (foldMap inline children) classes

-- | Selectable, wrapping text from Pango markup.
text' :: Follow -> Text -> [Text] -> IO Gtk.Widget
text' follow markup classes = do
  l <- GI.new Gtk.Label [ #xalign := 0
                        , #wrap := True
                        , #selectable := True
                        , #useMarkup := True
                        , #label := markup
                        , #cssClasses := classes
                        ]
  links follow l
  Gtk.toWidget l

-- | A label's links with a scheme (https:, mailto:) open as GTK opens
-- them; the others go to @follow@.
links :: Follow -> Gtk.Label -> IO ()
links follow l = void $ GI.on l #activateLink $ \uri ->
  if hasScheme uri then pure False else follow uri >> pure True
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
  HTML_INLINE raw -> Highlight.plain raw
  _ -> inner
  where
    inner = foldMap inline children

-- | Bullets or numbers; an item starting "[ ]" or "[x]" is a task with a
-- check box.
list :: Follow -> ListAttributes -> [Node] -> IO Gtk.Widget
list follow attributes items = do
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
    body <- block follow (Node p DOCUMENT rest)
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

table :: Follow -> [TableCellAlignment] -> [Node] -> IO Gtk.Widget
table follow alignments rows = do
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
      links follow cell
      Gtk.gridAttach grid cell c (fromIntegral r) 1 1
  Gtk.toWidget grid

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
  ]
