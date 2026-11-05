{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A comment or description as the design draws it: who and when, marked
-- "edited", its Markdown, and its reactions, which toggle with one click.
-- A ⋯ menu has Quote reply and Copy link, and on the user's own Edit,
-- which edits in place, and Delete, which asks once.
module Usta.UI.Components.CommentCard
  ( Card (..)
  , new
  , emoji
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Attach as Attach
import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import Usta.UI.Format (Clock, ago)
import Usta.Types.Discussion (Comment (..), Feeling (..), Reaction (..))
import Usta.Types.Parse (unparse)

type Card :: Type
data Card = Card
  { comment :: Comment
  , clock :: Clock
  , verb :: Text
    -- ^ e.g. "opened this" or "commented".
  , mine :: Bool
    -- ^ The user's own: Edit and Delete.
  , badge :: Maybe Text
    -- ^ e.g. "Author", beside the menu.
  , edit :: Text -> IO (Either Text ())
  , delete :: Maybe (IO (Either Text ()))
    -- ^ 'Nothing' for a description, which cannot be deleted.
  , quote :: Text -> IO ()
    -- ^ Puts the quoted text into the reply box.
  , react :: Feeling -> Bool -> IO (Either Text ())
    -- ^ 'True' adds the reaction, 'False' takes it back.
  , upload :: Maybe Attach.Uploader
    -- ^ Where files attached while editing go.
  , changed :: IO ()
    -- ^ After an edit or delete, e.g. to reload the page.
  , place :: Maybe Avatar.Place
    -- ^ The forge the author's picture comes from.
  }

new :: Card -> IO Gtk.Box
new c = do
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-comment-card"]]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["head"]]
  let who = unparse c.comment.author
  Avatar.person 24 c.place who >>= #append top
  label who ["strong"] >>= #append top
  when' <- label (c.verb <> " " <> ago c.clock c.comment.createdAt <> (if c.comment.edited then " · edited" else "")) ["muted"]
  GI.set when' [#hexpand := True]
  #append top when'
  forM_ c.badge $ \b -> label b ["badge"] >>= #append top
  dots <- label "⋯" ["dots"]
  menuButton <- GI.new Gtk.MenuButton [#child := dots, #tooltipText := "More", #cssClasses := ["more"]]
  #append top menuButton
  #append card top

  -- Deleting asks once, under the text.
  confirm <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #visible := False, #cssClasses := ["confirm"]]
  q <- label "Delete this comment? It cannot be undone." []
  GI.set q [#hexpand := True]
  #append confirm q
  keep <- Button.new Button.defaults {Button.label = Just "Keep it"}
  gone <- Button.new Button.defaults {Button.label = Just "Delete", Button.variant = Button.Danger}
  #append confirm keep >> #append confirm gone
  -- What the card shows under its head: the text, or the editor.
  slot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["body"]]
  #append card slot
  problem <- label "" ["problem"]
  GI.set problem [#visible := False, #wrap := True]
  let say e = GI.set problem [#label := e, #visible := True]
      showText = do
        clear slot
        text <- if T.null (T.strip c.comment.body)
          then label "No description." ["muted"] >>= Gtk.toWidget
          else Markdown.new c.comment.body
        #append slot text
      showEditor = do
        clear slot
        GI.set problem [#visible := False]
        GI.set confirm [#visible := False]
        (editor, buffer) <- MarkdownEditor.new MarkdownEditor.defaults {MarkdownEditor.text = c.comment.body, MarkdownEditor.upload = c.upload}
        #append slot editor
        buttons <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd]
        cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
        save <- Button.new Button.defaults {Button.label = Just "Save", Button.variant = Button.Primary}
        #append buttons cancel >> #append buttons save
        #append slot buttons
        Gtk.widgetAddCssClass card "editing"
        void $ GI.on cancel #clicked (Gtk.widgetRemoveCssClass card "editing" >> showText)
        void $ GI.on save #clicked $ do
          s <- Gtk.textBufferGetStartIter buffer
          e <- Gtk.textBufferGetEndIter buffer
          text <- Gtk.textBufferGetText buffer s e False
          Button.busy save [cancel] "Saving…" (c.edit text) (either say (const c.changed))
  showText

  -- The menu: a popover of plain buttons.
  items <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["menu"]]
  menu <- GI.new Gtk.Popover [#child := items, #hasArrow := False, #cssClasses := ["u-comment-menu"]]
  GI.set menuButton [#popover := menu]
  let item text danger act = do
        caption <- label text []
        b <- GI.new Gtk.Button [#child := caption, #cssClasses := ["item"] <> ["danger" | danger]]
        void $ GI.on b #clicked (Gtk.popoverPopdown menu >> act)
        #append items b
  when c.mine $ item "Edit" False showEditor
  item "Quote reply" False (c.quote (T.unlines (map ("> " <>) (T.lines c.comment.body))))
  unless (T.null c.comment.url) $ item "Copy link" False $ do
    clipboard <- Gtk.widgetGetClipboard card
    provider <- GI.toGValue (Just c.comment.url) >>= Gdk.contentProviderNewForValue
    void (Gdk.clipboardSetContent clipboard (Just provider))
  forM_ (if c.mine then c.delete else Nothing) $ \del -> do
    item "Delete" True $ do
      Gtk.widgetRemoveCssClass card "editing"
      showText
      GI.set confirm [#visible := True]
    void $ GI.on keep #clicked (GI.set confirm [#visible := False])
    void $ GI.on gone #clicked (Button.busy gone [keep] "Deleting…" del (either say (const c.changed)))
  #append card confirm

  -- Reactions: one chip per feeling used, and a picker of all eight.
  reactions <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #cssClasses := ["reactions"]]
  let toggle feeling adding = void $ forkIO $ do
        result <- c.react feeling adding
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          either say (const c.changed) result
          pure False
  forM_ c.comment.reactions $ \r -> do
    chip <- GI.new Gtk.Button [ #label := emoji r.feeling <> " " <> T.pack (show r.count)
                              , #tooltipText := (if r.mine then "Take your reaction back" else "React with this too")
                              , #cssClasses := ["chip"] <> ["mine" | r.mine] ]
    void $ GI.on chip #clicked (GI.set chip [#sensitive := False] >> toggle r.feeling (not r.mine))
    #append reactions chip
  palette <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 2, #cssClasses := ["palette"]]
  picker <- GI.new Gtk.Popover [#child := palette, #hasArrow := False, #cssClasses := ["u-comment-menu"]]
  addCaption <- label "☺ +" []
  add <- GI.new Gtk.MenuButton [#child := addCaption, #popover := picker, #tooltipText := "Add a reaction", #cssClasses := ["chip", "add"]]
  forM_ [minBound .. maxBound] $ \f -> do
    let had = any (\r -> r.feeling == f && r.mine) c.comment.reactions
    b <- GI.new Gtk.Button [#label := emoji f, #cssClasses := ["pick"] <> ["mine" | had]]
    void $ GI.on b #clicked (Gtk.popoverPopdown picker >> toggle f (not had))
    #append palette b
  #append reactions add
  #append card reactions
  #append card problem
  pure card
  where
    label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]
    clear b = Gtk.widgetGetFirstChild b >>= mapM_ (\w -> Gtk.boxRemove b w >> clear b)

-- | How each feeling is drawn.
emoji :: Feeling -> Text
emoji f = case f of
  ThumbsUp -> "👍"
  ThumbsDown -> "👎"
  Laugh -> "😄"
  Hooray -> "🎉"
  Confused -> "😕"
  Heart -> "❤️"
  Rocket -> "🚀"
  Eyes -> "👀"

css :: Text
css = mconcat
  [ "box.u-comment-card { background: var(--surface); border: 1px solid var(--border); border-radius: 8px; }"
  , "box.u-comment-card.editing { border-color: var(--accent); box-shadow: 0 0 0 1px var(--accent); }"
  , "box.u-comment-card box.head { padding: 8px 8px 8px 12px; background: var(--window); border-bottom: 1px solid var(--border); border-radius: 8px 8px 0 0; }"
  , "box.u-comment-card label.strong { font-weight: 600; }"
  , "box.u-comment-card label.badge { padding: 1px 8px; border-radius: 10px; border: 1px solid var(--border); font-size: 12px; color: var(--text-muted); }"
  , "box.u-comment-card menubutton.more > button { min-height: 26px; min-width: 26px; padding: 0 6px; border: none; box-shadow: none; background: transparent; }"
  , "box.u-comment-card label.dots { font-size: 18px; font-weight: 700; }"
  , "box.u-comment-card box.body { padding: 12px; }"
  , "box.u-comment-card box.reactions { padding: 0 12px 12px; }"
  , "box.u-comment-card button.chip, box.u-comment-card menubutton.chip > button {"
  , "  min-height: 26px; padding: 0 8px; border-radius: 13px; border: 1px solid var(--border); background: var(--surface); box-shadow: none; background-image: none; font-size: 13px; }"
  , "box.u-comment-card button.chip.mine { background: var(--accent-bg); border-color: var(--accent); }"
  , "box.u-comment-card box.confirm { margin: 0 12px 12px; padding: 8px 10px; border-radius: 6px; background: var(--danger-bg); }"
  , "box.u-comment-card label.problem { margin: 0 12px 10px; color: var(--danger); font-size: 12px; }"
  , "popover.u-comment-menu > contents { padding: 4px; border-radius: 10px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: 0 8px 24px rgba(27,29,33,0.14); }"
  , "popover.u-comment-menu button.item { min-height: 32px; padding: 0 12px; border: none; box-shadow: none; background: transparent; background-image: none; }"
  , "popover.u-comment-menu button.item:hover { background: var(--border-subtle); }"
  , "popover.u-comment-menu button.item.danger { color: var(--danger); }"
  , "popover.u-comment-menu button.item label { margin: 0; }"
  , "popover.u-comment-menu button.pick { min-height: 34px; min-width: 34px; padding: 0; border: none; border-radius: 8px; box-shadow: none; background: transparent; background-image: none; font-size: 18px; }"
  , "popover.u-comment-menu button.pick:hover { background: var(--border-subtle); }"
  , "popover.u-comment-menu button.pick.mine { background: var(--accent-bg); }"
  ]
