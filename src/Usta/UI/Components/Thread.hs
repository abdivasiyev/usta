{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A review thread under a line of a diff, as the design draws it: an
-- open one expanded, a resolved one a single line that opens on click,
-- an outdated one with the code it was left on. Notes written since the
-- user's last review are marked new. Also the box a new note on a line
-- is written in, and the card it shows as while it waits for the review.
module Usta.UI.Components.Thread
  ( Shown (..)
  , Thread (..)
  , new
  , Writing (..)
  , writer
  , quoteRange
  , ranged
  , Card (..)
  , pending
  , css
  ) where

import Control.Monad (forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Attach as Attach
import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import Usta.UI.Format (Clock, ago)
import Usta.UI.Style (Tone (..))
import Usta.Types.Parse (unparse)
import qualified Usta.Types.Thread as Th

-- | What to draw.
type Shown :: Type
data Shown = Shown
  { thread :: Th.Thread
  , outdated :: Bool
    -- ^ Its line has changed since; it sits at the nearest one left.
  , showCode :: Bool
    -- ^ Shows its file, line and code, away from the diff.
  , lastReviewed :: Maybe UTCTime
  , me :: Text
    -- ^ The user's login, whose own notes are never new.
  , clock :: Clock
  , reply :: Text -> IO (Either Text ())
    -- ^ Adds a note; runs off the main thread.
  , upload :: Maybe Attach.Uploader
  , place :: Maybe Avatar.Place
    -- ^ The forge the authors' pictures come from.
  }

-- | The drawn thread, and what the Files tab does with it.
type Thread :: Type
data Thread = Thread
  { widget :: Gtk.Widget
  , setOpen :: Bool -> IO ()
    -- ^ Shows the notes, or only the one-line bar; an open thread is
    -- always shown whole.
  , toggle :: IO ()
    -- ^ Folds an open thread, or opens a folded one.
  , focusReply :: IO ()
    -- ^ Opens it and puts the cursor in its Reply box.
  , isNew :: Bool
    -- ^ It has a note since the user's last review.
  , resolved :: Bool
  }

new :: Shown -> IO Thread
new s = do
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-thread"] <> ["resolved" | resolved'] <> ["outdated" | s.outdated]]
  let notes = s.thread.notes
      fresh n = unparse n.author /= s.me && maybe True (< n.createdAt) s.lastReviewed
      anyNew = any fresh notes
      count = T.pack (show (length notes)) <> (if length notes == 1 then " comment" else " comments")
      -- A note on several lines from a forge that takes one opens with
      -- them quoted: shown as the range, without the quote.
      range = ranged . (.body) =<< headOf notes
      bodies = case range of
        Just (_, _, rest) -> rest : map (.body) (drop 1 notes)
        Nothing -> map (.body) notes
      where' = case range of
        Just (from, _, _) -> "lines " <> lines' from s.thread.line
        Nothing -> "line " <> T.pack (show s.thread.line)
      replaced = case range of
        Just (_, code, _) -> code
        Nothing -> [T.drop 1 l | l <- take 1 (reverse s.thread.code)]

  -- The one-line bar a resolved thread starts as, and any thread folds to.
  bar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["bar"]]
  if resolved'
    then do
      label "✓" ["tick"] >>= #append bar
      label ("Resolved by " <> maybe "someone" unparse s.thread.resolvedBy) ["strong"] >>= #append bar
    else do
      Badge.dot Warning >>= #append bar
      label "Open" ["open", "strong"] >>= #append bar
  snippet <- label (maybe "" (\(n, b) -> unparse n.author <> ": “" <> firstLine b <> "”") (headOf (zip notes bodies))) ["muted"]
  GI.set snippet [#hexpand := True, #ellipsize := Pango.EllipsizeModeEnd]
  #append bar snippet
  label count ["muted"] >>= #append bar
  when anyNew $ Badge.new Accent "new reply" >>= #append bar
  showButton <- GI.new Gtk.Button [#label := "Show", #cssClasses := ["flat", "show"]]
  #append bar showButton
  #append card bar

  -- The whole thread.
  whole <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["top"]]
  if s.outdated
    then do
      Badge.new Neutral "Outdated" >>= #append top
      label ("was on " <> where' <> " in " <> s.thread.commit) ["muted"] >>= #append top
    else if resolved'
      then label ("✓ Resolved by " <> maybe "someone" unparse s.thread.resolvedBy) ["tick", "strong"] >>= #append top
      else do
        Badge.dot Warning >>= #append top
        label "Open" ["open", "strong"] >>= #append top
        label ("· " <> where' <> " · " <> count) ["muted"] >>= #append top
  spacer <- GI.new Gtk.Box [#hexpand := True]
  #append top spacer
  forge <- GI.new Gtk.Button [ #label := if resolved' then "Unresolve on the forge" else "Resolve on the forge"
                             , #cssClasses := ["flat", "forge"]
                             , #tooltipText := "Forgejo resolves threads only on its website"
                             ]
  void $ GI.on forge #clicked $ do
    launcher <- Gtk.uriLauncherNew (Just s.thread.url)
    Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
  #append top forge
  hide <- GI.new Gtk.Button [#label := "Hide", #cssClasses := ["flat", "show"]]
  when (resolved' || s.outdated) $ #append top hide
  #append whole top

  -- An outdated thread shows the code it was about, and so does one
  -- shown away from the diff, with where that code is.
  when (s.showCode && not s.outdated) $
    label (s.thread.path <> ":" <> T.pack (show s.thread.line)) ["mono", "where"] >>= #append whole
  when (s.outdated || s.showCode) $ do
    code <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["code"]]
    forM_ s.thread.code $ \l -> do
      let kind = case T.uncons l of
            Just ('+', _) -> "add"
            Just ('-', _) -> "del"
            _ -> "ctx"
      label l ["mono", kind] >>= #append code
    #append whole code

  noteBox <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  let addNote who when'' isFresh body = do
        row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["note"]]
        Avatar.person 24 s.place who >>= \a -> GI.set a [#valign := Gtk.AlignStart] >> #append row a
        column <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #hexpand := True]
        top' <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
        label who ["strong"] >>= #append top'
        label ("· " <> when'') ["muted"] >>= #append top'
        when isFresh $ Badge.new Accent "New" >>= #append top'
        #append column top'
        -- A suggestion replaces the lines the thread is on.
        Markdown.suggesting replaced body >>= #append column
        #append row column
        #append noteBox row
  forM_ (zip notes bodies) $ \(n, b) -> addNote (unparse n.author) (ago s.clock n.createdAt) (fresh n) b
  #append whole noteBox

  -- A reply goes to the forge, then shows here at once; Ctrl Enter sends.
  replyRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6, #cssClasses := ["reply"]]
  (entry, replyBuffer) <- MarkdownEditor.new MarkdownEditor.defaults {MarkdownEditor.height = 56, MarkdownEditor.compact = True, MarkdownEditor.upload = s.upload}
  #append replyRow entry
  send <- Button.new Button.defaults {Button.label = Just (if resolved' then "Reply and reopen" else "Reply")}
  GI.set send [#halign := Gtk.AlignEnd]
  #append replyRow send
  #append whole replyRow
  problem <- label "" ["problem"]
  GI.set problem [#visible := False, #wrap := True]
  #append whole problem
  let sendReply = do
        text <- T.strip . maybe "" id <$> GI.get replyBuffer #text
        unless (T.null text) $ do
          GI.set problem [#visible := False]
          Button.busy send ([] :: [Gtk.Button]) "Replying…" (s.reply text) $ \case
            Left why -> GI.set problem [#label := why, #visible := True]
            Right () -> do
              addNote s.me "just now" False text
              Gtk.textBufferSetText replyBuffer "" (-1)
  void $ GI.on send #clicked sendReply
  replyKeys <- GI.new Gtk.ShortcutController []
  shortcut replyKeys "<Primary>Return" sendReply
  Gtk.widgetAddController entry replyKeys
  #append card whole

  -- Only a resolved thread folds to its bar; Hide puts an outdated one
  -- away until it is opened again.
  let setOpen open = do
        let whole' = open || not resolved'
        GI.set card [#visible := open || not s.outdated || resolved']
        GI.set bar [#visible := not whole']
        GI.set whole [#visible := whole']
  void $ GI.on showButton #clicked (setOpen True)
  void $ GI.on hide #clicked (setOpen False)
  GI.set card [#visible := True]
  setOpen (not resolved')
  w <- Gtk.toWidget card
  -- Enter folds even an open thread to its bar.
  let fold = do
        GI.set card [#visible := True]
        GI.set bar [#visible := True]
        GI.set whole [#visible := False]
      toggle = GI.get whole #visible >>= \shown -> if shown then fold else setOpen True
      focusReply = setOpen True >> void (Gtk.widgetGrabFocus entry)
  pure Thread {widget = w, setOpen = setOpen, toggle = toggle, focusReply = focusReply, isNew = anyNew, resolved = resolved'}
  where
    resolved' = s.thread.resolvedBy /= Nothing
    headOf xs = case xs of
      x : _ -> Just x
      [] -> Nothing
    firstLine = T.takeWhile (/= '\n') . T.strip

-- New notes on lines

-- | What the box is for and what its buttons do.
type Writing :: Type
data Writing = Writing
  { path :: Text
  , first :: Int
  , line :: Int
    -- ^ The first and last line it is on, in the file as the pull
    -- request leaves it; the same for one line.
  , code :: [Text]
    -- ^ Those lines' code, for Suggest a change.
  , text :: Text
    -- ^ What the box starts with: empty, or a pending note being edited.
  , cancel :: IO ()
    -- ^ Esc or Cancel; the box is gone after.
  , add :: Text -> IO ()
    -- ^ Add to review (Ctrl Enter): the note waits for the review.
  , now :: Text -> IO (Either Text ())
    -- ^ Comment now: the note goes out alone, off the main thread.
  , posted :: Text -> IO ()
    -- ^ Comment now worked.
  , attach :: Maybe Attach.Uploader
  , draft :: MarkdownEditor.Draft
    -- ^ Where the note is kept while it is written.
  }

-- | "213–215".
lines' :: Int -> Int -> Text
lines' a b = T.pack (show a) <> "–" <> T.pack (show b)

-- | A note on lines @a@ to @b@ for a forge that takes one line: it opens
-- with the lines quoted, which reads right on the forge's page, and
-- 'ranged' reads back.
quoteRange :: Int -> Int -> [Text] -> Text -> Text
quoteRange a b code text
  | a == b = text
  | otherwise = "Lines " <> lines' a b <> ":\n```\n" <> T.unlines code <> "```\n\n" <> text

-- | The first line, the code and the rest of a note 'quoteRange' made.
ranged :: Text -> Maybe (Int, [Text], Text)
ranged body = case T.lines body of
  heading : "```" : rest
    | Just numbers <- T.stripSuffix ":" =<< T.stripPrefix "Lines " heading
    , (a, b) <- T.breakOn "–" numbers
    , [(from, "")] <- reads (T.unpack a)
    , [(_ :: Int, "")] <- reads (T.unpack (T.drop 1 b))
    , (code, "```" : after) <- break (== "```") rest ->
        Just (from, code, T.strip (T.unlines after))
  _ -> Nothing

-- | The box a new note on a line is written in, with the cursor in it.
writer :: Text -> Writing -> IO Gtk.Widget
writer me w = do
  frame <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-line-comment", "writing"]]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["top"]]
  Avatar.new 24 (T.toUpper (T.take 2 me)) >>= #append top
  label (if w.first == w.line then "Comment on line " <> T.pack (show w.line) else "Comment on lines " <> lines' w.first w.line) ["strong"] >>= #append top
  pathLabel <- label w.path ["mono", "muted", "small"]
  GI.set pathLabel [#hexpand := True, #ellipsize := Pango.EllipsizeModeStart]
  #append top pathLabel
  suggest <- Button.new Button.defaults {Button.label = Just "± Suggest a change", Button.variant = Button.Flat, Button.small = True, Button.tooltip = Just "Puts the code into a suggestion to edit"}
  #append top suggest
  #append frame top

  -- The toolbar, emoji, :name: and Preview of every comment box.
  (editor, buffer) <- MarkdownEditor.new MarkdownEditor.defaults {MarkdownEditor.text = w.text, MarkdownEditor.height = 72, MarkdownEditor.upload = w.attach, MarkdownEditor.draft = Just w.draft}
  #append frame editor
  void $ GI.on suggest #clicked $ do
    Gtk.textBufferInsertAtCursor buffer ("```suggestion\n" <> T.unlines w.code <> "```\n") (-1)
    void (Gtk.widgetGrabFocus editor)

  problem <- label "" ["problem"]
  GI.set problem [#visible := False, #wrap := True]
  #append frame problem

  bottom <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["bottom"]]
  hint <- label (if w.first == w.line then "Markdown" else "Markdown · it goes on line " <> T.pack (show w.line) <> ", opening with lines " <> lines' w.first w.line <> " quoted") ["muted", "small"]
  GI.set hint [#ellipsize := Pango.EllipsizeModeEnd]
  GI.set hint [#hexpand := True]
  #append bottom hint
  cancelButton <- Button.new Button.defaults {Button.label = Just "Cancel", Button.variant = Button.Flat}
  nowButton <- Button.new Button.defaults {Button.label = Just "Comment now"}
  GI.set nowButton [#tooltipText := "Posts it alone, as a comment"]
  addButton <- Button.new Button.defaults {Button.label = Just "Add to review", Button.variant = Button.Primary}
  mapM_ (#append bottom) [cancelButton, nowButton, addButton]
  #append frame bottom

  let written = T.strip . maybe "" id <$> GI.get buffer #text
      addIt = written >>= \t -> unless (T.null t) (w.add t)
      postIt = written >>= \t -> unless (T.null t) $ do
        GI.set problem [#visible := False]
        Button.busy nowButton [cancelButton, addButton] "Posting…" (w.now t) $ \case
          Left why -> GI.set problem [#label := why, #visible := True]
          Right () -> w.posted t
  void $ GI.on cancelButton #clicked w.cancel
  void $ GI.on nowButton #clicked postIt
  void $ GI.on addButton #clicked addIt
  keys <- GI.new Gtk.ShortcutController []
  shortcut keys "<Primary>Return" addIt
  shortcut keys "Escape" w.cancel
  Gtk.widgetAddController editor keys

  -- The cursor goes in once the box is on screen.
  void $ GI.on frame #map (void (Gtk.widgetGrabFocus editor))
  Gtk.toWidget frame

-- | A note shown under its line, waiting for the review or posted.
type Card :: Type
data Card = Card
  { widget :: Gtk.Widget
  , sent :: IO ()
    -- ^ It went out with the review: no more Edit or Delete.
  }

-- | A pending note by @me@, with Edit and Delete; 'sent' marks it sent.
pending :: Text -> Text -> IO () -> IO () -> IO Card
pending me body edit delete = do
  frame <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-line-comment", "pending"]]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["top"]]
  Avatar.new 24 (T.toUpper (T.take 2 me)) >>= #append top
  label me ["strong"] >>= #append top
  pill <- GI.new Gtk.Box []
  Badge.new Warning "Pending" >>= #append pill
  #append top pill
  status <- label "sent with your review" ["muted", "small"]
  GI.set status [#hexpand := True]
  #append top status
  editButton <- Button.new Button.defaults {Button.label = Just "Edit", Button.variant = Button.Flat}
  deleteButton <- Button.new Button.defaults {Button.label = Just "Delete", Button.variant = Button.Danger}
  mapM_ (#append top) [editButton, deleteButton]
  #append frame top
  text <- label body ["body"]
  GI.set text [#wrap := True, #selectable := True]
  #append frame text
  void $ GI.on editButton #clicked edit
  void $ GI.on deleteButton #clicked delete
  let markSent = do
        Gtk.widgetGetFirstChild pill >>= mapM_ (Gtk.boxRemove pill)
        Badge.new Success "Sent" >>= #append pill
        GI.set status [#label := "just now"]
        mapM_ (\b -> GI.set b [#visible := False]) [editButton, deleteButton]
        Gtk.widgetRemoveCssClass frame "pending"
  w <- Gtk.toWidget frame
  pure Card {widget = w, sent = markSent}

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-thread { margin: 8px 12px 8px 108px; border: 1px solid var(--border); border-radius: 8px; background: var(--surface); }"
  , "box.u-thread.resolved, box.u-thread.outdated { background: var(--window); }"
  , "box.u-thread box.bar { padding: 6px 12px; min-height: 26px; font-size: 13px; }"
  , "box.u-thread box.top { padding: 8px 12px; border-bottom: 1px solid var(--border-subtle); font-size: 12px; }"
  , "box.u-thread label.strong { font-weight: 600; }"
  , "box.u-thread label.tick { color: var(--success); }"
  , "box.u-thread label.open { color: var(--warning); }"
  , "box.u-thread button.show, box.u-thread button.forge { color: var(--accent); min-height: 24px; padding: 0 6px; font-size: 12px; }"
  , "box.u-thread box.code { margin: 10px 12px 0; border: 1px solid var(--border); border-radius: 6px; }"
  , "box.u-thread box.code label { padding: 1px 10px; font-family: 'JetBrains Mono', monospace; font-size: 12px; }"
  , "box.u-thread box.code label.add { background: var(--success-bg); }"
  , "box.u-thread box.code label.del { background: var(--danger-bg); }"
  , "box.u-thread box.note { padding: 10px 12px; }"
  , "box.u-thread label.where { padding: 10px 12px 0; font-size: 12px; color: var(--accent); }"
  , ".u-conversation box.u-thread { margin: 0; }"
  , "box.u-thread box.reply { padding: 10px 12px; border-top: 1px solid var(--border-subtle); }"
  , "box.u-thread label.problem { padding: 0 12px 10px; color: var(--danger); font-size: 12px; }"
  , "box.u-line-comment { margin: 6px 12px 10px 108px; border: 1px solid var(--border); border-radius: 8px; background: var(--surface); }"
  , "box.u-line-comment.writing { border: 2px solid var(--accent); }"
  , "box.u-line-comment.pending { border-style: dashed; }"
  , "box.u-line-comment box.top { padding: 8px 12px; font-size: 13px; }"
  , "box.u-line-comment.writing box.top { border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-line-comment label.strong { font-weight: 600; }"
  , "box.u-line-comment label.body { padding: 0 12px 10px 44px; }"
  , "box.u-line-comment box.bottom { padding: 8px 12px; border-top: 1px solid var(--border-subtle); background: var(--window); border-radius: 0 0 8px 8px; }"
  , "box.u-line-comment label.problem { padding: 0 12px 8px; color: var(--danger); font-size: 12px; }"
  ]
