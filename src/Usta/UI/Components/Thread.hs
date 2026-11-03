{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A review thread under a line of a diff, as the design draws it: an
-- open one expanded, a resolved one a single line that opens on click,
-- an outdated one with the code it was left on. Notes written since the
-- user's last review are marked new.
module Usta.UI.Components.Thread
  ( Shown (..)
  , Thread (..)
  , new
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

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Markdown as Markdown
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
  , lastReviewed :: Maybe UTCTime
  , me :: Text
    -- ^ The user's login, whose own notes are never new.
  , clock :: Clock
  , reply :: Text -> IO (Either Text ())
    -- ^ Adds a note; runs off the main thread.
  }

-- | The drawn thread, and what the Files tab does with it.
type Thread :: Type
data Thread = Thread
  { widget :: Gtk.Widget
  , setOpen :: Bool -> IO ()
    -- ^ Shows the notes, or only the one-line bar; an open thread is
    -- always shown whole.
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

  -- The one-line bar a resolved thread starts as.
  bar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["bar"]]
  label "✓" ["tick"] >>= #append bar
  label ("Resolved by " <> maybe "someone" unparse s.thread.resolvedBy) ["strong"] >>= #append bar
  snippet <- label (maybe "" (\n -> unparse n.author <> ": “" <> firstLine n.body <> "”") (headOf notes)) ["muted"]
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
      label ("was on line " <> T.pack (show s.thread.line) <> " in " <> s.thread.commit) ["muted"] >>= #append top
    else if resolved'
      then label ("✓ Resolved by " <> maybe "someone" unparse s.thread.resolvedBy) ["tick", "strong"] >>= #append top
      else do
        Badge.dot Warning >>= #append top
        label "Open" ["open", "strong"] >>= #append top
        label ("· line " <> T.pack (show s.thread.line) <> " · " <> count) ["muted"] >>= #append top
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

  -- An outdated thread shows the code it was about.
  when s.outdated $ do
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
        Avatar.new 24 (T.toUpper (T.take 2 who)) >>= \a -> GI.set a [#valign := Gtk.AlignStart] >> #append row a
        column <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #hexpand := True]
        top' <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
        label who ["strong"] >>= #append top'
        label ("· " <> when'') ["muted"] >>= #append top'
        when isFresh $ Badge.new Accent "New" >>= #append top'
        #append column top'
        Markdown.new body >>= #append column
        #append row column
        #append noteBox row
  forM_ notes $ \n -> addNote (unparse n.author) (ago s.clock n.createdAt) (fresh n) n.body
  #append whole noteBox

  -- A reply goes to the forge, then shows here at once.
  replyRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["reply"]]
  entry <- GI.new Gtk.Entry [#placeholderText := if resolved' then "Reply to reopen the discussion…" else "Reply…", #hexpand := True]
  #append replyRow entry
  send <- Button.new Button.defaults {Button.label = Just "Reply"}
  #append replyRow send
  #append whole replyRow
  problem <- label "" ["problem"]
  GI.set problem [#visible := False, #wrap := True]
  #append whole problem
  let sendReply = do
        text <- T.strip <$> Gtk.editableGetText entry
        unless (T.null text) $ do
          GI.set problem [#visible := False]
          Button.busy send [entry] "Replying…" (s.reply text) $ \case
            Left why -> GI.set problem [#label := why, #visible := True]
            Right () -> do
              addNote s.me "just now" False text
              Gtk.editableSetText entry ""
  void $ GI.on send #clicked sendReply
  void $ GI.on entry #activate sendReply
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
  pure Thread {widget = w, setOpen = setOpen, isNew = anyNew, resolved = resolved'}
  where
    resolved' = s.thread.resolvedBy /= Nothing
    headOf xs = case xs of
      x : _ -> Just x
      [] -> Nothing
    firstLine = T.takeWhile (/= '\n') . T.strip

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
  , "box.u-thread box.reply { padding: 10px 12px; border-top: 1px solid var(--border-subtle); }"
  , "box.u-thread label.problem { padding: 0 12px 10px; color: var(--danger); font-size: 12px; }"
  ]
