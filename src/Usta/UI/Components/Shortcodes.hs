{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Emoji shortcodes in a text view: typing @:@ and two letters offers the
-- matching @:name:@ codes under the cursor, and a finished @:name:@ turns
-- into its emoji. The names are GitHub's (gemoji), which forges share.
module Usta.UI.Components.Shortcodes
  ( attach
  , css
  ) where

import Control.Monad (forM_, void, when)
import Data.Char (isAlphaNum)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)), set)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import Text.Emoji (emojiFromAlias, emojis)

-- | How many codes the list offers at once.
shown :: Int
shown = 5

-- | The code being typed at the end of @before@ (the line up to the
-- cursor): what follows its last @:@, when that is two or more name
-- characters and the @:@ starts a word, so @12:30@ and URLs are left
-- alone.
typing :: Text -> Maybe Text
typing before = case T.breakOnEnd ":" before of
  (upTo, name)
    | not (T.null upTo)
    , T.length name >= 2
    , T.all nameChar name
    , T.all (not . isAlphaNum) (T.takeEnd 1 (T.dropEnd 1 upTo)) -> Just name
  _ -> Nothing
  where
    nameChar c = isAlphaNum c || c `elem` ("_+-" :: String)

-- | The codes starting with @name@, shortest first, then those that only
-- contain it.
matches :: Text -> [(Text, Text)]
matches name = take shown (sortOn rank [m | m@(alias, _) <- emojis, name `T.isInfixOf` alias])
  where
    rank (alias, _) = (not (name `T.isPrefixOf` alias), T.length alias, alias)

-- | A finished @:name:@ at the end of @before@, with its emoji.
finished :: Text -> Maybe (Text, Text)
finished before = do
  rest <- T.stripSuffix ":" before
  name <- typing rest
  emoji <- emojiFromAlias name
  pure (name, emoji)

attach :: Gtk.TextView -> IO ()
attach view = do
  buffer <- Gtk.textViewGetBuffer view
  list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeBrowse, #cssClasses := ["codes"]]
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append box list
  GI.new Gtk.Label [#label := "Tab or Enter inserts · Esc keeps the text", #xalign := 0, #cssClasses := ["hint"]] >>= #append box
  popover <- GI.new Gtk.Popover [ #child := box, #autohide := False, #hasArrow := False
                                , #position := Gtk.PositionTypeBottom, #cssClasses := ["u-shortcodes"] ]
  Gtk.widgetSetParent popover view
  offered <- newIORef []
  -- Where the code Esc set aside starts, so typing on does not offer it again.
  dismissed <- newIORef Nothing

  let cursor = do
        mark <- Gtk.textBufferGetInsert buffer
        Gtk.textBufferGetIterAtMark buffer mark
      lineBefore = do
        at <- cursor
        start <- Gtk.textIterCopy at
        Gtk.textIterSetLineOffset start 0
        (,) at <$> Gtk.textBufferGetText buffer start at False
      -- Replaces the last @n@ characters before the cursor with @emoji@.
      replace n emoji = do
        at <- cursor
        start <- Gtk.textIterCopy at
        void (Gtk.textIterBackwardChars start (fromIntegral n))
        Gtk.textBufferBeginUserAction buffer
        Gtk.textBufferDelete buffer start at
        Gtk.textBufferInsertAtCursor buffer emoji (-1)
        Gtk.textBufferEndUserAction buffer
      hide = Gtk.popoverPopdown popover
      choose i = do
        codes <- readIORef offered
        forM_ (lookup i (zip [0 ..] codes)) $ \(_, emoji) -> do
          hide
          (_, before) <- lineBefore
          forM_ (typing before) $ \name -> replace (T.length name + 1) emoji
      selected = do
        row <- Gtk.listBoxGetSelectedRow list
        maybe (pure 0) (fmap fromIntegral . Gtk.listBoxRowGetIndex) row
      move step = do
        codes <- readIORef offered
        i <- selected
        let j = max 0 (min (length codes - 1) (i + step))
        Gtk.listBoxGetRowAtIndex list (fromIntegral j) >>= mapM_ (Gtk.listBoxSelectRow list . Just)
      offer codes = do
        writeIORef offered codes
        Gtk.listBoxRemoveAll list
        forM_ codes $ \(alias, emoji) -> do
          row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
          GI.new Gtk.Label [#label := emoji, #cssClasses := ["emoji"]] >>= #append row
          GI.new Gtk.Label [#label := ":" <> alias <> ":", #cssClasses := ["mono"]] >>= #append row
          #append list row
        Gtk.listBoxGetRowAtIndex list 0 >>= mapM_ (Gtk.listBoxSelectRow list . Just)
        (at, _) <- lineBefore
        place <- Gtk.textViewGetIterLocation view at
        px <- Gdk.getRectangleX place
        py <- Gdk.getRectangleY place
        (x, y) <- Gtk.textViewBufferToWindowCoords view Gtk.TextWindowTypeWidget px py
        h <- Gdk.getRectangleHeight place
        rect <- Gdk.newZeroRectangle
        set rect [#x := x, #y := y, #width := 1, #height := h]
        Gtk.popoverSetPointingTo popover (Just rect)
        Gtk.popoverPopup popover
      update = do
        (at, before) <- lineBefore
        case finished before of
          Just (name, emoji) -> hide >> replace (T.length name + 2) emoji
          Nothing -> do
            line <- Gtk.textIterGetLine at
            let start = (line, T.length before - maybe 0 T.length (typing before))
            away <- readIORef dismissed
            case typing before of
              Just name | away /= Just start, codes@(_ : _) <- matches name -> offer codes
              _ -> hide
            when (typing before == Nothing) (writeIORef dismissed Nothing)

  -- The buffer may not change while it reports a change, so the work waits
  -- for the main loop.
  void $ GI.on buffer #changed $
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE (update >> pure False)
  void $ GI.on list #rowActivated $ \row -> Gtk.listBoxRowGetIndex row >>= choose . fromIntegral

  -- The keys act only while the list shows, and pass through otherwise.
  keys <- GI.new Gtk.ShortcutController [#propagationPhase := Gtk.PropagationPhaseCapture]
  let whileShown trigger act = do
        parsed <- Gtk.shortcutTriggerParseString trigger
        callback <- Gtk.callbackActionNew $ \_ _ -> do
          open <- Gtk.widgetGetVisible popover
          when open act
          pure open
        Gtk.shortcutNew parsed (Just callback) >>= Gtk.shortcutControllerAddShortcut keys
  whileShown "Tab" (selected >>= choose)
  whileShown "Return" (selected >>= choose)
  whileShown "Down" (move 1)
  whileShown "Up" (move (-1))
  whileShown "Escape" $ do
    hide
    (at, before) <- lineBefore
    line <- Gtk.textIterGetLine at
    writeIORef dismissed (Just (line, T.length before - maybe 0 T.length (typing before)))
  Gtk.widgetAddController view keys
  -- A popover parented by hand goes when its parent does.
  void $ GI.on view #destroy (Gtk.widgetUnparent popover)

css :: Text
css = mconcat
  [ "popover.u-shortcodes > contents { padding: 4px; min-width: 220px; border-radius: 10px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: 0 8px 24px rgba(27,29,33,0.14); }"
  , "popover.u-shortcodes list.codes { background: transparent; }"
  , "popover.u-shortcodes list.codes row { padding: 4px 8px; border-radius: 6px; }"
  , "popover.u-shortcodes list.codes row:selected { background: var(--accent-bg); color: var(--text); }"
  , "popover.u-shortcodes label.emoji { font-size: 16px; }"
  , "popover.u-shortcodes label.mono { font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , "popover.u-shortcodes label.hint { padding: 6px 8px 2px; font-size: 12px; color: var(--text-muted); }"
  ]
