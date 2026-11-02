{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A text area for Markdown with Write and Preview tabs, used for
-- descriptions and comments.
module Usta.UI.Components.MarkdownEditor
  ( Config (..)
  , defaults
  , new
  , css
  ) where

import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Segmented as Segmented

data Config = Config
  { text :: Text
  , hint :: Maybe Text
    -- ^ Shown under the editor, e.g. "Markdown supported".
  , height :: Int32
  }

defaults :: Config
defaults = Config { text = "", hint = Nothing, height = 120 }

-- | The editor and its buffer, to read what was written.
new :: Config -> IO (Gtk.Box, Gtk.TextBuffer)
new cfg = do
  box <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                        , #spacing := 6
                        , #cssClasses := ["u-markdown-editor"]
                        ]
  (tabs, buttons) <- Segmented.new ["Write", "Preview"] 0
  #append box tabs

  buffer <- GI.new Gtk.TextBuffer []
  Gtk.textBufferSetText buffer (text cfg) (-1)
  view <- GI.new Gtk.TextView [ #buffer := buffer
                              , #wrapMode := Gtk.WrapModeWordChar
                              , #heightRequest := height cfg
                              , #topMargin := 12
                              , #bottomMargin := 12
                              , #leftMargin := 12
                              , #rightMargin := 12
                              , #cssClasses := ["editor"]
                              ]
  rendered <- GI.new Gtk.Label [ #wrap := True
                               , #xalign := 0
                               , #yalign := 0
                               , #heightRequest := height cfg
                               , #cssClasses := ["preview"]
                               ]
  stack <- GI.new Gtk.Stack []
  void $ Gtk.stackAddNamed stack view (Just "write")
  void $ Gtk.stackAddNamed stack rendered (Just "preview")
  #append box stack

  -- Preview shows the text as it will read; Markdown rendering comes later.
  forM_ (zip ["write", "preview"] buttons) $ \(name, button) ->
    void $ GI.on button #toggled $ do
      active <- GI.get button #active
      when active $ do
        when (name == "preview") $ do
          start <- Gtk.textBufferGetStartIter buffer
          end <- Gtk.textBufferGetEndIter buffer
          current <- Gtk.textBufferGetText buffer start end False
          GI.set rendered [#label := current]
        Gtk.stackSetVisibleChildName stack name

  forM_ (hint cfg) $ \h ->
    GI.new Gtk.Label [#label := h, #xalign := 0, #cssClasses := ["hint"]] >>= #append box
  pure (box, buffer)

css :: Text
css = mconcat
  [ "box.u-markdown-editor textview.editor {"
  , "  border: 1px solid var(--input-border); border-radius: 0 8px 8px 8px;"
  , "  font-family: 'JetBrains Mono', monospace; font-size: 13px;"
  , "}"
  , "box.u-markdown-editor textview.editor text { background: var(--surface); color: var(--text); }"
  , "box.u-markdown-editor label.preview {"
  , "  padding: 12px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface);"
  , "}"
  , "box.u-markdown-editor label.hint { font-size: 12px; color: var(--text-muted); }"
  ]
