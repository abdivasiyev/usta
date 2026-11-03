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
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.Segmented as Segmented

type Config :: Type
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
  stack <- GI.new Gtk.Stack []
  void $ Gtk.stackAddNamed stack view (Just "write")
  void $ Gtk.stackAddNamed stack rendered (Just "preview")
  #append box stack

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

  forM_ cfg.hint $ \h ->
    GI.new Gtk.Label [#label := h, #xalign := 0, #cssClasses := ["hint"]] >>= #append box
  pure (box, buffer)

css :: Text
css = mconcat
  [ "box.u-markdown-editor textview.editor {"
  , "  border: 1px solid var(--input-border); border-radius: 0 8px 8px 8px;"
  , "  font-family: 'JetBrains Mono', monospace; font-size: 13px;"
  , "}"
  , "box.u-markdown-editor textview.editor text { background: var(--surface); color: var(--text); }"
  , "box.u-markdown-editor box.preview {"
  , "  padding: 12px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface);"
  , "}"
  , "box.u-markdown-editor label.hint { font-size: 12px; color: var(--text-muted); }"
  ]
