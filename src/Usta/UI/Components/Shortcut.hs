{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Keyboard hints: a single key cap, and the shortcut strip shown at the
-- bottom of each screen.
module Usta.UI.Components.Shortcut
  ( key
  , bar
  , css
  ) where

import Control.Monad (forM_)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified GI.Gtk as Gtk

-- | A key cap such as "Ctrl K".
key :: Text -> IO Gtk.Label
key text = GI.new Gtk.Label [#label := text, #cssClasses := ["u-key"]]

-- | The footer strip: @(keys, action)@ pairs, and optional text on the right.
bar :: [(Text, Text)] -> Maybe Text -> IO Gtk.Box
bar shortcuts trailing = do
  strip <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 20
                          , #cssClasses := ["u-shortcuts"]
                          ]
  forM_ shortcuts $ \(keys, action) -> do
    item <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 4]
    keysLabel <- GI.new Gtk.Label [#label := keys, #cssClasses := ["keys"]]
    actionLabel <- GI.new Gtk.Label [#label := action]
    #append item keysLabel
    #append item actionLabel
    #append strip item
  forM_ trailing $ \text -> do
    end <- GI.new Gtk.Label [#label := text, #hexpand := True, #halign := Gtk.AlignEnd]
    #append strip end
  pure strip

css :: Text
css = mconcat
  [ "label.u-key {"
  , "  font-family: 'JetBrains Mono', monospace; font-size: 12px; color: var(--text-muted);"
  , "  padding: 1px 6px; border: 1px solid var(--input-border); border-radius: 4px;"
  , "  background: var(--surface);"
  , "}"
  , "box.u-shortcuts {"
  , "  padding: 8px 16px; background: var(--surface); border-top: 1px solid var(--border);"
  , "  font-size: 12px; color: var(--text-muted);"
  , "}"
  , "box.u-shortcuts label.keys {"
  , "  font-family: 'JetBrains Mono', monospace; font-weight: 700; color: var(--text);"
  , "}"
  ]
