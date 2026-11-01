{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A round avatar with initials.
module Usta.UI.Components.Avatar
  ( new
  , css
  ) where

import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

-- | @new 32 "AA"@ is the header avatar; 24 and 28 are used in threads.
new :: Int32 -> Text -> IO Gtk.Label
new size initials =
  GI.new Gtk.Label [ #label := initials
                   , #widthRequest := size
                   , #heightRequest := size
                   , #halign := Gtk.AlignCenter
                   , #valign := Gtk.AlignCenter
                   , #cssClasses := ["u-avatar"]
                   ]

css :: Text
css = mconcat
  [ "label.u-avatar {"
  , "  border-radius: 999px; background: var(--accent-bg); color: var(--accent-soft);"
  , "  font-weight: 600; font-size: 12px;"
  , "}"
  ]
