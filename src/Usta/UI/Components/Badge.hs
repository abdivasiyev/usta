{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Small status markers: a rounded badge with text, and a colored dot.
module Usta.UI.Components.Badge
  ( new
  , dot
  , css
  ) where

import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import Usta.UI.Style (Tone, toneClass)

-- | A pill such as "Review requested" or "Open".
new :: Tone -> Text -> IO Gtk.Label
new tone text =
  GI.new Gtk.Label [#label := text, #cssClasses := ["u-badge", toneClass tone]]

-- | A small round marker, for unread items, forges and sync state.
dot :: Tone -> IO Gtk.Box
dot tone =
  GI.new Gtk.Box [ #cssClasses := ["u-dot", toneClass tone]
                 , #valign := Gtk.AlignCenter
                 , #halign := Gtk.AlignCenter
                 ]

css :: Text
css = mconcat
  [ "label.u-badge { font-size: 12px; padding: 2px 8px; border-radius: 999px; }"
  , "label.u-badge.tone-neutral { background: var(--neutral-bg); color: var(--text-soft); }"
  , "label.u-badge.tone-accent { background: var(--accent-bg); color: var(--accent-soft); }"
  , "label.u-badge.tone-success { background: var(--success-bg); color: var(--success); font-weight: 500; }"
  , "label.u-badge.tone-warning { background: var(--warning-bg); color: var(--warning); }"
  , "label.u-badge.tone-danger { background: var(--danger-bg); color: var(--danger); }"
  , "label.u-badge.tone-merged { background: var(--merged-bg); color: var(--merged); }"
  , "box.u-dot { min-width: 8px; min-height: 8px; border-radius: 999px; }"
  , "box.u-dot.tone-neutral { background: var(--text); }"
  , "box.u-dot.tone-accent { background: var(--accent); }"
  , "box.u-dot.tone-success { background: var(--success); }"
  , "box.u-dot.tone-warning { background: var(--warning); }"
  , "box.u-dot.tone-danger { background: var(--danger); }"
  , "box.u-dot.tone-merged { background: var(--merged); }"
  ]
