{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Bordered surfaces: a plain card, a stat card ("Checks / 1 of 1 passed")
-- and the small uppercase section title used above groups.
module Usta.UI.Components.Card
  ( new
  , stat
  , sectionTitle
  , css
  ) where

import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import Usta.UI.Style (Tone, toneClass)

-- | An empty vertical card; append rows or content to it.
new :: IO Gtk.Box
new = GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-card"]]

-- | A caption with a value below it, the value colored by the tone.
stat :: Tone -> Text -> Text -> IO Gtk.Box
stat tone caption value = do
  box <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                        , #spacing := 4
                        , #hexpand := True
                        , #cssClasses := ["u-card", "u-stat"]
                        ]
  captionLabel <- GI.new Gtk.Label [#label := caption, #xalign := 0, #cssClasses := ["caption"]]
  -- A long value ends in an ellipsis; the tooltip has all of it.
  valueLabel <- GI.new Gtk.Label [ #label := value
                                 , #xalign := 0
                                 , #ellipsize := Pango.EllipsizeModeEnd
                                 , #tooltipText := value
                                 , #cssClasses := ["value", toneClass tone]
                                 ]
  #append box captionLabel
  #append box valueLabel
  pure box

-- | "FILES", "FORGES", "PINNED REPOSITORIES".
sectionTitle :: Text -> IO Gtk.Label
sectionTitle text =
  GI.new Gtk.Label [#label := T.toUpper text, #xalign := 0, #cssClasses := ["u-section-title"]]

css :: Text
css = mconcat
  [ "box.u-card { background: var(--surface); border: 1px solid var(--border); border-radius: 8px; }"
  , "box.u-stat { padding: 12px; }"
  , "box.u-stat label.caption { font-size: 12px; color: var(--text-muted); }"
  , "box.u-stat label.value { font-weight: 600; }"
  , "box.u-stat label.value.tone-neutral { color: var(--text); }"
  , "box.u-stat label.value.tone-accent { color: var(--accent); }"
  , "box.u-stat label.value.tone-success { color: var(--success); }"
  , "box.u-stat label.value.tone-warning { color: var(--warning); }"
  , "box.u-stat label.value.tone-danger { color: var(--danger); }"
  , "box.u-stat label.value.tone-merged { color: var(--merged); }"
  , "label.u-section-title {"
  , "  font-size: 12px; font-weight: 600; color: var(--text-muted); letter-spacing: 0.5px;"
  , "}"
  ]
