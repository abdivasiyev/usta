{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The header's chip for what waits to be sent, "Offline · 2 waiting":
-- its popover lists each, in order, with × to discard it, and Retry now.
module Usta.UI.Components.WaitingChip
  ( new
  , css
  ) where

import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button

-- | @waiting@ described oldest first; @offline@ when the forge does not
-- answer; @discard@ takes one by its place; @retry@ sends them all.
new :: [Text] -> Bool -> (Int -> IO ()) -> IO () -> IO Gtk.Widget
new waiting offline discard retry = do
  popover <- GI.new Gtk.Popover [#cssClasses := ["u-waiting-popover"]]
  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #widthRequest := 340]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  GI.new Gtk.Image [#iconName := iconOf offline] >>= #append top
  label (headline <> " to send") ["title"] >>= #append top
  #append body top
  note <- label "They go out in order as soon as the forge answers again. Nothing is lost when usta closes." ["muted"]
  GI.set note [#wrap := True, #maxWidthChars := 40]
  #append body note
  forM_ (zip [0 ..] waiting) $ \(k, what) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["waiting"]]
    name <- label what []
    GI.set name [#hexpand := True]
    #append row name
    x <- Button.new Button.defaults {Button.label = Just "×", Button.variant = Button.Flat, Button.small = True, Button.tooltip = Just "Discard it"}
    void $ GI.on x #clicked (Gtk.popoverPopdown popover >> discard k)
    #append row x
    #append body row
  again <- Button.new Button.defaults {Button.label = Just "Retry now", Button.small = True}
  GI.set again [#halign := Gtk.AlignEnd]
  void $ GI.on again #clicked (Gtk.popoverPopdown popover >> retry)
  #append body again
  GI.set popover [#child := body]

  chip <- Button.menu Button.defaults {Button.variant = Button.Flat, Button.small = True} popover
  inside <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  GI.new Gtk.Image [#iconName := iconOf offline] >>= #append inside
  label headline [] >>= #append inside
  GI.set chip [#child := inside]
  Gtk.widgetAddCssClass chip "u-waiting-chip"
  Gtk.toWidget chip
  where
    count = T.pack (show (length waiting)) <> " waiting"
    headline = if offline then "Offline · " <> count else count
    iconOf o = if o then "network-offline-symbolic" else "mail-send-symbolic"

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "menubutton.u-waiting-chip > button { border-radius: 14px; background: var(--neutral-bg); padding: 2px 12px; }"
  , "popover.u-waiting-popover > contents { padding: 14px; }"
  , "popover.u-waiting-popover label.title { font-weight: 600; }"
  , "popover.u-waiting-popover box.waiting { padding: 8px 6px 8px 12px; border: 1px solid var(--border); border-radius: 8px; }"
  ]
