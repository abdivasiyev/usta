{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Placeholders shaped like the content that is on its way. They appear
-- only when nothing is cached yet, and match the real layout so nothing
-- jumps when the data lands.
module Usta.UI.Components.Skeleton
  ( bar
  , fill
  , circle
  , block
  , listRow
  , detail
  , delayed
  , appear
  , css
  ) where

import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.Text (Text)
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

-- | A grey bar of a fixed size.
bar :: Int32 -> Int32 -> IO Gtk.Widget
bar width height = shape ["u-skeleton"] width height False

-- | A grey bar as wide as its container.
fill :: Int32 -> IO Gtk.Widget
fill height = shape ["u-skeleton"] (-1) height True

-- | A grey dot, where an avatar or icon will be.
circle :: Int32 -> IO Gtk.Widget
circle size = shape ["u-skeleton", "circle"] size size False

-- | A full-width block, where a card will be.
block :: Int32 -> IO Gtk.Widget
block height = shape ["u-skeleton", "block"] (-1) height True

shape :: [Text] -> Int32 -> Int32 -> Bool -> IO Gtk.Widget
shape classes width height grow = do
  box <- GI.new Gtk.Box [ #widthRequest := width
                        , #heightRequest := height
                        , #hexpand := grow
                        , #halign := if grow then Gtk.AlignFill else Gtk.AlignStart
                        , #valign := Gtk.AlignStart
                        , #cssClasses := classes
                        ]
  Gtk.toWidget box

-- | A list row: icon, repository, title, label, and time on the right.
-- @titleWidth@ varies the title bar so a column of rows looks natural.
listRow :: Int32 -> IO Gtk.Widget
listRow titleWidth = do
  row <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 12
                        , #cssClasses := ["u-skeleton-row"]
                        ]
  circle 16 >>= #append row
  lines' <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 9
                           , #hexpand := True
                           ]
  forM_ [bar 160 10, bar titleWidth 12, pill] $ \make -> make >>= #append lines'
  #append row lines'
  bar 28 10 >>= #append row
  Gtk.toWidget row
  where
    pill = do
      p <- bar 96 14
      Gtk.widgetAddCssClass p "pill"
      pure p

-- | A detail pane: meta line, title, two stat cards and a few lines.
detail :: IO Gtk.Widget
detail = do
  pane <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 14]
  bar 140 10 >>= #append pane
  bar 300 18 >>= #append pane
  cards <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 12
                          , #homogeneous := True
                          ]
  forM_ [1 :: Int, 2] $ \_ -> block 54 >>= #append cards
  #append pane cards
  fill 10 >>= #append pane
  bar 330 10 >>= #append pane
  bar 210 10 >>= #append pane
  Gtk.toWidget pane

-- | Hides @widget@ and shows it after 300 ms, so a fast load never
-- flickers a skeleton. Replace it with the content before then and it is
-- never seen.
delayed :: Gtk.Widget -> IO ()
delayed widget = do
  Gtk.widgetSetVisible widget False
  void $ GLib.timeoutAdd GLib.PRIORITY_DEFAULT 300 $ do
    Gtk.widgetSetVisible widget True
    pure False

-- | Fades @widget@ in, for content that takes a skeleton's place, so the
-- swap is not a jump. GTK skips it when animations are turned off.
appear :: Gtk.IsWidget w => w -> IO ()
appear widget = Gtk.widgetAddCssClass widget "u-appear"

css :: Text
css = mconcat
  -- A slow pulse; GTK stops it when animations are turned off.
  [ "@keyframes u-shimmer { 0% { opacity: 1; } 50% { opacity: 0.55; } 100% { opacity: 1; } }"
  , "box.u-skeleton {"
  , "  background: var(--neutral-bg); border-radius: 5px;"
  , "  animation: u-shimmer 1.6s ease-in-out infinite;"
  , "}"
  , "box.u-skeleton.circle { border-radius: 999px; }"
  , "box.u-skeleton.pill { border-radius: 999px; }"
  , "box.u-skeleton.block { border-radius: 8px; }"
  , "box.u-skeleton-row { padding: 14px 16px; border-bottom: 1px solid var(--border-subtle); }"
  , "@keyframes u-appear { from { opacity: 0; } to { opacity: 1; } }"
  , ".u-appear { animation: u-appear 180ms ease-out; }"
  ]
