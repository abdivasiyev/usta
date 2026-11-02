{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Progress for work that takes a while: a percentage when the size is
-- known, a moving bar when it is not.
module Usta.UI.Components.Progress
  ( Part (..)
  , Amount (..)
  , bar
  , caption
  , new
  , css
  ) where

import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

-- | A piece of a caption: plain text, or a branch or path in code font.
data Part = Plain Text | Code Text

-- | How much is done, when that is known.
data Amount = Percent Double | Unknown

-- | Just the bar. An unknown amount moves back and forth while it is on
-- screen, unless animations are turned off.
bar :: Amount -> IO Gtk.Widget
bar amount = do
  progress <- GI.new Gtk.ProgressBar [#hexpand := True, #cssClasses := ["u-progress"]]
  case amount of
    Percent p -> GI.set progress [#fraction := p / 100]
    Unknown -> do
      Gtk.widgetAddCssClass progress "unknown"
      GI.set progress [#pulseStep := 0.04]
      settings <- Gtk.settingsGetDefault
      animate <- maybe (pure True) (`GI.get` #gtkEnableAnimations) settings
      frames <- newIORef (0 :: Int)
      -- The tick callback runs only while the bar is drawn.
      void $ Gtk.widgetAddTickCallback progress $ \_ _ -> do
        modifyIORef' frames (+ 1)
        n <- readIORef frames
        if animate && n `mod` 4 == 0 then Gtk.progressBarPulse progress else pure ()
        pure True
  Gtk.toWidget progress

-- | A one-line caption built from parts.
caption :: [Part] -> IO Gtk.Box
caption parts = do
  line <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  forM_ parts $ \part -> do
    let (text, classes) = case part of
          Plain t -> (t, [])
          Code t -> (t, ["mono"])
    GI.new Gtk.Label [#label := text, #cssClasses := classes] >>= #append line
  pure line

-- | The caption above, the percentage on its right when known, the bar
-- below.
new :: [Part] -> Amount -> IO Gtk.Box
new parts amount = do
  box <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                        , #spacing := 8
                        , #cssClasses := ["u-progress-block"]
                        ]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  text <- caption parts
  GI.set text [#hexpand := True]
  #append top text
  case amount of
    Percent p -> GI.new Gtk.Label [ #label := T.pack (show (round p :: Int)) <> "%"
                                  , #cssClasses := ["mono", "percent"]
                                  ] >>= #append top
    Unknown -> pure ()
  #append box top
  bar amount >>= #append box
  pure box

css :: Text
css = mconcat
  [ "progressbar.u-progress trough {"
  , "  min-height: 6px; border-radius: 999px; background: var(--neutral-bg); border: none;"
  , "}"
  , "progressbar.u-progress progress {"
  , "  min-height: 6px; border-radius: 999px; background: var(--accent); border: none;"
  , "}"
  , "progressbar.u-progress.unknown trough, progressbar.u-progress.unknown progress { min-height: 3px; }"
  , "box.u-progress-block label.percent { color: var(--text-soft); }"
  ]
