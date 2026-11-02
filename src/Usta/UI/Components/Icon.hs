{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Icons drawn from SVG source, including the logo.
--
-- Line icons use @currentColor@, so a 'Tone' (or any CSS @color@) recolors
-- them. Add an icon by adding a constructor and its SVG in 'source'.
module Usta.UI.Components.Icon
  ( Icon (..)
  , new
  , toned
  , css
  ) where

import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Usta.UI.Style (Tone, toneClass)

data Icon
  = Logo
  | PullRequest
  | Issue
  | Check
  | Cross
  | Skipped
  deriving (Eq, Show, Enum, Bounded)

-- | An icon at the given size in pixels, in the surrounding text color.
new :: Int32 -> Icon -> IO Gtk.Image
new size icon = do
  bytes <- GLib.bytesNew (Just (TE.encodeUtf8 (source icon)))
  svg <- Gtk.svgNewFromBytes bytes
  GI.new Gtk.Image [ #paintable := svg
                   , #pixelSize := size
                   , #halign := Gtk.AlignCenter
                   , #valign := Gtk.AlignCenter
                   , #cssClasses := ["u-icon"]
                   ]

-- | An icon colored by a tone, e.g. a green pull request icon.
toned :: Int32 -> Tone -> Icon -> IO Gtk.Image
toned size tone icon = do
  picture <- new size icon
  Gtk.widgetAddCssClass picture (toneClass tone)
  pure picture

source :: Icon -> Text
source icon = case icon of
  Logo -> svg64
    [ "<rect width='64' height='64' rx='14' fill='#2F5FD0'/>"
    , "<path d='M9 22H50Q56 22 56 28H43Q39 28 39 33V39H46V48H18V39H25V33Q25 28 20 28H18Q12 28 9 22Z' fill='#FFFFFF'/>"
    , "<path d='M44 15l3-5M36 14V8M28 15l-3-5' stroke='#F2C25C' stroke-width='3' stroke-linecap='round'/>"
    ]
  PullRequest -> line
    [ stroke "circle" "cx='6' cy='6' r='2.5'"
    , stroke "circle" "cx='6' cy='18' r='2.5'"
    , stroke "circle" "cx='18' cy='18' r='2.5'"
    , stroke "path" "d='M6 8.5v7M18 15.5V9a3 3 0 0 0-3-3h-4'"
    , stroke "path" "d='M13 3.5L10.5 6 13 8.5'"
    ]
  Issue -> line
    [ stroke "circle" "cx='12' cy='12' r='8.5'"
    , "<circle class='foreground-fill' gpa:fill='foreground'"
    , " fill='url(#gpa:foreground) rgb(0,0,0)' cx='12' cy='12' r='1.5'/>"
    ]
  Check -> line [ stroke "path" "d='M5 12l5 5 9-10'" ]
  Cross -> line [ stroke "path" "d='M6 6l12 12M18 6L6 18'" ]
  Skipped -> line
    [ stroke "circle" "cx='12' cy='12' r='8.5'"
    , stroke "path" "d='M8 12h8'"
    ]
  where
    svg64 parts = mconcat
      ([ "<svg xmlns='http://www.w3.org/2000/svg' width='64' height='64' viewBox='0 0 64 64'>" ]
       <> parts <> [ "</svg>" ])
    line parts = mconcat
      ([ "<svg xmlns='http://www.w3.org/2000/svg' xmlns:gpa='https://www.gtk.org/grappa'"
       , " gpa:version='1' width='24' height='24' viewBox='0 0 24 24'>" ]
       <> parts <> [ "</svg>" ])
    -- The same markup GTK's own symbolic icons use: the stroke is painted
    -- in the widget's CSS color.
    stroke element attrs = mconcat
      [ "<", element, " class='transparent-fill foreground-stroke' gpa:stroke='foreground'"
      , " fill='none' stroke='url(#gpa:foreground) rgb(0,0,0)'"
      , " stroke-width='2' stroke-linecap='round' stroke-linejoin='round' ", attrs, "/>" ]

css :: Text
css = mconcat
  [ "image.u-icon.tone-neutral { color: var(--text-soft); }"
  , "image.u-icon.tone-accent { color: var(--accent); }"
  , "image.u-icon.tone-success { color: var(--success); }"
  , "image.u-icon.tone-warning { color: var(--warning); }"
  , "image.u-icon.tone-danger { color: var(--danger); }"
  , "image.u-icon.tone-merged { color: var(--merged); }"
  ]
