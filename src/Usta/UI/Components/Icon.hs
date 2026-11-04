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
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Usta.UI.Style (Tone, toneClass)

type Icon :: Type
data Icon
  = Logo
  | PullRequest
  | Issue
  | Check
  | Cross
  | Skipped
  | Heading
  | Bold
  | Italic
  | Code
  | Link
  | Quote
  | Bullets
  | Numbers
  | Tasks
  | CodeBlock
  | Diagram
  | Smile
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
  Heading -> line [ stroke "path" "d='M6 5v14M18 5v14M6 12h12'" ]
  Bold -> line [ stroke "path" "d='M7 5h6a3.5 3.5 0 0 1 0 7H7zM7 12h7a3.5 3.5 0 0 1 0 7H7z'" ]
  Italic -> line [ stroke "path" "d='M10 5h8M6 19h8M14 5l-4 14'" ]
  Code -> line [ stroke "path" "d='M8 7l-5 5 5 5M16 7l5 5-5 5M14 4l-4 16'" ]
  Link -> line
    [ stroke "path" "d='M10 14a4 4 0 0 0 5.7 0l3-3a4 4 0 0 0-5.7-5.7l-1 1'"
    , stroke "path" "d='M14 10a4 4 0 0 0-5.7 0l-3 3a4 4 0 0 0 5.7 5.7l1-1'"
    ]
  Quote -> line [ stroke "path" "d='M5 5v14M10 8h9M10 12h9M10 16h6'" ]
  Bullets -> line
    [ stroke "path" "d='M10 7h10M10 12h10M10 17h10'"
    , stroke "circle" "cx='5' cy='7' r='1'"
    , stroke "circle" "cx='5' cy='12' r='1'"
    , stroke "circle" "cx='5' cy='17' r='1'"
    ]
  Numbers -> line [ stroke "path" "d='M11 7h9M11 12h9M11 17h9M4 5h1.5v5M4 14.5a1.5 1.5 0 0 1 3 0c0 1-3 2.5-3 3.5h3'" ]
  Tasks -> line
    [ stroke "rect" "x='4' y='4' width='16' height='16' rx='3'"
    , stroke "path" "d='M8 12l3 3 5-6'"
    ]
  CodeBlock -> line
    [ stroke "path" "d='M9 4c-2 0-3 1-3 3v2c0 1.5-1 3-2 3 1 0 2 1.5 2 3v2c0 2 1 3 3 3'"
    , stroke "path" "d='M15 4c2 0 3 1 3 3v2c0 1.5 1 3 2 3-1 0-2 1.5-2 3v2c0 2-1 3-3 3'"
    ]
  Diagram -> line [ stroke "path" "d='M2 12l4-4 4 4-4 4zM12 12h9M18 9l3 3-3 3'" ]
  Smile -> line
    [ stroke "circle" "cx='12' cy='12' r='8.5'"
    , stroke "path" "d='M8.5 14a4 4 0 0 0 7 0M9 9.5h.01M15 9.5h.01'"
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
