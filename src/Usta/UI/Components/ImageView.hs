{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Images from a repository: one on a checkerboard, so transparency
-- shows, fitted to the space or at its own size; or two, before and
-- after a change.
module Usta.UI.Components.ImageView
  ( texture
  , size
  , new
  , pair
  , css
  ) where

import Control.Exception (try)
import Control.Monad (forM_, void, when)
import Data.ByteString (ByteString)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)), GError)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

-- | The image in @bytes@, in any format GDK reads (PNG, JPEG, GIF, WebP,
-- SVG and more through its loaders); 'Nothing' when it reads none.
texture :: ByteString -> IO (Maybe Gdk.Texture)
texture bytes = do
  loaded <- try (GLib.bytesNew (Just bytes) >>= Gdk.textureNewFromBytes)
  pure (either (\(_ :: GError) -> Nothing) Just loaded)

-- | Width and height in pixels.
size :: Gdk.Texture -> IO (Int, Int)
size image = do
  w <- Gdk.textureGetWidth image
  h <- Gdk.textureGetHeight image
  pure (fromIntegral w, fromIntegral h)

-- | The image, with Fit and 100% above it.
new :: Gdk.Texture -> IO Gtk.Widget
new image = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-image"]]
  bar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 0, #halign := Gtk.AlignEnd, #cssClasses := ["zoom"]]
  picture <- GI.new Gtk.Picture [ #paintable := image
                                , #contentFit := Gtk.ContentFitScaleDown
                                , #canShrink := True
                                , #cssClasses := ["picture"]
                                ]
  holder <- GI.new Gtk.ScrolledWindow [ #child := picture
                                      , #propagateNaturalHeight := True
                                      , #propagateNaturalWidth := True
                                      , #maxContentHeight := 560
                                      , #cssClasses := ["checker"]
                                      ]
  -- Fit shrinks a large image into the space; 100% shows it as it is and
  -- scrolls.
  buttons <- mapM (\(text, fit) -> do
      b <- GI.new Gtk.ToggleButton [#label := text, #active := fit]
      void $ GI.on b #toggled $ do
        active <- GI.get b #active
        when active $ GI.set picture [#canShrink := fit]
      #append bar b
      pure b) [("Fit", True), ("100%", False)]
  case buttons of
    first : rest -> forM_ rest $ \b -> GI.set b [#group := first]
    [] -> pure ()
  #append box bar
  #append box holder
  Gtk.toWidget box

-- | An image before and after a change; a side that does not exist (an
-- added or deleted image) is left out.
pair :: Maybe Gdk.Texture -> Maybe Gdk.Texture -> IO Gtk.Widget
pair before after = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #homogeneous := True, #hexpand := True, #cssClasses := ["u-image-pair"]]
  forM_ [(before, "Before", "before"), (after, "After", "after")] $ \(side, caption, kind) ->
    forM_ side $ \image -> do
      column <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8, #cssClasses := ["side", "checker"]]
      picture <- GI.new Gtk.Picture [ #paintable := image
                                    , #contentFit := Gtk.ContentFitScaleDown
                                    , #canShrink := True
                                    , #halign := Gtk.AlignCenter
                                    , #valign := Gtk.AlignStart
                                    , #cssClasses := ["picture", kind]
                                    ]
      #append column picture
      (w, h) <- size image
      GI.new Gtk.Label [ #label := caption <> " · " <> T.pack (show w) <> " × " <> T.pack (show h)
                       , #xalign := 0
                       , #halign := Gtk.AlignCenter
                       , #cssClasses := ["caption", kind]
                       ] >>= #append column
      #append box column
  Gtk.toWidget box

css :: Text
css = mconcat
  [ ".checker {"
  , "  background-color: var(--surface);"
  , "  background-image: linear-gradient(45deg, var(--neutral-bg) 25%, transparent 25%),"
  , "    linear-gradient(-45deg, var(--neutral-bg) 25%, transparent 25%),"
  , "    linear-gradient(45deg, transparent 75%, var(--neutral-bg) 75%),"
  , "    linear-gradient(-45deg, transparent 75%, var(--neutral-bg) 75%);"
  , "  background-size: 16px 16px; background-position: 0 0, 0 8px, 8px -8px, -8px 0;"
  , "}"
  , "box.u-image box.zoom { padding: 8px 12px; }"
  , "box.u-image box.zoom button { min-height: 28px; padding: 0 10px; font-size: 12px; }"
  , "box.u-image picture.picture { margin: 24px; }"
  , "box.u-image-pair box.side { padding: 14px; }"
  , "box.u-image-pair picture.before { border: 2px solid var(--danger-border); }"
  , "box.u-image-pair picture.after { border: 2px solid var(--success); }"
  , "box.u-image-pair label.caption { font-size: 12px; padding: 1px 6px; border-radius: 4px; background: var(--surface); }"
  , "box.u-image-pair label.caption.before { color: var(--danger); }"
  , "box.u-image-pair label.caption.after { color: var(--success); }"
  ]
