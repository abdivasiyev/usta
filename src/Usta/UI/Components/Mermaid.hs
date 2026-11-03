{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A Mermaid block in Markdown, drawn as a diagram on the app's own
-- background, with its source a click away. A diagram usta cannot draw
-- shows its source and says why.
module Usta.UI.Components.Mermaid
  ( new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as BL
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.Mermaid as Mermaid
import qualified Usta.UI.Components.ImageView as ImageView
import qualified Usta.UI.Components.Skeleton as Skeleton
import qualified Usta.UI.Highlight as Highlight

new :: Text -> IO Gtk.Widget
new source = do
  figure <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-mermaid"]]
  bar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["bar"]]
  GI.new Gtk.Label [#label := "Diagram · mermaid", #hexpand := True, #xalign := 0] >>= #append bar
  toggle <- GI.new Gtk.ToggleButton [#label := "Show source", #cssClasses := ["toggle"]]
  #append bar toggle
  #append figure bar

  stack <- GI.new Gtk.Stack []
  diagram <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["diagram"]]
  void $ Gtk.stackAddNamed stack diagram (Just "diagram")
  text <- GI.new Gtk.Label [#label := source, #xalign := 0, #selectable := True, #cssClasses := ["source"]]
  void $ Gtk.stackAddNamed stack text (Just "source")
  #append figure stack
  void $ GI.on toggle #toggled $ do
    on <- GI.get toggle #active
    Gtk.stackSetVisibleChildName stack (if on then "source" else "diagram")

  -- The source, with the reason under it, when there is no drawing.
  let fallBack reason = do
        problem <- GI.new Gtk.Label [#label := reason, #xalign := 0, #wrap := True, #cssClasses := ["problem"]]
        Gtk.boxAppend figure problem
        GI.set toggle [#visible := False]
        Gtk.stackSetVisibleChildName stack "source"
  case Mermaid.parse source of
    Left reason -> fallBack reason
    Right chart ->
      -- Drawn again in the other theme's colors when it changes.
      Highlight.follow figure $ \palette -> do
        clear diagram
        Skeleton.block 120 >>= #append diagram
        void $ forkIO $ do
          drawn <- Mermaid.draw (colors palette) chart
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            clear diagram
            case drawn of
              Left reason -> fallBack reason
              Right png -> do
                image <- ImageView.texture (BL.toStrict png)
                case image of
                  Nothing -> fallBack "The diagram was drawn in a form usta cannot show."
                  Just t -> do
                    -- dot drew it at twice the screen's resolution.
                    (w, h) <- ImageView.size t
                    picture <- GI.new Gtk.Picture [ #paintable := t
                                                  , #canShrink := True
                                                  , #contentFit := Gtk.ContentFitScaleDown
                                                  , #widthRequest := fromIntegral (w `div` 2)
                                                  , #heightRequest := fromIntegral (h `div` 2)
                                                  , #halign := Gtk.AlignStart
                                                  ]
                    #append diagram picture
            pure False
  Gtk.toWidget figure
  where
    clear box = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear box)
    colors palette = case palette of
      Highlight.LightPalette -> Mermaid.Colors {fill = "#FFFFFF", border = "#B9C0CB", text = "#1B1D21", line = "#5B616B"}
      Highlight.DarkPalette -> Mermaid.Colors {fill = "#23262B", border = "#4A505A", text = "#E6E8EB", line = "#9AA0A9"}

css :: Text
css = mconcat
  [ "box.u-mermaid { border: 1px solid var(--border); border-radius: 8px; }"
  , "box.u-mermaid box.bar { padding: 4px 6px 4px 12px; background: var(--window); border-bottom: 1px solid var(--border); font-size: 12px; color: var(--text-muted); }"
  , "box.u-mermaid button.toggle { min-height: 26px; padding: 0 10px; font-size: 12px; }"
  , "box.u-mermaid box.diagram { padding: 16px; }"
  , "box.u-mermaid label.source { padding: 10px 14px; font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , "box.u-mermaid label.problem { padding: 0 14px 10px; font-size: 12px; color: var(--danger); }"
  ]
