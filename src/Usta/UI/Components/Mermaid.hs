{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A Mermaid block in Markdown, drawn by usta as a diagram on the app's own
-- background, with its source a click away. A diagram usta cannot draw
-- shows its source and says why.
module Usta.UI.Components.Mermaid
  ( new
  , css
  ) where

import Control.Monad (void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.Diagram.Flowchart as Flowchart
import qualified Usta.Mermaid as Mermaid
import qualified Usta.UI.Components.DiagramView as DiagramView

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
    -- Drawn by usta; the theme's colors come in when it is painted.
    Right chart -> DiagramView.new (\measure -> Flowchart.draw measure chart) >>= #append diagram
  Gtk.toWidget figure

css :: Text
css = mconcat
  [ "box.u-mermaid { border: 1px solid var(--border); border-radius: 8px; }"
  , "box.u-mermaid box.bar { padding: 4px 6px 4px 12px; background: var(--window); border-bottom: 1px solid var(--border); font-size: 12px; color: var(--text-muted); }"
  , "box.u-mermaid button.toggle { min-height: 26px; padding: 0 10px; font-size: 12px; }"
  , "box.u-mermaid box.diagram { padding: 16px; }"
  , "box.u-mermaid label.source { padding: 10px 14px; font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , "box.u-mermaid label.problem { padding: 0 14px 10px; font-size: 12px; color: var(--danger); }"
  ]
