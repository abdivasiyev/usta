{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A Mermaid block in Markdown, drawn by usta as a diagram on the app's own
-- background, with its source a click away. A diagram usta cannot draw
-- shows its source and says why. Fullscreen shows the document's
-- diagrams one at a time over the window.
module Usta.UI.Components.Mermaid
  ( Diagram (..)
  , new
  , css
  ) where

import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import Usta.UI.Components.CommandPalette (shortcut)

import qualified Usta.Diagram as Diagram
import qualified Usta.UI.Components.DiagramView as DiagramView

-- | A diagram in a document: the heading above it and its source.
type Diagram :: Type
data Diagram = Diagram { title :: Text, source :: Text }

-- | The document's diagrams, in order, and this one's source.
new :: [Diagram] -> Text -> IO Gtk.Widget
new siblings source = do
  figure <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-mermaid"]]
  bar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["bar"]]
  GI.new Gtk.Label [#label := "Diagram · mermaid", #hexpand := True, #xalign := 0] >>= #append bar
  toggle <- GI.new Gtk.ToggleButton [#label := "Show source", #cssClasses := ["u-toggle"]]
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
    GI.set toggle [#label := if on then "Show diagram" else "Show source"]

  -- The source, with the reason under it, when there is no drawing.
  let fallBack reason = do
        problem <- GI.new Gtk.Label [#label := reason, #xalign := 0, #wrap := True, #cssClasses := ["problem"]]
        Gtk.boxAppend figure problem
        GI.set toggle [#visible := False]
        Gtk.stackSetVisibleChildName stack "source"
  case Diagram.parse source of
    Left reason -> fallBack reason
    -- Drawn by usta; the theme's colors come in when it is painted.
    Right d -> do
      let document = if null siblings then [Diagram {title = "Diagram", source}] else siblings
          at = fromMaybe 0 (findIndex (\x -> x.source == source) document)
          large = fullscreen figure document at
      DiagramView.new DiagramView.Config {large = False, fullscreen = Just large} (\measure -> Diagram.draw measure d)
        >>= #append diagram
  Gtk.toWidget figure

-- | A window over @from@'s showing the diagrams one at a time, from
-- @start@: @←@ and @→@ move between them, @Esc@ closes.
fullscreen :: Gtk.Box -> [Diagram] -> Int -> IO ()
fullscreen from diagrams start = do
  parent <- Gtk.widgetGetRoot from >>= maybe (pure Nothing) (GI.castTo Gtk.Window)
  (w, h) <- case parent of
    Just p -> (,) <$> Gtk.widgetGetWidth p <*> Gtk.widgetGetHeight p
    Nothing -> pure (1100, 760)
  win <- GI.new Gtk.Window [ #modal := True, #title := "Diagram", #defaultWidth := max 600 (w - 48)
                           , #defaultHeight := max 400 (h - 48), #cssClasses := ["u-diagram-full"] ]
  forM_ parent $ \p -> GI.set win [#transientFor := p]

  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["header"]]
  name <- GI.new Gtk.Label [#xalign := 0, #cssClasses := ["name"]]
  place <- GI.new Gtk.Label [#xalign := 0, #hexpand := True, #cssClasses := ["place"]]
  close <- GI.new Gtk.Button [#cssClasses := ["close"]]
  closeLabel <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  GI.new Gtk.Label [#label := "Close"] >>= #append closeLabel
  GI.new Gtk.Label [#label := "Esc", #cssClasses := ["key"]] >>= #append closeLabel
  GI.set close [#child := closeLabel]
  mapM_ (#append header) =<< sequence [Gtk.toWidget name, Gtk.toWidget place, Gtk.toWidget close]
  #append box header
  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #vexpand := True, #cssClasses := ["body"]]
  overlay <- GI.new Gtk.Overlay [#child := body]
  let several = length diagrams > 1
  hint <- GI.new Gtk.Label [ #label := if several then "Drag to pan · ← → other diagrams" else "Drag to pan"
                           , #halign := Gtk.AlignStart, #valign := Gtk.AlignEnd, #canTarget := False, #cssClasses := ["hint"] ]
  Gtk.overlayAddOverlay overlay hint
  #append box overlay
  GI.set win [#child := box]

  shown <- newIORef start
  let count = length diagrams
      showAt i = forM_ (lookup i (zip [0 ..] diagrams)) $ \d -> do
        writeIORef shown i
        GI.set name [#label := d.title]
        GI.set place [#label := if several then "diagram " <> num (i + 1) <> " of " <> num count else ""]
        Gtk.widgetGetFirstChild body >>= mapM_ (Gtk.boxRemove body)
        view <- case Diagram.parse d.source of
          Left reason -> GI.new Gtk.Label [#label := reason, #wrap := True, #cssClasses := ["problem"]] >>= Gtk.toWidget
          Right parsed -> DiagramView.new DiagramView.Config {large = True, fullscreen = Nothing} (\measure -> Diagram.draw measure parsed)
        #append body view
        -- The view takes focus, so its zoom keys work at once.
        Gtk.widgetGetFirstChild view >>= mapM_ Gtk.widgetGrabFocus
      move k = do
        i <- readIORef shown
        let j = i + k
        when (j >= 0 && j < count) (showAt j)
      num = T.pack . show
  void $ GI.on close #clicked (Gtk.windowDestroy win)
  -- Before the view, so arrows switch diagrams instead of moving focus.
  keys <- GI.new Gtk.ShortcutController [#propagationPhase := Gtk.PropagationPhaseCapture]
  shortcut keys "Escape" (Gtk.windowDestroy win)
  shortcut keys "Left" (move (-1))
  shortcut keys "Right" (move 1)
  Gtk.widgetAddController win keys
  showAt start
  Gtk.windowPresent win

-- | With the diagram view's, which only diagrams use.
css :: Text
css = DiagramView.css <> mconcat
  [ "box.u-mermaid { border: 1px solid var(--border); border-radius: 8px; }"
  , "box.u-mermaid box.bar { padding: 4px 6px 4px 12px; background: var(--window); border-bottom: 1px solid var(--border); font-size: 12px; color: var(--text-muted); }"
  , "box.u-mermaid box.diagram { padding: 16px; }"
  , "box.u-mermaid label.source { padding: 10px 14px; font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , "window.u-diagram-full box.header { padding: 10px 12px 10px 16px; border-bottom: 1px solid var(--border); background: var(--surface); }"
  , "window.u-diagram-full label.name { font-weight: 600; color: var(--text); }"
  , "window.u-diagram-full label.place { font-size: 12px; color: var(--text-muted); }"
  , "window.u-diagram-full label.key { font-family: 'JetBrains Mono', monospace; font-size: 11px; padding: 0 5px; border: 1px solid var(--border); border-radius: 4px; }"
  , "window.u-diagram-full box.body {"
  , "  background-color: var(--window);"
  , "  background-image: radial-gradient(circle, var(--border) 1px, transparent 1.5px);"
  , "  background-size: 16px 16px;"
  , "}"
  , "window.u-diagram-full label.hint { margin: 12px 16px; font-size: 12px; color: var(--text-muted); }"
  , "window.u-diagram-full label.problem { margin: 24px; color: var(--danger); }"
  , "box.u-mermaid label.problem { padding: 0 14px 10px; font-size: 12px; color: var(--danger); }"
  ]
