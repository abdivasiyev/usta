{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Buttons: a label, an icon, or both.
--
-- > Button.new Button.defaults { Button.label = Just "Open review", Button.variant = Button.Primary }
module Usta.UI.Components.Button
  ( Variant (..)
  , Config (..)
  , defaults
  , new
  , busy
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, void)
import Data.GI.Base (AttrOp ((:=)), set)
import qualified Data.GI.Base as GI
import Data.Kind (Type)
import Data.Text (Text)
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

type Variant :: Type
data Variant
  = Primary   -- ^ The one main action on a screen.
  | Secondary -- ^ Bordered, for the other actions.
  | Flat      -- ^ No background, for toolbars and icon buttons.
  | Danger    -- ^ Destructive actions.
  deriving (Eq, Show, Enum, Bounded)

type Config :: Type
data Config = Config
  { label :: Maybe Text
  , icon :: Maybe Text
    -- ^ A symbolic icon name from the icon theme.
  , variant :: Variant
  , tooltip :: Maybe Text
  }

defaults :: Config
defaults = Config
  { label = Nothing
  , icon = Nothing
  , variant = Secondary
  , tooltip = Nothing
  }

new :: Config -> IO Gtk.Button
new cfg = do
  button <- GI.new Gtk.Button [#cssClasses := ["u-button", variantClass cfg.variant]]
  case (cfg.icon, cfg.label) of
    (Just name, Nothing) -> do
      set button [#iconName := name]
      Gtk.widgetAddCssClass button "icon-only"
    (Nothing, Just text) -> set button [#label := text]
    (Just name, Just text) -> do
      image <- GI.new Gtk.Image [#iconName := name]
      caption <- GI.new Gtk.Label [#label := text]
      box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
      #append box image
      #append box caption
      set button [#child := box]
    (Nothing, Nothing) -> pure ()
  forM_ cfg.tooltip $ \t -> set button [#tooltipText := t]
  pure button

-- | Runs @act@ off the main thread while the button waits for it: it
-- keeps its width, shows a spinner and @doing@ (such as "Commenting…") and
-- cannot be pressed, and @others@ (the buttons beside it) wait too, so
-- nothing is sent twice. Then the button comes back as it was and @done@
-- gets the result on the main thread.
busy :: Gtk.IsWidget w => Gtk.Button -> [w] -> Text -> IO a -> (a -> IO ()) -> IO ()
busy button others doing act done = do
  before <- Gtk.buttonGetChild button
  width <- Gtk.widgetGetWidth button
  spinner <- GI.new Gtk.Spinner [#spinning := True]
  caption <- GI.new Gtk.Label [#label := doing]
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignCenter]
  #append box spinner
  #append box caption
  set button [#child := box, #widthRequest := width, #sensitive := False]
  Gtk.widgetAddCssClass button "busy"
  forM_ others (\w -> Gtk.widgetSetSensitive w False)
  void $ forkIO $ do
    result <- act
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      Gtk.buttonSetChild button before
      set button [#widthRequest := -1, #sensitive := True]
      Gtk.widgetRemoveCssClass button "busy"
      forM_ others (\w -> Gtk.widgetSetSensitive w True)
      done result
      pure False

variantClass :: Variant -> Text
variantClass v = case v of
  Primary -> "u-primary"
  Secondary -> "u-secondary"
  Flat -> "u-flat"
  Danger -> "u-danger"

css :: Text
css = mconcat
  [ "button.u-button {"
  , "  min-height: 40px; padding: 0 16px; border-radius: 8px;"
  , "  font-weight: 500; box-shadow: none; background-image: none;"
  , "}"
  , "button.u-button.icon-only { min-width: 36px; min-height: 36px; padding: 0; }"
  , "button.u-primary { background: var(--accent); color: var(--on-accent); border: none; font-weight: 600; }"
  , "button.u-primary:hover { opacity: 0.92; }"
  , "button.u-secondary { background: var(--surface); color: var(--text); border: 1px solid var(--input-border); }"
  , "button.u-secondary:hover { background: var(--border-subtle); }"
  , "button.u-flat { background: transparent; color: var(--text-soft); border: none; }"
  , "button.u-flat:hover { background: var(--border-subtle); }"
  , "button.u-danger { background: var(--surface); color: var(--danger); border: 1px solid var(--danger-border); }"
  , "button.u-danger:hover { background: var(--danger-bg); }"
  , "button.u-button:disabled { background: var(--border); color: var(--text-muted); border: none; }"
  -- A busy button keeps its look, so it reads as working, not refused.
  , "button.u-primary.busy:disabled { background: var(--accent); color: var(--on-accent); }"
  , "button.u-secondary.busy:disabled { background: var(--surface); color: var(--text-soft); border: 1px solid var(--input-border); }"
  , "button.u-danger.busy:disabled { background: var(--surface); color: var(--danger); border: 1px solid var(--danger-border); }"
  , "button.u-flat.busy:disabled { background: transparent; color: var(--text-soft); }"
  , "button.u-button.busy spinner, button.u-button.busy spinner:disabled { min-width: 14px; min-height: 14px; color: var(--accent); opacity: 1; }"
  , "button.u-primary.busy spinner, button.u-primary.busy spinner:disabled { color: var(--on-accent); }"
  ]
