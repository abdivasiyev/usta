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
  , css
  ) where

import Control.Monad (forM_)
import Data.GI.Base (AttrOp ((:=)), set)
import qualified Data.GI.Base as GI
import Data.Kind (Type)
import Data.Text (Text)
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
  ]
