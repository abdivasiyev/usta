{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Where syncing with the forges stands, shown in the header.
module Usta.UI.Components.SyncStatus
  ( State (..)
  , new
  , css
  ) where

import Control.Monad (void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import Usta.UI.Style (Tone (..))

type State :: Type
data State
  = Synced Text
    -- ^ When, e.g. "1 min ago".
  | Syncing Text
    -- ^ What, e.g. "3 of 5 repositories" or "github.com".
  | Offline
  | Failed

-- | @onRetry@ runs for the Retry button a failed sync shows.
new :: State -> IO () -> IO Gtk.Box
new state onRetry = do
  chip <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                         , #spacing := 8
                         , #valign := Gtk.AlignCenter
                         , #cssClasses := ["u-sync"]
                         ]
  case state of
    Syncing _ -> GI.new Gtk.Spinner [#spinning := True] >>= #append chip
    _ -> do
      dot <- Badge.dot (tone state)
      -- Offline is a resting state, quieter than the neutral text color.
      case state of
        Offline -> Gtk.widgetAddCssClass dot "quiet"
        _ -> pure ()
      #append chip dot
  GI.new Gtk.Label [#label := text state, #cssClasses := ["text", stateClass state]] >>= #append chip
  case state of
    Failed -> do
      retry <- Button.new Button.defaults { Button.label = Just "Retry" }
      Gtk.widgetAddCssClass retry "retry"
      void $ GI.on retry #clicked onRetry
      #append chip retry
    _ -> pure ()
  pure chip

tone :: State -> Tone
tone s = case s of
  Synced _ -> Success
  Syncing _ -> Accent
  Offline -> Neutral
  Failed -> Danger

text :: State -> Text
text s = case s of
  Synced when' -> "Synced " <> when'
  Syncing what -> "Syncing " <> what
  Offline -> "Offline · showing cached data"
  Failed -> "Sync failed"

stateClass :: State -> Text
stateClass s = case s of
  Failed -> "failed"
  _ -> "ok"

css :: Text
css = mconcat
  [ "box.u-sync label.text { font-size: 13px; color: var(--text-muted); }"
  , "box.u-sync label.text.failed { color: var(--danger); }"
  , "box.u-sync box.u-dot.quiet { background: var(--text-muted); }"
  , "box.u-sync spinner { min-width: 14px; min-height: 14px; color: var(--accent); }"
  , "box.u-sync button.retry { min-height: 26px; padding: 0 10px; font-size: 13px; }"
  ]
