{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Where syncing with the forges stands, shown in the header.
module Usta.UI.Components.SyncStatus
  ( State (..)
  , new
  , css
  ) where

import Control.Monad (when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Badge as Badge
import Usta.UI.Style (Tone (..))

type State :: Type
data State
  = Synced Text
    -- ^ When, e.g. "1 min ago".
  | Syncing Text
    -- ^ What, e.g. "3 of 5 repositories" or "github.com".
  | Offline
  | Failed
  | NoAccounts
    -- ^ Nothing to sync yet.
  deriving (Eq)

-- | The chip only says where syncing stands; the header's refresh button
-- is what syncs again, failed or not.
new :: State -> IO Gtk.Box
new state = do
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
      when (state `elem` [Offline, NoAccounts]) $ Gtk.widgetAddCssClass dot "quiet"
      #append chip dot
  GI.new Gtk.Label [#label := text state, #cssClasses := ["text", stateClass state]] >>= #append chip
  pure chip

tone :: State -> Tone
tone s = case s of
  Synced _ -> Success
  Syncing _ -> Accent
  Offline -> Neutral
  Failed -> Danger
  NoAccounts -> Neutral

text :: State -> Text
text s = case s of
  Synced when' -> "Synced " <> when'
  Syncing what -> "Syncing " <> what
  Offline -> "Offline · showing cached data"
  Failed -> "Sync failed"
  NoAccounts -> "No accounts yet"

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
  ]
