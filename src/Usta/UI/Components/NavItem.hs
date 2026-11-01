{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A sidebar entry: "Inbox  7", "● github.com", "abdivasiyev/usta".
module Usta.UI.Components.NavItem
  ( Config (..)
  , defaults
  , new
  , css
  ) where

import Control.Monad (forM_, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)), set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Badge as Badge
import Usta.UI.Style (Tone)

data Config = Config
  { label :: Text
  , count :: Maybe Int
  , dot :: Maybe Tone
    -- ^ A leading colored dot, e.g. for forges.
  , active :: Bool
  , mono :: Bool
    -- ^ Monospace, for repository names.
  , link :: Bool
    -- ^ Accent colored, for actions such as "Add forge".
  }

defaults :: Config
defaults = Config
  { label = ""
  , count = Nothing
  , dot = Nothing
  , active = False
  , mono = False
  , link = False
  }

new :: Config -> IO Gtk.Button
new cfg = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  forM_ (dot cfg) $ \tone -> Badge.dot tone >>= #append row
  text <- GI.new Gtk.Label [#label := label cfg, #xalign := 0, #hexpand := True]
  when (mono cfg) $ Gtk.widgetAddCssClass text "mono"
  #append row text
  forM_ (count cfg) $ \n -> do
    countLabel <- GI.new Gtk.Label [#label := T.pack (show n), #cssClasses := ["count"]]
    #append row countLabel
  button <- GI.new Gtk.Button [#child := row, #cssClasses := ["u-nav-item"]]
  when (active cfg) $ Gtk.widgetAddCssClass button "active"
  when (link cfg) $ Gtk.widgetAddCssClass button "link"
  set button [#hasFrame := False]
  pure button

css :: Text
css = mconcat
  [ "button.u-nav-item {"
  , "  min-height: 36px; padding: 0 10px; border-radius: 6px; border: none;"
  , "  background: transparent; color: var(--text); box-shadow: none; background-image: none;"
  , "  font-weight: 400;"
  , "}"
  , "button.u-nav-item:hover { background: var(--border-subtle); }"
  , "button.u-nav-item.active { background: var(--nav-active); font-weight: 600; }"
  , "button.u-nav-item.link { color: var(--accent); }"
  , "button.u-nav-item label.count { font-size: 12px; color: var(--text-muted); }"
  , "button.u-nav-item.active label.count { color: var(--accent-soft); }"
  ]
