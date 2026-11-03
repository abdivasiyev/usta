{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The header search field with its keyboard hint on the right.
module Usta.UI.Components.SearchField
  ( Config (..)
  , defaults
  , new
  , css
  ) where

import Control.Monad (forM_)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)), set)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Shortcut as Shortcut

type Config :: Type
data Config = Config
  { placeholder :: Text
  , shortcut :: Maybe Text
  , width :: Int32
  }

defaults :: Config
defaults = Config
  { placeholder = "Search pull requests, issues, repositories"
  , shortcut = Just "Ctrl K"
  , width = 560
  }

-- | Returns the container to place and the entry to read from.
new :: Config -> IO (Gtk.Overlay, Gtk.SearchEntry)
new cfg = do
  entry <- GI.new Gtk.SearchEntry [ #placeholderText := cfg.placeholder
                                  , #widthRequest := cfg.width
                                  , #cssClasses := ["u-search"]
                                  ]
  overlay <- GI.new Gtk.Overlay [#child := entry]
  forM_ cfg.shortcut $ \keys -> do
    hint <- Shortcut.key keys
    set hint [ #halign := Gtk.AlignEnd
             , #valign := Gtk.AlignCenter
             , #marginEnd := 8
             , #canTarget := False
             ]
    #addOverlay overlay hint
  pure (overlay, entry)

css :: Text
css = mconcat
  -- GTK names a search entry's node "entry", with a "search" class.
  [ "entry.u-search {"
  , "  min-height: 34px; padding-right: 60px; background: var(--input-bg); color: var(--text);"
  , "  border: 1px solid var(--input-border); border-radius: 8px; box-shadow: none;"
  , "}"
  ]
