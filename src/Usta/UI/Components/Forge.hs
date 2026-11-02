{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Forge identity: each forge keeps its own color, independent of status
-- tones, wherever it is shown (sidebar, list rows, headers).
module Usta.UI.Components.Forge
  ( Kind (..)
  , dot
  , smallDot
  , css
  ) where

import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified GI.Gtk as Gtk

data Kind = GitHub | GitLab | Forgejo
  deriving (Eq, Show, Enum, Bounded)

-- | The 8px dot used in the sidebar.
dot :: Kind -> IO Gtk.Widget
dot kind = make kind []

-- | The 6px dot used inside list rows.
smallDot :: Kind -> IO Gtk.Widget
smallDot kind = make kind ["small"]

make :: Kind -> [Text] -> IO Gtk.Widget
make kind extra = do
  box <- GI.new Gtk.Box [ #cssClasses := ["u-forge-dot", kindClass kind] <> extra
                        , #valign := Gtk.AlignCenter
                        ]
  Gtk.toWidget box

kindClass :: Kind -> Text
kindClass kind = case kind of
  GitHub -> "github"
  GitLab -> "gitlab"
  Forgejo -> "forgejo"

css :: Text
css = mconcat
  [ "box.u-forge-dot { min-width: 8px; min-height: 8px; border-radius: 999px; }"
  , "box.u-forge-dot.small { min-width: 6px; min-height: 6px; }"
  , "box.u-forge-dot.github { background: var(--text); }"
  , "box.u-forge-dot.gitlab { background: #6D28D9; }"
  , "box.u-forge-dot.forgejo { background: #C2410C; }"
  , "@media (prefers-color-scheme: dark) {"
  , "  box.u-forge-dot.gitlab { background: #A78BFA; }"
  , "  box.u-forge-dot.forgejo { background: #F08A4B; }"
  , "}"
  ]
