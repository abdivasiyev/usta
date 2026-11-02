{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The first screen, before any forge account is connected.
module Usta.UI.Pages.Welcome
  ( new
  , css
  ) where

import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Platform as Platform
import Usta.UI.Style (Tone (..))

-- | @onConnect@ runs with the forge whose card was clicked.
new :: (Forge.Kind -> IO ()) -> IO Gtk.Widget
new onConnect = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #hexpand := True
                         , #vexpand := True
                         , #cssClasses := ["u-welcome"]
                         ]

  -- No app header here, but the window still needs its controls.
  header <- GI.new Gtk.HeaderBar [#cssClasses := ["bare"]]
  Platform.configureHeaderBar Platform.current header
  GI.new Gtk.Box [] >>= \noTitle -> GI.set header [#titleWidget := noTitle]
  #append page header

  column <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 12
                           , #widthRequest := 560
                           , #halign := Gtk.AlignCenter
                           , #valign := Gtk.AlignCenter
                           , #vexpand := True
                           , #cssClasses := ["column"]
                           ]
  logo <- Icon.new 80 Icon.Logo
  GI.set logo [#halign := Gtk.AlignCenter, #marginBottom := 16]
  #append column logo
  GI.new Gtk.Label [#label := "Welcome to usta", #cssClasses := ["title"]] >>= #append column
  GI.new Gtk.Label [ #label := "Your pull requests, issues and notifications from every forge, in one fast native window."
                   , #wrap := True
                   , #justify := Gtk.JustificationCenter
                   , #maxWidthChars := 52
                   , #cssClasses := ["lead"]
                   ] >>= #append column

  cards <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                          , #spacing := 10
                          , #marginTop := 20
                          ]
  forM_ [minBound .. maxBound] $ \kind -> do
    card <- connectCard kind
    void $ GI.on card #clicked (onConnect kind)
    #append cards card
  #append column cards

  promises <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                             , #spacing := 24
                             , #halign := Gtk.AlignCenter
                             , #marginTop := 20
                             ]
  forM_ ["Data cached on this machine", "Tokens in your system keyring", "Works offline"] $ \text -> do
    item <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    Icon.toned 14 Success Icon.Check >>= #append item
    GI.new Gtk.Label [#label := text, #cssClasses := ["promise"]] >>= #append item
    #append promises item
  #append column promises

  #append page column
  Gtk.toWidget page

connectCard :: Forge.Kind -> IO Gtk.Button
connectCard kind = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 14]
  Forge.tile kind >>= #append row
  texts <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                          , #spacing := 2
                          , #hexpand := True
                          , #valign := Gtk.AlignCenter
                          ]
  GI.new Gtk.Label [#label := "Connect " <> Forge.name kind, #xalign := 0, #cssClasses := ["name"]]
    >>= #append texts
  GI.new Gtk.Label [#label := reach kind, #xalign := 0, #cssClasses := ["reach"]] >>= #append texts
  #append row texts
  GI.new Gtk.Image [#iconName := "go-next-symbolic", #cssClasses := ["chevron"]] >>= #append row
  GI.new Gtk.Button [#child := row, #cssClasses := ["connect"]]

reach :: Forge.Kind -> Text
reach kind = case kind of
  Forge.GitHub -> "github.com or GitHub Enterprise"
  Forge.GitLab -> "gitlab.com or a self-hosted instance"
  Forge.Forgejo -> "Forgejo or Gitea, self-hosted"

css :: Text
css = mconcat
  [ "box.u-welcome { background: var(--window); color: var(--text); }"
  , "box.u-welcome headerbar.bare { background: transparent; border: none; box-shadow: none; }"
  , "box.u-welcome box.column { margin-bottom: 52px; }"
  , "box.u-welcome label.title { font-size: 32px; font-weight: 700; }"
  , "box.u-welcome label.lead { font-size: 17px; line-height: 1.45; color: var(--text-soft); }"
  , "box.u-welcome button.connect {"
  , "  padding: 14px 18px; border: 1px solid var(--input-border); border-radius: 10px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
  , "}"
  , "box.u-welcome button.connect:hover { border-color: var(--accent); }"
  , "box.u-welcome label.name { font-weight: 600; font-size: 15px; }"
  , "box.u-welcome label.reach { font-size: 14px; color: var(--text-muted); }"
  , "box.u-welcome image.chevron { color: var(--text-muted); }"
  , "box.u-welcome label.promise { font-size: 14px; color: var(--text-soft); }"
  ]
