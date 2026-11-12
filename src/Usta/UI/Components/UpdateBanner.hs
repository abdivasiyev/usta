{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | "usta 0.2.0 is out", under the header, with Not now and Open the
release; either button hides it until the next start.
-}
module Usta.UI.Components.UpdateBanner
    ( new
    , css
    )
where

import Control.Monad (void)
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)

import Data.GI.Base qualified as GI
import GI.Gtk qualified as Gtk

import Usta.UI.Components.Button qualified as Button

-- | @newer@ and @running@ are versions; @open@ shows the release page.
new :: Text -> Text -> IO () -> IO Gtk.Widget
new newer running open = do
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["u-update-banner"]]
    texts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #hexpand := True]
    GI.new Gtk.Label [#label := "usta " <> newer <> " is out", #xalign := 0, #cssClasses := ["title"]] >>= #append texts
    GI.new Gtk.Label [#label := "You have " <> running <> ". The release page has what changed and the download.", #xalign := 0, #wrap := True] >>= #append texts
    #append box texts
    later <- Button.new Button.defaults {Button.label = Just "Not now", Button.small = True}
    opening <- Button.new Button.defaults {Button.label = Just "Open the release", Button.small = True, Button.variant = Button.Primary}
    #append box later
    #append box opening
    void $ GI.on later #clicked (GI.set box [#visible := False])
    void $ GI.on opening #clicked (GI.set box [#visible := False] >> open)
    Gtk.toWidget box

css :: Text
css =
    mconcat
        [ "box.u-update-banner { margin: 10px 16px 0; padding: 10px 14px; border: 1px solid var(--accent); border-radius: 8px; background: var(--accent-bg); }"
        , "box.u-update-banner label.title { font-weight: 600; color: var(--accent); }"
        , "box.u-update-banner label { font-size: 13px; }"
        ]
