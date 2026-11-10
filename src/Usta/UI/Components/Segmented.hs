{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A row of mutually exclusive filters: "All · Unread · Participating".
module Usta.UI.Components.Segmented
    ( new
    , css
    )
where

import Control.Monad (forM, forM_, when)
import Data.GI.Base (AttrOp ((:=)), set)
import Data.Text (Text)

import Data.GI.Base qualified as GI
import GI.Gtk qualified as Gtk

{- | The options and the index of the one selected at start. Returns the
container and the buttons, so callers can react to @#toggled@.
-}
new :: [Text] -> Int -> IO (Gtk.Box, [Gtk.ToggleButton])
new options selected = do
    box <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationHorizontal
            , #spacing := 4
            , #cssClasses := ["u-segmented"]
            ]
    buttons <- forM options $ \text -> GI.new Gtk.ToggleButton [#label := text]
    case buttons of
        first : rest -> forM_ rest $ \b -> set b [#group := first]
        [] -> pure ()
    forM_ (zip [0 ..] buttons) $ \(i, b) -> do
        when (i == selected) $ set b [#active := True]
        #append box b
    pure (box, buttons)

css :: Text
css =
    mconcat
        [ "box.u-segmented button {"
        , "  min-height: 32px; padding: 0 12px; border-radius: 6px; border: none;"
        , "  background: transparent; color: var(--text-soft); box-shadow: none;"
        , "  font-size: 13px; background-image: none;"
        , "}"
        , "box.u-segmented button:hover { background: var(--border-subtle); }"
        , "box.u-segmented button:checked { background: var(--text); color: var(--window); font-weight: 500; }"
        ]
