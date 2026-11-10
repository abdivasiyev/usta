{-# LANGUAGE OverloadedStrings #-}

{- | A dropdown of names under an "All …" first line, as a list's filter:
the names follow what is listed, and the choice stays while it is there.
-}
module Usta.UI.Components.Choices
    ( new
    , offer
    , chosen
    )
where

import Data.List (elemIndex, nub, sort)
import Data.Text (Text)
import Data.Word (Word32)

import Data.GI.Base qualified as GI
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

-- | @new "All repositories"@: only that line until 'offer' gives names.
new :: Text -> IO Gtk.DropDown
new everything = Gtk.dropDownNewFromStrings [everything]

-- | The names to choose from, sorted, after the first line.
offer :: Gtk.DropDown -> [Text] -> IO ()
offer d names = do
    before <- chosen d
    first <- lineAt d 0
    let
        lines' = first : sort (nub names)
    model <- Gtk.stringListNew (Just lines')
    Gtk.dropDownSetModel d (Just model)
    Gtk.dropDownSetSelected d (maybe 0 fromIntegral (before >>= (`elemIndex` lines')))

-- | The name chosen; 'Nothing' for the first line.
chosen :: Gtk.DropDown -> IO (Maybe Text)
chosen d = do
    index <- Gtk.dropDownGetSelected d
    -- GTK says maxBound for no line at all.
    if index == 0 || index == maxBound then pure Nothing else Just <$> lineAt d index

lineAt :: Gtk.DropDown -> Word32 -> IO Text
lineAt d index =
    Gtk.dropDownGetModel d >>= \case
        Nothing -> pure ""
        Just m -> Gio.listModelGetItem m index >>= maybe (pure Nothing) (GI.castTo Gtk.StringObject) >>= maybe (pure "") Gtk.stringObjectGetString
