{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A file's text, highlighted by its language, with line numbers.
module Usta.UI.Components.CodeView
    ( new
    , withView
    , css
    )
where

import Control.Monad (forM_)
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.Gtk qualified as Gtk

import Usta.UI.Highlight qualified as Highlight
import Usta.UI.Scroll qualified as Scroll

-- | @path@ picks the language; @text@ is the whole file.
new :: Text -> Text -> IO Gtk.Widget
new path text = fst <$> withView path text

-- | Like 'new', with the text view, to search it.
withView :: Text -> Text -> IO (Gtk.Widget, Gtk.TextView)
withView path text = do
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["u-code"]]
    let
        count = max 1 (length (T.lines text))
    numbers <-
        GI.new
            Gtk.Label
            [ #label := T.intercalate "\n" (map (T.pack . show) [1 .. count])
            , #xalign := 1
            , #yalign := 0
            , #cssClasses := ["numbers"]
            ]
    #append box numbers
    view <-
        GI.new
            Gtk.TextView
            [ #editable := False
            , #cursorVisible := False
            , #monospace := True
            , #hexpand := True
            , #wrapMode := Gtk.WrapModeNone
            , #cssClasses := ["code"]
            ]
    #append box view
    buffer <- Gtk.textViewGetBuffer view
    let
        language = Highlight.languageFor (T.unpack path)
    -- Painted again in the other palette when the theme changes.
    Highlight.follow view $ \palette -> do
        Gtk.textBufferSetText buffer "" (-1)
        let
            marked = Highlight.markup palette language text
        forM_ (zip [1 :: Int ..] marked) $ \(i, line) -> do
            end <- Gtk.textBufferGetEndIter buffer
            Gtk.textBufferInsertMarkup buffer end (if i < length marked then line <> "\n" else line) (-1)
    scroll <-
        GI.new
            Gtk.ScrolledWindow
            [ #child := box
            , #vscrollbarPolicy := Gtk.PolicyTypeNever
            , #propagateNaturalHeight := True
            ]
    Scroll.sideways scroll
    w <- Gtk.toWidget scroll
    pure (w, view)

css :: Text
css =
    mconcat
        [ "box.u-code { background: var(--surface); padding: 8px 0; }"
        , "box.u-code label.numbers {"
        , "  font-family: 'JetBrains Mono', monospace; font-size: 13px; color: var(--text-muted);"
        , "  padding: 0 12px 0 8px; min-width: 36px;"
        , "}"
        , "box.u-code textview.code, box.u-code textview.code text {"
        , "  background: var(--surface); color: var(--text);"
        , "  font-family: 'JetBrains Mono', monospace; font-size: 13px;"
        , "}"
        ]
