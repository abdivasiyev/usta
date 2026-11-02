{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A unified diff of one file: a header with the path, counts and a
-- "Viewed" toggle, then numbered lines colored by kind. Any widget (a
-- review thread, say) can be attached below a line.
module Usta.UI.Components.Diff
  ( Line (..)
  , File (..)
  , new
  , css
  ) where

import Control.Monad (forM_)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

data Line
  = Hunk Text
    -- ^ The "@@ -12,6 +12,8 @@" header line.
  | Context Int Int Text
    -- ^ Unchanged: old number, new number, text.
  | Added Int Text
  | Removed Int Text

data File = File
  { path :: Text
  , viewed :: Bool
  , diffLines :: [Line]
  }

-- | @attached@ pairs a line's index with a widget shown under it.
new :: File -> [(Int, Gtk.Widget)] -> IO Gtk.Box
new file attached = do
  card <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #cssClasses := ["u-card", "u-diff"]
                         ]
  header <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 10
                           , #cssClasses := ["diff-header"]
                           ]
  label (path file) ["mono", "strong"] >>= #append header
  label (stat file) ["muted", "small"] >>= #append header
  check <- GI.new Gtk.CheckButton [ #label := "Viewed"
                                  , #active := viewed file
                                  , #hexpand := True
                                  , #halign := Gtk.AlignEnd
                                  ]
  #append header check
  #append card header

  forM_ (zip [0 ..] (diffLines file)) $ \(index, line) -> do
    lineRow line >>= #append card
    forM_ [w | (i, w) <- attached, i == index] $ \w -> do
      holder <- GI.new Gtk.Box [#cssClasses := ["diff-attached"]]
      #append holder w
      #append card holder
  pure card

stat :: File -> Text
stat file = "+" <> count isAdded <> " −" <> count isRemoved
  where
    count p = T.pack (show (length (filter p (diffLines file))))
    isAdded (Added _ _) = True
    isAdded _ = False
    isRemoved (Removed _ _) = True
    isRemoved _ = False

lineRow :: Line -> IO Gtk.Box
lineRow line = do
  row <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #cssClasses := ["diff-line", kindClass]
                        ]
  gutter old >>= #append row
  gutter new' >>= #append row
  label sign ["sign"] >>= #append row
  text <- label body ["code"]
  GI.set text [#hexpand := True, #selectable := True]
  #append row text
  pure row
  where
    (kindClass, old, new', sign, body) = case line of
      Hunk t -> ("hunk", "", "", "", t)
      Context a b t -> ("context", number a, number b, "", t)
      Added b t -> ("added", "", number b, "+", t)
      Removed a t -> ("removed", number a, "", "−", t)
    number = T.pack . show

gutter :: Text -> IO Gtk.Label
gutter n = do
  l <- GI.new Gtk.Label [ #label := n
                        , #xalign := 1
                        , #widthRequest := (44 :: Int32)
                        , #cssClasses := ["gutter"]
                        ]
  pure l

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-diff { border-radius: 8px; }"
  , "box.u-diff box.diff-header {"
  , "  padding: 8px 12px; background: var(--window); border-bottom: 1px solid var(--border);"
  , "  border-radius: 8px 8px 0 0;"
  , "}"
  , "box.u-diff label.strong { font-weight: 500; }"
  , "box.u-diff label.small { font-size: 12px; }"
  , "box.u-diff box.diff-line { font-family: 'JetBrains Mono', monospace; font-size: 13px; }"
  , "box.u-diff label.gutter { padding: 2px 8px 2px 0; color: var(--text-muted); }"
  , "box.u-diff label.sign { min-width: 20px; padding: 2px 0; }"
  , "box.u-diff label.code { padding: 2px 16px 2px 0; }"
  , "box.u-diff box.diff-line.hunk { background: var(--selected); color: var(--text-soft); }"
  , "box.u-diff box.diff-line.hunk label.gutter { background: var(--nav-active); }"
  , "box.u-diff box.diff-line.context { background: var(--surface); }"
  , "box.u-diff box.diff-line.context label.gutter { background: var(--window); }"
  , "box.u-diff box.diff-line.added { background: var(--success-bg); color: var(--success); }"
  , "box.u-diff box.diff-line.removed { background: var(--danger-bg); color: var(--danger); }"
  , "box.u-diff box.diff-attached { padding: 8px 12px 8px 108px; background: var(--surface); }"
  , "box.u-diff > box:last-child { border-radius: 0 0 8px 8px; }"
  ]
