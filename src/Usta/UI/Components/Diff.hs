{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A unified diff of one file: a header with the path, counts and a
-- "Viewed" toggle, then numbered lines colored by kind. Any widget (a
-- review thread, say) can be attached below a line.
module Usta.UI.Components.Diff
  ( Line (..)
  , File (..)
  , new
  , plain
  , css
  ) where

import Control.Monad (forM, forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Highlight as Highlight

type Line :: Type
data Line
  = Hunk Text
    -- ^ The "@@ -12,6 +12,8 @@" header line.
  | Context Int Int Text
    -- ^ Unchanged: old number, new number, text.
  | Added Int Text
  | Removed Int Text

type File :: Type
data File = File
  { path :: Text
  , viewed :: Bool
  , diffLines :: [Line]
  }

-- | @attached@ pairs a line's index with a widget shown under it.
-- A viewed file shows only its header; @onViewed@ hears every toggle.
new :: File -> [(Int, Gtk.Widget)] -> (Bool -> IO ()) -> IO Gtk.Box
new file attached onViewed = build file attached (Just onViewed)

-- | A diff to read, without the "Viewed" toggle, e.g. for one commit.
plain :: File -> IO Gtk.Box
plain file = build file { viewed = False } [] Nothing

build :: File -> [(Int, Gtk.Widget)] -> Maybe (Bool -> IO ()) -> IO Gtk.Box
build file attached onViewed = do
  card <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #cssClasses := ["u-card", "u-diff"]
                         ]
  header <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 10
                           , #cssClasses := ["diff-header"]
                           ]
  label file.path ["mono", "strong"] >>= #append header
  label (stat file) ["muted", "small"] >>= #append header
  #append card header

  body <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #visible := not file.viewed
                         ]
  codes <- forM (zip [0 ..] file.diffLines) $ \(index, line) -> do
    (row, code) <- lineRow line
    #append body row
    forM_ [w | (i, w) <- attached, i == index] $ \w -> do
      holder <- GI.new Gtk.Box [#cssClasses := ["diff-attached"]]
      #append holder w
      #append body holder
    pure code
  Highlight.follow card $ \palette ->
    forM_ (zip codes (highlighted palette file)) $ \(code, marked) -> Gtk.labelSetMarkup code marked
  #append card body
  setCollapsed card file.viewed

  forM_ onViewed $ \report -> do
    check <- GI.new Gtk.CheckButton [ #label := "Viewed"
                                    , #active := file.viewed
                                    , #hexpand := True
                                    , #halign := Gtk.AlignEnd
                                    ]
    #append header check
    void $ GI.on check #toggled $ do
      isViewed <- GI.get check #active
      GI.set body [#visible := not isViewed]
      setCollapsed card isViewed
      report isViewed
  pure card
  where
    setCollapsed card collapsed
      | collapsed = Gtk.widgetAddCssClass card "collapsed"
      | otherwise = Gtk.widgetRemoveCssClass card "collapsed"

stat :: File -> Text
stat file = "+" <> count isAdded <> " −" <> count isRemoved
  where
    count p = T.pack (show (length (filter p file.diffLines)))
    isAdded (Added _ _) = True
    isAdded _ = False
    isRemoved (Removed _ _) = True
    isRemoved _ = False

-- | Each line's code as Pango markup, highlighted by the file's language.
-- The lines are tokenized together, so a comment or string that spans
-- lines keeps its color; hunk headers stay plain.
highlighted :: Highlight.Palette -> File -> [Text]
highlighted palette file
  | length marked == length codes = fill file.diffLines marked
  | otherwise = map (Highlight.plain . textOf) file.diffLines
  where
    codes = [t | line <- file.diffLines, Just t <- [codeOf line]]
    marked = Highlight.markup palette (Highlight.languageFor (T.unpack file.path)) (T.intercalate "\n" codes <> "\n")
    fill (line : rest) ms = case (codeOf line, ms) of
      (Just _, m : ms') -> m : fill rest ms'
      _ -> Highlight.plain (textOf line) : fill rest ms
    fill [] _ = []
    codeOf line = case line of
      Hunk _ -> Nothing
      Context _ _ t -> Just t
      Added _ t -> Just t
      Removed _ t -> Just t
    textOf line = case line of
      Hunk t -> t
      Context _ _ t -> t
      Added _ t -> t
      Removed _ t -> t

-- | The row, and the label its code goes in as markup.
lineRow :: Line -> IO (Gtk.Box, Gtk.Label)
lineRow line = do
  row <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #cssClasses := ["diff-line", kindClass]
                        ]
  gutter old >>= #append row
  gutter new' >>= #append row
  label sign ["sign"] >>= #append row
  text <- label "" ["code"]
  GI.set text [#hexpand := True, #selectable := True]
  #append row text
  pure (row, text)
  where
    (kindClass, old, new', sign) = case line of
      Hunk _ -> ("hunk", "", "", "")
      Context a b _ -> ("context", number a, number b, "")
      Added b _ -> ("added", "", number b, "+")
      Removed a _ -> ("removed", number a, "", "−")
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
  , "box.u-diff.collapsed box.diff-header { border-bottom: none; border-radius: 8px; }"
  ]
