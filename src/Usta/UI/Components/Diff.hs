{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A unified diff of one file: a header with the path, counts and a
-- "Viewed" toggle, then numbered lines colored by kind. Any widget (a
-- review thread, say) can be attached below a line, and a line of the
-- new version can be commented on.
module Usta.UI.Components.Diff
  ( Line (..)
  , File (..)
  , Commenting (..)
  , new
  , plain
  , css
  ) where

import Control.Monad (forM, forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.IORef (IORef, writeIORef)
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

-- | How a diff lets the user start a note on a line of the new version:
-- a "+" shows by the line under the pointer.
type Commenting :: Type
data Commenting = Commenting
  { start :: Int -> Gtk.Box -> IO ()
    -- ^ The line's number in the new version and the box under it,
    -- which the note is written in.
  , hovered :: IORef (Maybe (IO ()))
    -- ^ While the pointer is on such a line, what starting there does.
  }

-- | @attached@ pairs a line's index with a widget shown under it.
-- A viewed file shows only its header; @onViewed@ hears every toggle.
new :: File -> [(Int, Gtk.Widget)] -> Commenting -> (Bool -> IO ()) -> IO Gtk.Box
new file attached commenting onViewed = build file attached (Just commenting) (Just onViewed)

-- | A diff to read, without the "Viewed" toggle, e.g. for one commit.
plain :: File -> IO Gtk.Box
plain file = build file { viewed = False } [] Nothing Nothing

build :: File -> [(Int, Gtk.Widget)] -> Maybe Commenting -> Maybe (Bool -> IO ()) -> IO Gtk.Box
build file attached commenting onViewed = do
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
    slot <- case (commenting, newSide line) of
      (Just c, Just number) -> do
        overlay <- GI.new Gtk.Overlay [#child := row]
        plus <- GI.new Gtk.Button [ #label := "+", #halign := Gtk.AlignStart, #valign := Gtk.AlignCenter
                                  , #visible := False, #tooltipText := "Comment on this line (c)"
                                  , #cssClasses := ["comment-plus"] ]
        Gtk.overlayAddOverlay overlay plus
        under <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
        let begin = c.start number under
        void $ GI.on plus #clicked begin
        motion <- Gtk.eventControllerMotionNew
        void $ GI.on motion #enter $ \_ _ -> GI.set plus [#visible := True] >> writeIORef c.hovered (Just begin)
        void $ GI.on motion #leave $ GI.set plus [#visible := False] >> writeIORef c.hovered Nothing
        Gtk.widgetAddController overlay motion
        #append body overlay
        pure (Just under)
      _ -> Nothing <$ #append body row
    forM_ [w | (i, w) <- attached, i == index] $ \w -> do
      holder <- GI.new Gtk.Box [#cssClasses := ["diff-attached"]]
      #append holder w
      #append body holder
    mapM_ (#append body) slot
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

-- | A line's number in the new version, for a line that is there.
newSide :: Line -> Maybe Int
newSide line = case line of
  Context _ b _ -> Just b
  Added b _ -> Just b
  _ -> Nothing

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
  -- The holder already indents what it holds.
  , "box.u-diff box.diff-attached > box.u-thread { margin: 0; }"
  , "box.u-diff button.comment-plus { min-width: 18px; min-height: 18px; margin-left: 14px; padding: 0; border-radius: 4px; background: var(--accent); color: var(--on-accent); font-weight: 700; font-size: 12px; }"
  , "box.u-diff > box:last-child { border-radius: 0 0 8px 8px; }"
  , "box.u-diff.collapsed box.diff-header { border-bottom: none; border-radius: 8px; }"
  ]
