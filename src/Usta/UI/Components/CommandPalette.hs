{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The command palette: a search over everything in the local cache, and
-- the app's commands after a ">". It floats over the window, which dims.
module Usta.UI.Components.CommandPalette
  ( Kind (..)
  , Item (..)
  , Model (..)
  , new
  , css
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (find, findIndex)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Shortcut as Shortcut

data Kind = PullRequest | Issue | Repository | Command
  deriving (Eq, Enum, Bounded)

data Item = Item
  { kind :: Kind
  , title :: Text
  , detail :: Text
    -- ^ Shown on the right: "usta #2", a host, or a command's keys.
  , run :: IO ()
  }

data Model = Model
  { recent :: [Item]
    -- ^ Shown before anything is typed.
  , items :: [Item]
    -- ^ Everything searchable, commands included.
  }

-- | What was typed, parsed once: an optional kind prefix and the search term.
data Query = Query (Maybe Kind) Text

prefix :: Kind -> Text
prefix k = case k of
  PullRequest -> "pr:"
  Issue -> "is:"
  Repository -> "repo:"
  Command -> ">"

parseQuery :: Text -> Query
parseQuery raw =
  case find (\k -> prefix k `T.isPrefixOf` T.toLower text) [minBound ..] of
    Just k -> Query (Just k) (T.strip (T.drop (T.length (prefix k)) text))
    Nothing -> Query Nothing text
  where
    text = T.strip raw

sectionName :: Kind -> Text
sectionName k = case k of
  PullRequest -> "Pull requests"
  Issue -> "Issues"
  Repository -> "Repositories"
  Command -> "Commands"

-- | The sections to show for a query, with the chip's kind as a fallback
-- for a query without a prefix.
sections :: Maybe Kind -> Query -> Model -> [(Text, [Item])]
sections chosen (Query typed term) m = case (typed <|> chosen, T.null term) of
  (Nothing, True) -> nonEmpty [("Recent", recent m), ("Commands", ofKind Command (items m))]
  (only, _) -> nonEmpty
    [ (sectionName k, ofKind k (filter (matches term) (items m)))
    | k <- [minBound ..], maybe True (== k) only
    ]
  where
    ofKind k = filter ((== k) . kind)
    nonEmpty = filter (not . null . snd)

matches :: Text -> Item -> Bool
matches term i = T.toLower term `T.isInfixOf` T.toLower (title i <> " " <> detail i)

-- | Wraps @content@ in an overlay that can show the palette. Returns the
-- overlay to place instead of @content@, and an action that opens the
-- palette. @model@ runs on every open, so items can reflect current state.
new :: Gtk.Widget -> IO Model -> IO (Gtk.Overlay, IO ())
new content model = do
  overlay <- GI.new Gtk.Overlay [#child := content]

  backdrop <- GI.new Gtk.Box [#visible := False, #cssClasses := ["u-palette-backdrop"]]
  #addOverlay overlay backdrop

  palette <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                            , #visible := False
                            , #widthRequest := 640
                            , #halign := Gtk.AlignCenter
                            , #valign := Gtk.AlignStart
                            , #marginTop := 96
                            , #cssClasses := ["u-palette"]
                            ]
  #addOverlay overlay palette

  entry <- GI.new Gtk.SearchEntry [ #placeholderText := "Search everything, or type > for commands"
                                  , #hexpand := True
                                  ]
  esc <- Shortcut.key "Esc"
  GI.set esc [#valign := Gtk.AlignCenter]
  searchRow <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                              , #spacing := 8
                              , #cssClasses := ["query"]
                              ]
  #append searchRow entry
  #append searchRow esc
  #append palette searchRow

  chipBar <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 6
                            , #cssClasses := ["chips"]
                            ]
  allChip <- chip "All" Nothing
  kindChips <- forM [minBound ..] $ \k -> do
    c <- chip (sectionName' k) (Just (prefix k))
    Gtk.toggleButtonSetGroup c (Just allChip)
    pure (c, k)
  GI.set allChip [#active := True]
  #append chipBar allChip
  forM_ kindChips $ #append chipBar . fst
  #append palette chipBar

  list <- GI.new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeSingle
                             , #cssClasses := ["results"]
                             ]
  scroll <- GI.new Gtk.ScrolledWindow [ #child := list
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      , #propagateNaturalHeight := True
                                      , #maxContentHeight := 400
                                      ]
  #append palette scroll

  footer <- Shortcut.bar
    [("↑ ↓", "move"), ("Enter", "open"), ("Ctrl Enter", "open in browser")]
    (Just "Searching local cache")
  Gtk.widgetAddCssClass footer "footer"
  #append palette footer

  current <- newIORef (Model [] [])
  shown <- newIORef []
  let chipKind = do
        active <- forM kindChips $ \(c, k) -> do
          on <- GI.get c #active
          pure [k | on]
        pure (case concat active of
          k : _ -> Just k
          [] -> Nothing)

      refresh = do
        m <- readIORef current
        text <- Gtk.editableGetText entry
        selected <- chipKind
        Gtk.listBoxRemoveAll list
        rows <- fmap concat $ forM (sections selected (parseQuery text) m) $ \(name, xs) -> do
          header <- GI.new Gtk.ListBoxRow [ #selectable := False
                                          , #activatable := False
                                          , #cssClasses := ["header"]
                                          ]
          GI.new Gtk.Label [#label := T.toUpper name, #xalign := 0, #cssClasses := ["section"]]
            >>= \l -> GI.set header [#child := l]
          #append list header
          forM xs $ \item -> do
            row <- GI.new Gtk.ListBoxRow []
            itemRow item >>= \r -> GI.set row [#child := r]
            #append list row
            pure (row, item)
        writeIORef shown rows
        forM_ (take 1 rows) $ Gtk.listBoxSelectRow list . Just . fst

      selectedIndex = do
        rows <- readIORef shown
        row <- Gtk.listBoxGetSelectedRow list
        pure (row >>= \r -> findIndex ((== r) . fst) rows, rows)

      move step = do
        (index, rows) <- selectedIndex
        let next = maybe 0 (\i -> max 0 (min (length rows - 1) (i + step))) index
        forM_ (take 1 (drop next rows)) $ Gtk.listBoxSelectRow list . Just . fst

      close = do
        GI.set backdrop [#visible := False]
        GI.set palette [#visible := False]
        root <- Gtk.widgetGetRoot overlay
        window <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
        forM_ window $ \w -> Gtk.windowSetFocus w (Nothing :: Maybe Gtk.Widget)

      runRow row = do
        rows <- readIORef shown
        forM_ (lookup row rows) $ \item -> close >> run item

      open = do
        model >>= writeIORef current
        GI.set allChip [#active := True]
        Gtk.editableSetText entry ""
        refresh
        GI.set backdrop [#visible := True]
        GI.set palette [#visible := True]
        void (Gtk.widgetGrabFocus entry)

  void $ GI.on entry #searchChanged refresh
  void $ GI.on entry #stopSearch close
  void $ GI.on entry #activate $ Gtk.listBoxGetSelectedRow list >>= mapM_ runRow
  void $ GI.on list #rowActivated runRow
  forM_ (allChip : map fst kindChips) $ \c ->
    void $ GI.on c #toggled $ do
      on <- GI.get c #active
      when on refresh

  -- Arrow keys move through the results while typing stays in the entry.
  keys <- GI.new Gtk.ShortcutController [#propagationPhase := Gtk.PropagationPhaseCapture]
  forM_ [("Down", 1), ("Up", -1)] $ \(name, step) ->
    shortcut keys name (move step)
  Gtk.widgetAddController entry keys

  -- Ctrl K (Cmd K on macOS) opens it from anywhere in the window.
  global <- GI.new Gtk.ShortcutController [#scope := Gtk.ShortcutScopeGlobal]
  shortcut global "<Primary>k" open
  Gtk.widgetAddController overlay global

  click <- GI.new Gtk.GestureClick []
  void $ GI.on click #pressed $ \_ _ _ -> close
  Gtk.widgetAddController backdrop click

  pure (overlay, open)
  where
    sectionName' k = case k of
      Repository -> "Repos"
      _ -> sectionName k

-- | Runs @action@ for the key, e.g. "Down" or "<Primary>k".
shortcut :: Gtk.ShortcutController -> Text -> IO () -> IO ()
shortcut controller trigger action = do
  parsed <- Gtk.shortcutTriggerParseString trigger
  callback <- Gtk.callbackActionNew (\_ _ -> action >> pure True)
  s <- Gtk.shortcutNew parsed (Just callback)
  Gtk.shortcutControllerAddShortcut controller s

-- | A filter chip: a name and, in code font, the prefix it stands for.
chip :: Text -> Maybe Text -> IO Gtk.ToggleButton
chip name typed = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 4]
  GI.new Gtk.Label [#label := name] >>= #append box
  forM_ typed $ \t -> GI.new Gtk.Label [#label := t, #cssClasses := ["mono", "prefix"]] >>= #append box
  GI.new Gtk.ToggleButton [#child := box, #cssClasses := ["chip"]]

itemRow :: Item -> IO Gtk.Box
itemRow item = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  let (glyph, glyphClass) = case kind item of
        PullRequest -> ("PR", "pr")
        Issue -> ("#", "issue")
        Repository -> ("R", "repo")
        Command -> (">", "command")
  GI.new Gtk.Label [ #label := glyph
                   , #widthRequest := 28
                   , #cssClasses := ["mono", "glyph", glyphClass]
                   ] >>= #append row
  GI.new Gtk.Label [#label := title item, #xalign := 0, #hexpand := True, #cssClasses := ["title"]]
    >>= #append row
  GI.new Gtk.Label [#label := detail item, #cssClasses := ["mono", "detail"]] >>= #append row
  pure row

css :: Text
css = mconcat
  [ "box.u-palette-backdrop { background: rgba(27, 29, 33, 0.45); }"
  , "box.u-palette {"
  , "  background: var(--surface); color: var(--text); border: 1px solid var(--border);"
  , "  border-radius: 12px; box-shadow: 0 16px 48px rgba(0, 0, 0, 0.25);"
  , "}"
  , "box.u-palette box.query { padding: 10px 16px; border-bottom: 1px solid var(--border); }"
  , "box.u-palette box.query entry, box.u-palette box.query entry:focus-within {"
  , "  min-height: 36px; font-size: 18px; background: transparent; border: none; box-shadow: none;"
  , "  outline: none;"
  , "}"
  , "box.u-palette box.chips { padding: 10px 16px; border-bottom: 1px solid var(--border); }"
  , "box.u-palette button.chip {"
  , "  min-height: 22px; padding: 1px 10px; border-radius: 999px; font-size: 13px;"
  , "  background: var(--neutral-bg); color: var(--text); border: none; box-shadow: none; background-image: none;"
  , "}"
  , "box.u-palette button.chip:checked { background: var(--text); color: var(--surface); }"
  , "box.u-palette button.chip label.prefix { font-weight: 700; font-size: 12px; }"
  , "box.u-palette list.results { background: transparent; padding: 6px; }"
  , "box.u-palette list.results row { padding: 10px 12px; border-radius: 8px; background: transparent; color: var(--text); }"
  , "box.u-palette list.results row:selected { background: var(--selected); }"
  , "box.u-palette list.results row.header { padding: 10px 10px 4px; }"
  , "box.u-palette label.section { font-size: 12px; font-weight: 600; color: var(--text-muted); }"
  , "box.u-palette label.title { font-size: 15px; }"
  , "box.u-palette label.glyph { color: var(--text-muted); }"
  , "box.u-palette label.glyph.pr { color: var(--success); }"
  , "box.u-palette label.detail { color: var(--text-muted); }"
  , "box.u-palette box.footer {"
  , "  padding: 10px 16px; border-top: 1px solid var(--border); border-radius: 0 0 12px 12px;"
  , "  background: var(--window);"
  , "}"
  ]
