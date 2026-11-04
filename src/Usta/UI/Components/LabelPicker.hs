{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The labels picker of an issue or pull request, as the design draws it:
-- search, the repository's labels with colour and description, and
-- "Create label" for a name it lacks, which opens a form with a name, a
-- description and a colour. Changes save when the picker closes.
module Usta.UI.Components.LabelPicker
  ( Picker (..)
  , attach
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import Data.Time.Clock.POSIX (getPOSIXTime)
import Numeric (readHex)

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Icon as Icon
import Usta.Types.Label (Color, Definition (..))
import Usta.Types.Parse (parse, unparse)

type Picker :: Type
data Picker = Picker
  { repo :: Text
    -- ^ Shown on the new label form, e.g. "abdivasiyev/usta".
  , current :: [Text]
    -- ^ The labels it has, by name.
  , load :: IO (Either Text [Definition])
  , save :: [Text] -> IO (Either Text ())
    -- ^ Runs when the picker closes, only if the labels changed.
  , create :: Definition -> IO (Either Text ())
  , manage :: IO ()
    -- ^ Shows all the repository's labels.
  , saved :: IO ()
    -- ^ After a save, e.g. to reload the page.
  }

-- | The picker under @anchor@; gives what opens it.
attach :: Picker -> Gtk.Button -> IO (IO ())
attach p anchor = do
  stack <- GI.new Gtk.Stack [#vhomogeneous := False, #cssClasses := ["u-label-picker"]]
  popover <- GI.new Gtk.Popover [#child := stack, #position := Gtk.PositionTypeBottom, #hasArrow := False, #cssClasses := ["u-label-picker-pop"]]
  Gtk.widgetSetParent popover anchor
  void $ GI.on anchor #destroy (Gtk.widgetUnparent popover)

  chosen <- newIORef p.current
  known <- newIORef []
  rows <- newIORef []

  -- Picking.
  pick <- box 10
  GI.set pick [#widthRequest := 320, #cssClasses := ["pick"]]
  label "Labels" ["picker-title"] >>= #append pick
  search <- GI.new Gtk.SearchEntry [#placeholderText := "Filter labels"]
  #append pick search
  list <- box 2
  scroll <- GI.new Gtk.ScrolledWindow [#child := list, #maxContentHeight := 300, #propagateNaturalHeight := True, #hscrollbarPolicy := Gtk.PolicyTypeNever]
  #append pick scroll
  createText <- GI.new Gtk.Label [#xalign := 0]
  createRow <- GI.new Gtk.Button [#child := createText, #visible := False, #cssClasses := ["create"]]
  #append pick createRow
  problem <- label "" ["problem"]
  GI.set problem [#visible := False, #wrap := True]
  #append pick problem
  footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["footer"]]
  manage <- GI.new Gtk.Button [#label := "Manage labels", #cssClasses := ["link-button"]]
  #append footer manage
  note <- label "Changes save when it closes" ["muted", "small"]
  GI.set note [#hexpand := True, #xalign := 1]
  #append footer note
  #append pick footer
  void $ Gtk.stackAddNamed stack pick (Just "pick")

  -- Creating.
  form <- box 10
  GI.set form [#widthRequest := 360, #cssClasses := ["new"]]
  heading <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  t <- label "New label" ["picker-title"]
  GI.set t [#hexpand := True]
  #append heading t
  label p.repo ["muted", "small"] >>= #append heading
  #append form heading
  label "Name" ["field"] >>= #append form
  nameEntry <- GI.new Gtk.Entry []
  #append form nameEntry
  label "Description (optional)" ["field"] >>= #append form
  aboutEntry <- GI.new Gtk.Entry []
  #append form aboutEntry
  label "Colour" ["field"] >>= #append form
  swatchRow <- GI.new Gtk.FlowBox [#selectionMode := Gtk.SelectionModeNone, #maxChildrenPerLine := 12, #minChildrenPerLine := 6, #columnSpacing := 6, #rowSpacing := 6]
  #append form swatchRow
  hexRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  hexEntry <- GI.new Gtk.Entry [#maxLength := 7, #widthChars := 9, #cssClasses := ["mono"]]
  random <- Button.new Button.defaults {Button.label = Just "Random"}
  #append hexRow hexEntry >> #append hexRow random
  #append form hexRow
  previewRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  label "Preview" ["muted"] >>= #append previewRow
  preview <- GI.new Gtk.Label [#useMarkup := True]
  #append previewRow preview
  #append form previewRow
  formProblem <- label "" ["problem"]
  GI.set formProblem [#visible := False, #wrap := True]
  #append form formProblem
  formButtons <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd]
  cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
  createButton <- Button.new Button.defaults {Button.label = Just "Create label", Button.variant = Button.Primary}
  #append formButtons cancel >> #append formButtons createButton
  #append form formButtons
  void $ Gtk.stackAddNamed stack form (Just "new")

  let colorOf = do
        typed <- GI.get hexEntry #text
        pure (either (const Nothing) Just (parse typed) :: Maybe Color)
      redrawPreview = do
        name <- T.strip <$> GI.get nameEntry #text
        c <- colorOf
        GI.set preview [#label := maybe "" (\k -> chip k (if T.null name then "label" else name)) c]
        GI.set createButton [#sensitive := not (T.null name) && c /= Nothing]
  swatchButtons <- newIORef []
  let setColor hex = do
        GI.set hexEntry [#text := "#" <> hex]
        readIORef swatchButtons >>= mapM_ (\(h, b) -> (if h == hex then Gtk.widgetAddCssClass else Gtk.widgetRemoveCssClass) b "on")
        redrawPreview
  forM_ swatches $ \hex -> do
    b <- GI.new Gtk.Button [#cssClasses := ["swatch"], #tooltipText := "#" <> hex]
    dot <- GI.new Gtk.Label [#useMarkup := True, #label := "<span foreground='#" <> hex <> "' size='x-large'>●</span>"]
    GI.set b [#child := dot]
    void $ GI.on b #clicked (setColor hex)
    modifyIORef swatchButtons ((hex, b) :)
    Gtk.flowBoxAppend swatchRow b
  void $ GI.on random #clicked $ anySwatch >>= setColor
  forM_ [nameEntry, aboutEntry, hexEntry] $ \e -> void $ GI.on e #changed redrawPreview

  let draw = do
        typed <- T.toLower . T.strip <$> GI.get search #text
        defs <- readIORef known
        on' <- readIORef chosen
        rs <- readIORef rows
        forM_ rs $ \(d, b, mark) -> do
          Gtk.widgetSetVisible b (typed `T.isInfixOf` T.toLower (d.name <> " " <> d.description))
          let isOn = d.name `elem` on'
          (if isOn then Gtk.widgetAddCssClass else Gtk.widgetRemoveCssClass) b "chosen"
          GI.set mark [#visible := isOn]
        let exact = any ((== typed) . T.toLower . (.name)) defs
        raw <- T.strip <$> GI.get search #text
        GI.set createRow [#visible := not (T.null typed) && not exact]
        GI.set createText [#label := "+  Create label “" <> raw <> "”"]
      fill defs = do
        writeIORef known defs
        clearBox list
        made <- forM defs $ \d -> do
          b <- GI.new Gtk.Button [#cssClasses := ["label-row"]]
          content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
          check <- GI.new Gtk.Box [#valign := Gtk.AlignCenter, #cssClasses := ["check"]]
          mark <- Icon.new 11 Icon.Check
          #append check mark
          #append content check
          GI.new Gtk.Label [#useMarkup := True, #label := "<span foreground='#" <> unparse d.color <> "'>●</span>"] >>= #append content
          texts <- box 0
          GI.set texts [#hexpand := True, #valign := Gtk.AlignCenter]
          label d.name ["name"] >>= #append texts
          unless (T.null d.description) $ label d.description ["muted", "small"] >>= \l -> GI.set l [#ellipsize := Pango.EllipsizeModeEnd] >> #append texts l
          #append content texts
          GI.set b [#child := content]
          void $ GI.on b #clicked $ do
            modifyIORef chosen (\xs -> if d.name `elem` xs then filter (/= d.name) xs else d.name : xs)
            draw
          #append list b
          pure (d, b, mark)
        writeIORef rows made
        when (null defs) $ label "The repository has no labels yet." ["muted"] >>= #append list
        draw
      reload = do
        clearBox list
        label "Loading…" ["muted"] >>= #append list
        void $ forkIO $ do
          got <- p.load
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            either (\e -> clearBox list >> GI.set problem [#label := e, #visible := True]) fill got
            pure False
      open = do
        writeIORef chosen p.current
        GI.set search [#text := ""]
        GI.set problem [#visible := False]
        Gtk.stackSetVisibleChildName stack "pick"
        Gtk.popoverPopup popover
        void (Gtk.widgetGrabFocus search)
        reload
      newForm = do
        typed <- T.strip <$> GI.get search #text
        GI.set nameEntry [#text := typed]
        GI.set aboutEntry [#text := ""]
        GI.set formProblem [#visible := False]
        anySwatch >>= setColor
        Gtk.stackSetVisibleChildName stack "new"
        void (Gtk.widgetGrabFocus aboutEntry)
  void $ GI.on search #searchChanged draw
  void $ GI.on createRow #clicked newForm
  void $ GI.on cancel #clicked (Gtk.stackSetVisibleChildName stack "pick")
  void $ GI.on manage #clicked (Gtk.popoverPopdown popover >> p.manage)
  void $ GI.on createButton #clicked $ do
    name <- T.strip <$> GI.get nameEntry #text
    about <- T.strip <$> GI.get aboutEntry #text
    c <- colorOf
    forM_ c $ \k ->
      Button.busy createButton [cancel] "Creating…" (p.create Definition {name = name, color = k, description = about}) $ \case
        Left e -> GI.set formProblem [#label := e, #visible := True]
        Right () -> do
          modifyIORef chosen (name :)
          GI.set search [#text := ""]
          Gtk.stackSetVisibleChildName stack "pick"
          reload
  -- Closing saves what changed.
  void $ GI.on popover #closed $ do
    now <- readIORef chosen
    when (sort now /= sort p.current) $ void $ forkIO $ do
      result <- p.save now
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
        case result of
          Right () -> p.saved
          Left e -> do
            Gtk.popoverPopup popover
            GI.set problem [#label := "Not saved: " <> e, #visible := True]
        pure False
  pure open
  where
    box gap = GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := gap]
    label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]
    clearBox b = Gtk.widgetGetFirstChild b >>= mapM_ (\c -> Gtk.boxRemove b c >> clearBox b)
    chip k name = "<span background='#" <> unparse k <> "' foreground='" <> readable k <> "'> " <> escape name <> " </span>"
    escape = T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

-- | Black or white, whichever reads on the colour.
readable :: Color -> Text
readable k = if light then "#1B1D21" else "#FFFFFF"
  where
    channel i = case readHex (T.unpack (T.take 2 (T.drop i (unparse k)))) of
      [(v, "")] -> v :: Int
      _ -> 0
    light = 299 * channel 0 + 587 * channel 2 + 114 * channel 4 > 150000

-- | One of the swatches, chosen by the clock: random enough for a colour.
anySwatch :: IO Text
anySwatch = do
  t <- getPOSIXTime
  pure (swatches !! (floor (t * 1000) `mod` length swatches))

-- | The colours offered, as on the design: red, orange, amber, green, teal,
-- navy, blue, violet, pink, ink, grey, silver.
swatches :: [Text]
swatches = ["b42318", "c2410c", "8a5a00", "1f7a3a", "0f766e", "1e40af", "2f5fd0", "6d28d9", "be185d", "3d434c", "7a808a", "b9c0cb"]

css :: Text
css = mconcat
  [ "popover.u-label-picker-pop > contents { padding: 14px; border-radius: 12px; border: 1px solid var(--input-border); background: var(--surface); }"
  , "stack.u-label-picker label.picker-title { font-size: 15px; font-weight: 600; color: var(--text); }"
  , "stack.u-label-picker label.field { font-size: 13px; font-weight: 500; }"
  , "stack.u-label-picker label.small { font-size: 12px; }"
  , "stack.u-label-picker label.problem { color: var(--danger); font-size: 12px; }"
  , "stack.u-label-picker button.label-row { min-height: 40px; padding: 4px 8px; border: none; border-radius: 6px; box-shadow: none; background: transparent; background-image: none; color: var(--text); }"
  , "stack.u-label-picker button.label-row:hover { background: var(--border-subtle); }"
  , "stack.u-label-picker button.label-row:focus-visible { box-shadow: inset 0 0 0 2px var(--accent); }"
  , "stack.u-label-picker button.label-row.chosen { background: var(--accent-bg); }"
  , "stack.u-label-picker box.check { min-width: 16px; min-height: 16px; border-radius: 4px; border: 1.5px solid var(--input-border); color: #FFFFFF; }"
  , "stack.u-label-picker button.chosen box.check { border-color: var(--accent); background: var(--accent); }"
  , "stack.u-label-picker label.name { font-weight: 500; }"
  , "stack.u-label-picker button.create { min-height: 34px; border: none; box-shadow: none; background: transparent; background-image: none; color: var(--accent); font-weight: 500; }"
  , "stack.u-label-picker button.create label { margin-left: 0; }"
  , "stack.u-label-picker box.footer { padding-top: 8px; border-top: 1px solid var(--border-subtle); }"
  , "stack.u-label-picker button.link-button { min-height: 0; padding: 0; border: none; box-shadow: none; background: transparent; color: var(--accent); font-size: 13px; }"
  , "stack.u-label-picker button.swatch { min-width: 26px; min-height: 26px; padding: 0; border: none; border-radius: 13px; box-shadow: none; background: transparent; background-image: none; }"
  , "stack.u-label-picker button.swatch:hover { background: var(--border-subtle); }"
  , "stack.u-label-picker button.swatch.on { box-shadow: 0 0 0 2px var(--accent); }"
  ]
