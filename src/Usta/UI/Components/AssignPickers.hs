{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The assignees and milestone pickers of an issue or pull request, as
-- the design draws them. Assignees: "Assign yourself" first, those
-- assigned ticked, then the others with their full names; several at
-- once. Milestone: one choice, "No milestone" or a milestone with its
-- progress and due date, closed ones folded. Changes save when a picker
-- closes; a failed save opens it again and says why.
module Usta.UI.Components.AssignPickers
  ( Assignees (..)
  , assignees
  , Milestones (..)
  , milestone
  , progressText
  , assigneesSection
  , milestoneSection
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM, forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Card as Card
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Icon as Icon
import Usta.UI.Format (Clock, date)
import Usta.Types.Login (Login)
import Usta.Types.Milestone (Milestone (..))
import Usta.Types.Parse (unparse)
import Usta.Types.Reviewer (Candidate (..), Who (..))

type Assignees :: Type
data Assignees = Assignees
  { current :: [Login]
  , me :: Maybe Login
    -- ^ Who usta signs in as here, for "Assign yourself".
  , load :: IO (Either Text [Candidate])
  , save :: [Login] -> IO (Either Text ())
  , saved :: IO ()
  , place :: Maybe Avatar.Place
    -- ^ The forge the people's pictures come from.
  }

type Milestones :: Type
data Milestones = Milestones
  { current :: Maybe Text
    -- ^ The milestone's title.
  , load :: IO (Either Text [Milestone])
  , save :: Maybe Text -> IO (Either Text ())
  , manage :: IO ()
  , saved :: IO ()
  , clock :: Clock
  }

-- | "due Nov 30 · 8 of 14 closed", or "closed Oct 12 · …" once closed.
progressText :: Clock -> Milestone -> Text
progressText c m = when' <> " · " <> T.pack (show m.closedItems) <> " of " <> T.pack (show (m.openItems + m.closedItems)) <> " closed"
  where
    when' = case (m.open, m.due, m.closedAt) of
      (False, _, Just t) -> "closed " <> date c t
      (False, _, Nothing) -> "closed"
      (True, Just t, _) -> "due " <> date c t
      (True, Nothing, _) -> "no due date"

-- | A popover under the anchor with a title, a search, a scrolling list,
-- and a footer; and what fills the list.
type Frame :: Type
data Frame = Frame
  { popover :: Gtk.Popover
  , search :: Gtk.SearchEntry
  , list :: Gtk.Box
  , problem :: Gtk.Label
  , footer :: Gtk.Box
  }

frame :: Text -> Text -> Gtk.Button -> IO Frame
frame title placeholder anchor = do
  outer <- box 0
  GI.set outer [#widthRequest := 340, #cssClasses := ["u-assign-picker"]]
  top <- box 10
  GI.set top [#cssClasses := ["top"]]
  label title ["picker-title"] >>= #append top
  search <- GI.new Gtk.SearchEntry [#placeholderText := placeholder]
  #append top search
  list <- box 0
  scroll <- GI.new Gtk.ScrolledWindow [#child := list, #maxContentHeight := 360, #propagateNaturalHeight := True, #hscrollbarPolicy := Gtk.PolicyTypeNever]
  #append top scroll
  problem <- label "" ["problem"]
  GI.set problem [#visible := False, #wrap := True]
  #append top problem
  #append outer top
  footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["bottom"]]
  #append outer footer
  popover <- GI.new Gtk.Popover [#child := outer, #position := Gtk.PositionTypeBottom, #hasArrow := False, #cssClasses := ["u-assign-picker-pop"]]
  Gtk.widgetSetParent popover anchor
  void $ GI.on anchor #destroy (Gtk.widgetUnparent popover)
  pure Frame {popover, search, list, problem, footer}

-- | Loads in the background, then fills; a failure shows in the picker.
loadInto :: Frame -> IO (Either Text a) -> (a -> IO ()) -> IO ()
loadInto f act fill = do
  clearBox f.list
  label "Loading…" ["muted", "small"] >>= #append f.list
  void $ forkIO $ do
    got <- act
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      either (\e -> clearBox f.list >> GI.set f.problem [#label := e, #visible := True]) fill got
      pure False

-- | Saves on close when the choice changed; a failure reopens and says so.
saveOnClose :: Eq a => Frame -> IORef a -> a -> (a -> IO (Either Text ())) -> IO () -> IO ()
saveOnClose f chosen start save saved =
  void $ GI.on f.popover #closed $ do
    now <- readIORef chosen
    when (now /= start) $ void $ forkIO $ do
      result <- save now
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
        case result of
          Right () -> saved
          Left e -> Gtk.popoverPopup f.popover >> GI.set f.problem [#label := "Not saved: " <> e, #visible := True]
        pure False

assignees :: Assignees -> Gtk.Button -> IO (IO ())
assignees a anchor = do
  f <- frame "Assignees" "Search people" anchor
  counter <- label "" ["muted", "small"]
  GI.set counter [#hexpand := True]
  #append f.footer counter
  label "Changes save when it closes" ["muted", "small"] >>= #append f.footer
  chosen <- newIORef (sort a.current)
  rows <- newIORef []
  selfRow <- GI.new Gtk.Button [#cssClasses := ["self"], #visible := False]
  selfBox <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  label "+" ["plus"] >>= #append selfBox
  selfText <- label "Assign yourself" []
  GI.set selfText [#hexpand := True]
  #append selfBox selfText
  label "Ctrl A" ["key"] >>= #append selfBox
  GI.set selfRow [#child := selfBox]
  let draw = do
        typed <- T.toLower . T.strip <$> GI.get f.search #text
        now <- readIORef chosen
        rs <- readIORef rows
        forM_ rs $ \(login, haystack, b, mark, tag) -> do
          let on' = login `elem` now
          Gtk.widgetSetVisible b (typed `T.isInfixOf` haystack)
          (if on' then Gtk.widgetAddCssClass else Gtk.widgetRemoveCssClass) b "chosen"
          GI.set mark [#visible := on']
          GI.set tag [#visible := on' && login `notElem` a.current]
        GI.set selfRow [#visible := maybe False (`notElem` now) a.me && T.null typed]
        let added = length [l | l <- now, l `notElem` a.current]
        GI.set counter [#label := T.pack (show (length (filter (`elem` a.current) now))) <> " assigned" <> (if added > 0 then " · " <> T.pack (show added) <> " more" else "")]
      toggle login = modifyIORef chosen (\xs -> sort (if login `elem` xs then filter (/= login) xs else login : xs)) >> draw
      fill cs = do
        clearBox f.list
        #append f.list selfRow
        let people = [(l, c.about) | c <- cs, Person l <- [c.who]]
            (assigned, others) = (filter ((`elem` a.current) . fst) people, filter ((`notElem` a.current) . fst) people)
        made <- fmap concat $ forM [("Assigned", assigned), ("Collaborators", others)] $ \(heading, group) ->
          if null group then pure [] else do
            label (T.toUpper heading) ["lbl"] >>= #append f.list
            forM group $ \(login, name) -> do
              (b, mark, tag) <- personRow a.place (unparse login) name
              void $ GI.on b #clicked (toggle login)
              #append f.list b
              pure (login, T.toLower (unparse login <> " " <> name), b, mark, tag)
        when (null people) $ label "Nobody can be assigned here." ["muted"] >>= #append f.list
        writeIORef rows made
        draw
      open = do
        writeIORef chosen (sort a.current)
        GI.set f.search [#text := ""]
        GI.set f.problem [#visible := False]
        Gtk.popoverPopup f.popover
        void (Gtk.widgetGrabFocus f.search)
        loadInto f a.load fill
  void $ GI.on f.search #searchChanged draw
  forM_ a.me $ \self -> do
    void $ GI.on selfRow #clicked (toggle self)
    keys <- GI.new Gtk.ShortcutController [#propagationPhase := Gtk.PropagationPhaseCapture]
    shortcut keys "<Primary>a" $ do
      now <- readIORef chosen
      unless (self `elem` now) (toggle self)
    Gtk.widgetAddController f.popover keys
  saveOnClose f chosen (sort a.current) (a.save) a.saved
  pure open

milestone :: Milestones -> Gtk.Button -> IO (IO ())
milestone m anchor = do
  f <- frame "Milestone" "Search milestones" anchor
  manage <- GI.new Gtk.Button [#label := "Manage milestones", #cssClasses := ["link-button"]]
  #append f.footer manage
  note <- label "Changes save when it closes" ["muted", "small"]
  GI.set note [#hexpand := True, #xalign := 1]
  #append f.footer note
  chosen <- newIORef m.current
  rows <- newIORef []
  foldOpen <- newIORef False
  folder <- GI.new Gtk.Button [#halign := Gtk.AlignStart, #cssClasses := ["fold"]]
  closedHead <- label "CLOSED" ["lbl"]
  openHead <- label "OPEN" ["lbl"]
  let draw = do
        typed <- T.toLower . T.strip <$> GI.get f.search #text
        now <- readIORef chosen
        unfolded <- readIORef foldOpen
        rs <- readIORef rows
        shownOpen <- forM rs $ \(title, isOpen, b, dot) -> do
          let hit = typed `T.isInfixOf` T.toLower (maybe "no milestone" id title)
              shown = hit && (isOpen || unfolded || not (T.null typed))
          Gtk.widgetSetVisible b shown
          (if now == title then Gtk.widgetAddCssClass else Gtk.widgetRemoveCssClass) b "chosen"
          (if now == title then Gtk.widgetAddCssClass else Gtk.widgetRemoveCssClass) dot "on"
          pure (shown && isOpen && title /= Nothing)
        GI.set openHead [#visible := or shownOpen]
        let closedCount = length [() | (_, False, _, _) <- rs]
        GI.set folder [#visible := closedCount > 0 && T.null typed, #label := (if unfolded then "▴ " else "▾ ") <> T.pack (show closedCount) <> (if closedCount == 1 then " closed milestone" else " closed milestones")]
        GI.set closedHead [#visible := closedCount > 0 && (unfolded || not (T.null typed))]
      pickTitle title = writeIORef chosen title >> draw >> Gtk.popoverPopdown f.popover
      fill ms = do
        clearBox f.list
        if null ms
          then do
            box' <- box 6
            GI.set box' [#cssClasses := ["message"]]
            label "This repository has no milestones" ["message-title"] >>= #append box'
            t <- label "Milestones group issues and pull requests toward a release. Create one on the forge." ["muted", "small"]
            GI.set t [#wrap := True]
            #append box' t
            #append f.list box'
            writeIORef rows []
          else do
            (noneB, noneDot) <- choiceRow "No milestone" Nothing
            Gtk.widgetAddCssClass noneB "none"
            void $ GI.on noneB #clicked (pickTitle Nothing)
            #append f.list noneB
            #append f.list openHead
            let row x = do
                  (b, dot) <- choiceRow x.title (Just (fraction x, progressText m.clock x))
                  void $ GI.on b #clicked (pickTitle (Just x.title))
                  #append f.list b
                  pure (Just x.title, x.open, b, dot)
            openRows <- forM (filter (.open) ms) row
            #append f.list folder
            #append f.list closedHead
            closedRows <- forM (filter (not . (.open)) ms) row
            writeIORef rows ((Nothing, True, noneB, noneDot) : openRows <> closedRows)
        draw
      open = do
        writeIORef chosen m.current
        writeIORef foldOpen False
        GI.set f.search [#text := ""]
        GI.set f.problem [#visible := False]
        Gtk.popoverPopup f.popover
        void (Gtk.widgetGrabFocus f.search)
        loadInto f m.load fill
  void $ GI.on folder #clicked (modifyIORef foldOpen not >> draw)
  void $ GI.on f.search #searchChanged draw
  void $ GI.on manage #clicked (Gtk.popoverPopdown f.popover >> m.manage)
  saveOnClose f chosen m.current m.save m.saved
  pure open
  where
    fraction x = let total = x.openItems + x.closedItems in if total == 0 then 0 else fromIntegral x.closedItems / fromIntegral total

-- | The sidebar's Assignees: who, with Edit; with nobody, "No one" and
-- "Assign yourself", which assigns you at once. Gives the section and
-- what opens the picker.
assigneesSection :: Assignees -> IO (Gtk.Box, IO ())
assigneesSection a = do
  section <- box 6
  (header, edit) <- sectionHead "Assignees" "Edit assignees (a)"
  #append section header
  forM_ a.current $ \login -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    Avatar.person 22 a.place (unparse login) >>= #append row
    label (unparse login) [] >>= #append row
    #append section row
  when (null a.current) $ do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    label "No one" ["muted"] >>= #append row
    forM_ a.me $ \self -> do
      label "·" ["muted"] >>= #append row
      link <- GI.new Gtk.Button [#label := "Assign yourself", #cssClasses := ["link-button"]]
      void $ GI.on link #clicked $ do
        GI.set link [#sensitive := False]
        void $ forkIO $ do
          result <- a.save [self]
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            either (\e -> GI.set link [#sensitive := True, #tooltipText := e]) (const a.saved) result
            pure False
      #append row link
    #append section row
  open <- assignees a edit
  void $ GI.on edit #clicked open
  pure (section, open)

-- | The sidebar's Milestone: its title, progress and due date, with Edit.
milestoneSection :: Milestones -> Maybe Milestone -> IO (Gtk.Box, IO ())
milestoneSection m info = do
  section <- box 6
  (header, edit) <- sectionHead "Milestone" "Edit milestone (m)"
  #append section header
  case (info, m.current) of
    (Just x, _) -> do
      label x.title ["name"] >>= #append section
      label (progressText m.clock x) ["muted", "small"] >>= #append section
      let total = x.openItems + x.closedItems
      GI.new Gtk.ProgressBar [#fraction := (if total == 0 then 0 else fromIntegral x.closedItems / fromIntegral total), #cssClasses := ["milestone-bar"]]
        >>= #append section
    (Nothing, Just title) -> label title [] >>= #append section
    (Nothing, Nothing) -> label "None" ["muted"] >>= #append section
  open <- milestone m edit
  void $ GI.on edit #clicked open
  pure (section, open)

-- | A section's title and its Edit link.
sectionHead :: Text -> Text -> IO (Gtk.Box, Gtk.Button)
sectionHead title hint = do
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["u-assign-head"]]
  t <- Card.sectionTitle title
  GI.set t [#hexpand := True]
  #append header t
  edit <- GI.new Gtk.Button [#label := "Edit", #tooltipText := hint, #cssClasses := ["link-button"]]
  #append header edit
  pure (header, edit)

-- | A person to tick: box, avatar, login and full name, and "new".
personRow :: Maybe Avatar.Place -> Text -> Text -> IO (Gtk.Button, Gtk.Image, Gtk.Label)
personRow place login name = do
  b <- GI.new Gtk.Button [#cssClasses := ["row"]]
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  check <- GI.new Gtk.Box [#valign := Gtk.AlignCenter, #cssClasses := ["check"]]
  mark <- Icon.new 11 Icon.Check
  #append check mark
  #append content check
  Avatar.person 24 place login >>= #append content
  texts <- box 0
  GI.set texts [#hexpand := True, #valign := Gtk.AlignCenter]
  label login ["name"] >>= #append texts
  unless (T.null name) $ label name ["muted", "small"] >>= \l -> GI.set l [#ellipsize := Pango.EllipsizeModeEnd] >> #append texts l
  #append content texts
  tag <- label "new" ["tag"]
  #append content tag
  GI.set b [#child := content]
  pure (b, mark, tag)

-- | One choice: a round mark, the title, and its progress when it has one.
choiceRow :: Text -> Maybe (Double, Text) -> IO (Gtk.Button, Gtk.Box)
choiceRow title progress = do
  b <- GI.new Gtk.Button [#cssClasses := ["row"]]
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  dot <- GI.new Gtk.Box [#valign := Gtk.AlignCenter, #cssClasses := ["radio"]]
  #append content dot
  texts <- box 3
  GI.set texts [#hexpand := True, #valign := Gtk.AlignCenter]
  label title (maybe ["muted"] (const ["name"]) progress) >>= #append texts
  forM_ progress $ \(done, text) -> do
    line <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    bar <- GI.new Gtk.ProgressBar [#fraction := done, #widthRequest := 70, #hexpand := False, #halign := Gtk.AlignStart, #valign := Gtk.AlignCenter]
    #append line bar
    label text ["muted", "small"] >>= #append line
    #append texts line
  #append content texts
  GI.set b [#child := content]
  pure (b, dot)

box :: Int -> IO Gtk.Box
box gap = GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := fromIntegral gap]

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

clearBox :: Gtk.Box -> IO ()
clearBox b = Gtk.widgetGetFirstChild b >>= mapM_ (\c -> Gtk.boxRemove b c >> clearBox b)

css :: Text
css = mconcat
  [ "button.link-button { min-height: 0; padding: 0 2px; border: none; box-shadow: none; background: transparent; background-image: none; color: var(--accent); font-size: 13px; }"
  , "progressbar.milestone-bar, progressbar.milestone-bar trough, progressbar.milestone-bar progress { min-width: 0; }"
  , "progressbar.milestone-bar trough { min-height: 6px; border-radius: 3px; background: var(--border-subtle); }"
  , "progressbar.milestone-bar progress { min-height: 6px; border-radius: 3px; background: var(--success); }"
  , "popover.u-assign-picker-pop > contents { padding: 0; border-radius: 12px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: 0 12px 32px rgba(27,29,33,0.16); }"
  , "box.u-assign-picker box.top { padding: 14px 14px 8px; }"
  , "box.u-assign-picker label.picker-title { font-size: 15px; font-weight: 600; color: var(--text); }"
  , "box.u-assign-picker label.lbl { margin: 8px 8px 2px; font-size: 11px; font-weight: 600; color: var(--text-muted); }"
  , "box.u-assign-picker label.small { font-size: 12px; }"
  , "box.u-assign-picker label.problem { color: var(--danger); font-size: 12px; }"
  , "box.u-assign-picker button.row, box.u-assign-picker button.self, box.u-assign-picker button.fold {"
  , "  min-height: 40px; padding: 4px 8px; border: none; border-radius: 6px; box-shadow: none; background: transparent; background-image: none; color: var(--text); }"
  , "box.u-assign-picker button.row:hover, box.u-assign-picker button.self:hover { background: var(--border-subtle); }"
  , "box.u-assign-picker button.row:focus-visible, box.u-assign-picker button.self:focus-visible { box-shadow: inset 0 0 0 2px var(--accent); }"
  , "box.u-assign-picker button.row.chosen { background: var(--accent-bg); }"
  , "box.u-assign-picker button.self { min-height: 34px; color: var(--accent); font-weight: 500; }"
  , "box.u-assign-picker label.plus { font-size: 16px; font-weight: 600; }"
  , "box.u-assign-picker button.fold { min-height: 28px; color: var(--text-muted); font-size: 12px; }"
  , "box.u-assign-picker label.key { font-family: 'JetBrains Mono', monospace; font-size: 11px; padding: 0 5px; border: 1px solid var(--border); border-radius: 4px; color: var(--text); }"
  , "box.u-assign-picker box.check { min-width: 16px; min-height: 16px; border-radius: 4px; border: 1.5px solid var(--input-border); color: #FFFFFF; }"
  , "box.u-assign-picker button.chosen box.check { border-color: var(--accent); background: var(--accent); }"
  , "box.u-assign-picker box.radio { min-width: 16px; min-height: 16px; border-radius: 8px; border: 1.5px solid var(--input-border); background: var(--surface); }"
  , "box.u-assign-picker box.radio.on { border: 5px solid var(--accent); min-width: 6px; min-height: 6px; }"
  , "box.u-assign-picker label.name { font-weight: 500; }"
  , "box.u-assign-picker label.tag { padding: 1px 8px; border-radius: 10px; font-size: 12px; font-weight: 500; background: var(--accent-bg); color: var(--accent-soft); }"
  , "box.u-assign-picker progressbar, box.u-assign-picker progressbar trough, box.u-assign-picker progressbar progress { min-width: 0; }"
  , "box.u-assign-picker progressbar trough { min-height: 6px; border-radius: 3px; background: var(--border-subtle); }"
  , "box.u-assign-picker progressbar progress { min-height: 6px; border-radius: 3px; background: var(--success); }"
  , "box.u-assign-picker box.message { margin: 8px 6px 6px; }"
  , "box.u-assign-picker label.message-title { font-weight: 600; color: var(--text); }"
  , "box.u-assign-picker box.bottom { padding: 10px 14px; border-top: 1px solid var(--border-subtle); background: var(--window); border-radius: 0 0 12px 12px; }"
  , "box.u-assign-picker button.link-button { min-height: 0; padding: 0; border: none; box-shadow: none; background: transparent; color: var(--accent); font-size: 13px; }"
  ]
