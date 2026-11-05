{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A repository's Actions tab: its latest CI runs on the left, filtered
-- by workflow, branch and state, and the chosen run's jobs with their logs
-- on the right, as a pull request's Checks tab shows them.
module Usta.UI.Pages.RepositoryActions
  ( Actions (..)
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, join, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (diffUTCTime)
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Pages.Settings as Settings
import Usta.UI.Format (Clock, ago)
import Usta.UI.Page (refill)
import qualified Usta.UI.Components.Skeleton as Skeleton
import qualified Usta.UI.Pages.PullRequest.Checks as Checks
import Usta.UI.Style (Tone (..))
import Usta.Types.Check (Job, Run (..), State (..))

-- | What the tab asks of the forge; each runs off the main thread.
type Actions :: Type
data Actions = Actions
  { runs :: IO (Either Text [Run])
  , current :: IO (Either Text [Text])
    -- ^ The workflows the repository has now, which the filter offers; a
    -- deleted one's runs still list.
  , logOf :: Job -> IO (Either Text Text)
  , stepsOf :: Job -> IO (Either Text (State, [(Text, State)]))
  , runWorkflow :: Maybe (Gtk.Widget -> IO () -> IO ())
    -- ^ Opens Run workflow over the tab, and what to do once one started.
  , rerun :: Maybe (Int64 -> Bool -> IO (Either Text ()))
    -- ^ A run again, all or only its failed jobs; 'Nothing' where the forge
    -- cannot, as Forgejo, and the buttons are left out.
  , cancelRun :: Maybe (Int64 -> IO (Either Text ()))
  , rerunJob :: Maybe (Job -> IO (Either Text ()))
  }

new :: Clock -> Actions -> IO Gtk.Widget
new c actions = do
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 16, #cssClasses := ["u-repo-actions"]]
  -- The runs keep to a narrow column; the jobs and logs take the rest.
  left <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #hexpand := False, #widthRequest := 380]
  workflows <- Gtk.dropDownNewFromStrings ["All workflows"]
  branches <- Gtk.dropDownNewFromStrings ["Any branch"]
  states <- Gtk.dropDownNewFromStrings ("Any status" : map stateText [minBound ..])
  toolbar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  mapM_ (#append toolbar) [workflows, branches, states]
  runButton <- Button.new Button.defaults {Button.label = Just "▶ Run workflow", Button.variant = Button.Primary, Button.small = True}
  GI.set runButton [#hexpand := True, #halign := Gtk.AlignEnd, #visible := isJust actions.runWorkflow]
  #append toolbar runButton
  #append left toolbar
  list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle, #cssClasses := ["u-run-list"]]
  empty <- label "No runs here" ["muted", "empty"]
  Gtk.listBoxSetPlaceholder list (Just empty)
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-card", "list-card"]]
  #append card list
  -- Placeholder rows until the forge answers.
  loading <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  mapM_ (\_ -> Skeleton.listRow 300 >>= #append loading) [1 .. 6 :: Int]
  #append card loading
  scroll <- GI.new Gtk.ScrolledWindow [ #child := card, #vexpand := True, #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      , #propagateNaturalHeight := True, #valign := Gtk.AlignStart ]
  #append left scroll
  note <- label "" ["meta", "note"]
  GI.set note [#visible := False, #wrap := True]
  #append left note
  #append page left
  -- The chosen run's jobs and logs.
  right <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #hexpand := True, #widthRequest := 360, #cssClasses := ["u-card", "run-detail"]]
  #append page right

  items <- newIORef []
  -- What lists the runs anew; set once it exists, below.
  reloading <- newIORef (pure ())
  shownRows <- newIORef []
  let draw = do
        everything <- readIORef items
        workflow <- selectedText workflows
        branch <- selectedText branches
        state <- Gtk.dropDownGetSelected states
        let rows = [ r | r <- everything
                   , maybe True (== r.workflow) workflow
                   , maybe True (== r.branch) branch
                   , state == 0 || fromIntegral state - 1 == fromEnum r.state ]
        writeIORef shownRows rows
        refill list (runRow c) rows
        first <- Gtk.listBoxGetRowAtIndex list 0
        Gtk.listBoxSelectRow list first
        when (null rows) (clear right)
      fill (runs, named) = do
        writeIORef items runs
        -- Without the forge's list, the workflows the runs name.
        choices workflows "All workflows" (either (const (nub (map (.workflow) runs))) id named)
        choices branches "Any branch" (nub (map (.branch) runs))
        draw
  forM_ [workflows, branches, states] $ \d -> void $ GI.on d (GI.PropertyNotify #selected) (const draw)
  void $ GI.on list #rowSelected $ \selected -> forM_ selected $ \row -> do
    index <- Gtk.listBoxRowGetIndex row
    rows <- readIORef shownRows
    when (index >= 0 && fromIntegral index < length rows) $ do
      clear right
      runDetail c actions (join (readIORef reloading)) (rows !! fromIntegral index) >>= #append right

  -- The runs again, after one was started or changed; the newest is
  -- selected.
  let fetch = void $ forkIO $ do
        result <- try ((,) <$> actions.runs <*> actions.current)
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          parent <- Gtk.widgetGetParent loading
          forM_ parent $ \_ -> Gtk.boxRemove card loading
          case result of
            Right (Right runs, named) -> fill (runs, named) >> GI.set note [#visible := False]
            Right (Left problem, _) -> GI.set note [#label := "The forge could not list the runs. " <> problem, #visible := True]
            Left (e :: SomeException) -> GI.set note [#label := "The forge could not list the runs. " <> T.pack (show e), #visible := True]
          pure False
      -- A new run takes a moment to list.
      reload = void $ GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT 3 (fetch >> pure False)
  writeIORef reloading reload
  forM_ actions.runWorkflow $ \opening -> void $ GI.on runButton #clicked $ do
    anchor <- Gtk.toWidget runButton
    opening anchor reload
  fetch
  Gtk.toWidget page
  where
    -- Keeps the choice when it is still there.
    choices d first names = do
      before <- selectedText d
      let options = first : names
      model <- Gtk.stringListNew (Just options)
      Gtk.dropDownSetModel d (Just model)
      Gtk.dropDownSetSelected d (maybe 0 (\n -> fromIntegral (length (takeWhile (/= n) options))) before)
    selectedText d = do
      item <- Gtk.dropDownGetSelectedItem d
      index <- Gtk.dropDownGetSelected d
      case item of
        Just o | index > 0 -> do
          s <- GI.unsafeCastTo Gtk.StringObject o
          Just <$> Gtk.stringObjectGetString s
        _ -> pure Nothing

runRow :: Clock -> Run -> IO Gtk.Box
runRow c r = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  stateIcon r.state >>= #append row
  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 3, #hexpand := True]
  title <- label r.title ["title"]
  GI.set title [#ellipsize := Pango.EllipsizeModeEnd]
  #append body title
  meta <- label (T.intercalate " · " [r.workflow, r.branch, r.event, T.take 7 r.sha]) ["meta"]
  GI.set meta [#ellipsize := Pango.EllipsizeModeEnd]
  #append body meta
  #append row body
  forM_ [(took r, 60), (ago c r.started, 90)] $ \(text, width) -> do
    l <- label text ["meta"]
    GI.set l [#widthRequest := width]
    #append row l
  pure row

-- | The run's head, with Re-run and Cancel where the forge can, and its
-- jobs, each with its log.
runDetail :: Clock -> Actions -> IO () -> Run -> IO Gtk.Box
runDetail c actions changed r = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #vexpand := True]
  head' <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #cssClasses := ["run-head"]]
  top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  stateIcon r.state >>= #append top
  name <- label (r.workflow <> " · run #" <> T.pack (show r.number)) ["title"]
  GI.set name [#hexpand := True]
  #append top name
  label ("started " <> ago c r.started) ["meta"] >>= #append top
  #append head' top
  label (r.event <> " on " <> r.branch) ["meta"] >>= #append head'
  buttons <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #halign := Gtk.AlignEnd]
  problem <- label "" ["meta", "problem"]
  GI.set problem [#visible := False, #wrap := True]
  let running = r.state `elem` [Running, Waiting]
      act text danger work = do
        b <- Button.new Button.defaults {Button.label = Just text, Button.small = True, Button.variant = if danger then Button.Danger else Button.Secondary}
        void $ GI.on b #clicked $ Button.busy b ([] :: [Gtk.Widget]) "…" work $ \case
          Right () -> changed
          Left why -> GI.set problem [#label := why, #visible := True]
        #append buttons b
  forM_ actions.rerun $ \again -> when (not running) $ do
    when (r.state == Failed) $ act "↻ Re-run failed jobs" False (again r.id True)
    act "↻ Re-run all" False (again r.id False)
  forM_ actions.cancelRun $ \stop -> when running $ do
    b <- Button.new Button.defaults {Button.label = Just "■ Cancel run", Button.small = True, Button.variant = Button.Danger}
    void $ GI.on b #clicked $ do
      anchor <- Gtk.toWidget b
      Settings.confirm anchor "Cancel this run?" "Its jobs stop where they are; it can be run again after." "Cancel run" $
        Button.busy b ([] :: [Gtk.Widget]) "Cancelling…" (stop r.id) $ \case
          Right () -> changed
          Left why -> GI.set problem [#label := why, #visible := True]
    #append buttons b
  #append head' buttons
  #append head' problem
  #append box head'
  Checks.new Checks.Model {provider = "Jobs", checks = r.jobs, logOf = actions.logOf, stepsOf = actions.stepsOf, rerunJob = actions.rerunJob, rerunDone = changed} >>= #append box
  pure box

-- | How long a run took, or that it still runs.
took :: Run -> Text
took r
  | r.state `elem` [Running, Waiting] = "running"
  | s < 60 = T.pack (show s) <> "s"
  | otherwise = T.pack (show (s `div` 60)) <> "m " <> T.pack (show (s `mod` 60)) <> "s"
  where
    s = round (diffUTCTime r.updated r.started) :: Int

stateIcon :: State -> IO Gtk.Image
stateIcon state = case state of
  Passed -> Icon.toned 16 Success Icon.Check
  Failed -> Icon.toned 16 Danger Icon.Cross
  Running -> Icon.toned 16 Warning Icon.Clock
  Waiting -> Icon.toned 16 Neutral Icon.Clock
  Skipped -> Icon.toned 16 Neutral Icon.Skipped

stateText :: State -> Text
stateText = T.pack . show

clear :: Gtk.Box -> IO ()
clear box = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear box)

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-repo-actions { padding: 14px 20px; }"
  , "box.u-repo-actions dropdown > button {"
  , "  min-height: 34px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 6px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none; font-size: 13px; }"
  , "box.u-repo-actions box.list-card { border-radius: 10px; }"
  , "box.u-repo-actions box.run-detail { border-radius: 10px; }"
  , "box.u-repo-actions box.run-head { padding: 12px 14px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-repo-actions label.title { font-weight: 600; }"
  , "box.u-repo-actions label.meta { font-size: 12px; color: var(--text-muted); }"
  , "box.u-repo-actions label.note { padding: 2px 4px; }"
  , "list.u-run-list { background: transparent; }"
  , "list.u-run-list row { padding: 10px 14px; border-bottom: 1px solid var(--border-subtle); background: transparent; color: var(--text); }"
  , "list.u-run-list row:last-child { border-bottom: none; }"
  , "list.u-run-list row:selected { background: var(--accent-bg); box-shadow: inset 3px 0 0 var(--accent); }"
  , "list.u-run-list label.empty { padding: 32px; }"
  ]
