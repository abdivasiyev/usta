{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Run workflow: a workflow that can be started by hand, the branch to
-- run it on, and its inputs as its file asks for them: a choice as a
-- dropdown, a boolean as a switch, the rest as text. Run starts it; there
-- is nothing more to confirm.
module Usta.UI.Components.RunWorkflow
  ( Dialog (..)
  , open
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.BranchPicker as BranchPicker
import qualified Usta.UI.Components.Button as Button
import Usta.UI.Format (Clock)
import qualified Usta.UI.Platform as Platform
import Usta.Types.Branch (Branch)
import Usta.Types.Workflow (Input (..), Kind (..), Workflow (..))

type Dialog :: Type
data Dialog = Dialog
  { clock :: Clock
  , defaultBranch :: Text
  , branch :: Text
    -- ^ Chosen at first: the default, or a pull request's.
  , branches :: IO (Either Text [Branch])
  , workflows :: Text -> IO (Either Text [Workflow])
    -- ^ Those of a branch; off the main thread.
  , run :: Workflow -> Text -> [(Text, Text)] -> IO (Either Text ())
  , started :: IO ()
    -- ^ Once the forge took the run.
  }

-- | The dialog over the window @anchor@ is in.
open :: Gtk.Widget -> Dialog -> IO ()
open anchor d = do
  root <- Gtk.widgetGetRoot anchor
  parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
  dialog <- GI.new Gtk.Window [ #title := "Run workflow", #modal := True, #defaultWidth := 460
                              , #destroyWithParent := True, #cssClasses := ["usta", "u-run-workflow"] ]
  forM_ parent $ \p -> Gtk.windowSetTransientFor dialog (Just p)
  header <- GI.new Gtk.HeaderBar []
  Platform.configureHeaderBar Platform.current header
  GI.new Gtk.Label [#label := "Run workflow", #cssClasses := ["title"]] >>= \t -> GI.set header [#titleWidget := t]
  GI.set dialog [#titlebar := header]

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8, #cssClasses := ["form"]]
  caption "Workflow" >>= #append body
  choice <- Gtk.dropDownNewFromStrings []
  #append body choice
  caption "Branch" >>= #append body
  branchSlot <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  #append body branchSlot
  fields <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
  #append body fields
  note <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #cssClasses := ["muted"], #visible := False]
  #append body note

  footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd, #cssClasses := ["footer"]]
  cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
  go <- Button.new Button.defaults {Button.label = Just "Run", Button.variant = Button.Primary}
  GI.set go [#sensitive := False]
  #append footer cancel
  #append footer go
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append content body
  #append content footer
  GI.set dialog [#child := content]

  branch <- newIORef d.branch
  found <- newIORef []
  -- What reads each input's value, by its name.
  readers <- newIORef []
  let say text = GI.set note [#label := text, #visible := not (T.null text)]
      drawFields = do
        clear fields
        i <- fromIntegral <$> Gtk.dropDownGetSelected choice
        ws <- readIORef found
        case drop i ws of
          w : _ -> do
            rs <- forM w.inputs $ \input -> do
              caption (input.name <> (if input.required then " · required" else "")) >>= #append fields
              when (not (T.null input.description)) $
                GI.new Gtk.Label [#label := input.description, #xalign := 0, #wrap := True, #cssClasses := ["muted"]] >>= #append fields
              reader <- field input
              pure (input, reader)
            writeIORef readers rs
            GI.set go [#sensitive := True]
          [] -> writeIORef readers [] >> GI.set go [#sensitive := False]
      field input = case input.kind of
        ChoiceInput options -> do
          dd <- Gtk.dropDownNewFromStrings options
          let at = length (takeWhile (/= fromMaybe "" input.preset) options)
          when (at < length options) $ Gtk.dropDownSetSelected dd (fromIntegral at)
          #append fields dd
          pure $ do
            k <- fromIntegral <$> Gtk.dropDownGetSelected dd
            pure (if k < length options then options !! k else "")
        BooleanInput -> do
          row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
          sw <- GI.new Gtk.Switch [#active := (input.preset == Just "true"), #halign := Gtk.AlignStart]
          #append row sw
          #append fields row
          pure ((\on -> if on then "true" else "false") <$> GI.get sw #active)
        _ -> do
          entry <- GI.new Gtk.Entry [#text := fromMaybe "" input.preset, #cssClasses := ["mono"]]
          #append fields entry
          pure (Gtk.editableGetText entry)
      -- The workflows the branch has.
      load = do
        b <- readIORef branch
        say ("Reading the workflows on " <> b <> "…")
        GI.set go [#sensitive := False]
        void $ forkIO $ do
          got <- try (d.workflows b) :: IO (Either SomeException (Either Text [Workflow]))
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            case got of
              Right (Right ws) -> do
                before <- readIORef found
                writeIORef found ws
                i <- fromIntegral <$> Gtk.dropDownGetSelected choice
                let wanted = case drop i before of
                      w : _ -> w.name
                      [] -> ""
                model <- Gtk.stringListNew (Just (map (.name) ws))
                Gtk.dropDownSetModel choice (Just model)
                Gtk.dropDownSetSelected choice (fromIntegral (min (length ws) (length (takeWhile ((/= wanted) . (.name)) ws))))
                say (if null ws then b <> " has no workflow that can be started by hand (on: workflow_dispatch)." else "")
                drawFields
              Right (Left problem) -> say problem
              Left e -> say (T.pack (show e))
            pure False
      drawBranch = do
        clear branchSlot
        b <- readIORef branch
        picker <- BranchPicker.new BranchPicker.Picker
          { shown = b, defaultBranch = d.defaultBranch, clock = d.clock, branches = d.branches, tags = Nothing
          , choose = \name -> writeIORef branch name >> drawBranch >> load
          , note = "The workflow and its inputs come from this branch" }
        #append branchSlot picker
  drawBranch
  void $ GI.on choice (GI.PropertyNotify #selected) (const drawFields)
  void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
  void $ GI.on go #clicked $ do
    i <- fromIntegral <$> Gtk.dropDownGetSelected choice
    ws <- readIORef found
    rs <- readIORef readers
    b <- readIORef branch
    forM_ (take 1 (drop i ws)) $ \w -> do
      values <- forM rs $ \(input, reader) -> (,) input <$> reader
      case [input.name | (input, v) <- values, input.required, T.null (T.strip v)] of
        missing : _ -> say ("Give " <> missing <> " a value.")
        [] -> Button.busy go [cancel] "Starting…" (d.run w b [(input.name, v) | (input, v) <- values]) $ \case
          Right () -> Gtk.windowDestroy dialog >> d.started
          Left problem -> say ("The forge did not start it: " <> problem)
  load
  #present dialog
  where
    caption t = GI.new Gtk.Label [#label := t, #xalign := 0, #cssClasses := ["step"]]
    clear box = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear box)

css :: Text
css = mconcat
  [ "window.u-run-workflow headerbar { background: var(--surface); border-bottom: 1px solid var(--border); box-shadow: none; }"
  , "window.u-run-workflow box.form { padding: 18px 20px; background: var(--surface); }"
  , "window.u-run-workflow label.step { font-size: 12px; color: var(--text-soft); margin-top: 4px; }"
  , "window.u-run-workflow label.muted { color: var(--text-muted); font-size: 12px; }"
  , "window.u-run-workflow box.footer { padding: 12px 20px; background: var(--window); border-top: 1px solid var(--border); }"
  , "window.u-run-workflow dropdown > button { min-height: 34px; border: 1px solid var(--input-border); border-radius: 6px; background: var(--surface); background-image: none; box-shadow: none; }"
  , "window.u-run-workflow entry { min-height: 34px; border: 1px solid var(--input-border); border-radius: 6px; }"
  ]
