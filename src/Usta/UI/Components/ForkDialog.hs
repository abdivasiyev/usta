{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Fork: where the fork goes (you or one of your organizations) and its
-- name. A fork you have already is offered instead of a second one.
module Usta.UI.Components.ForkDialog
  ( Dialog (..)
  , open
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Platform as Platform

type Dialog :: Type
data Dialog = Dialog
  { repo :: Text
    -- ^ The one to fork, e.g. "aliftechuz/mobi-backend".
  , defaultBranch :: Text
  , reason :: Maybe Text
    -- ^ Why it is asked, as before a pull request to a repository you
    -- cannot push to.
  , branchOnly :: Bool
    -- ^ Whether the forge can copy only the default branch (GitHub).
  , options :: IO (Either Text ([Text], [Text]))
    -- ^ Where it can go, you first, and the forks of it there already;
    -- off the main thread.
  , create :: Maybe Text -> Text -> Bool -> IO (Either Text Text)
    -- ^ In an organization or yours, its name, and whether only the
    -- default branch: the new fork's full name.
  , opened :: Text -> IO ()
    -- ^ Shows a fork, made now or before.
  }

-- | The dialog over the window @anchor@ is in.
open :: Gtk.Widget -> Dialog -> IO ()
open anchor d = do
  root <- Gtk.widgetGetRoot anchor
  parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
  dialog <- GI.new Gtk.Window [ #title := "Fork", #modal := True, #defaultWidth := 440
                              , #destroyWithParent := True, #cssClasses := ["usta", "u-fork"] ]
  Platform.dialogOver dialog parent
  header <- GI.new Gtk.HeaderBar []
  Platform.configureHeaderBar Platform.current header
  GI.new Gtk.Label [#label := "Fork " <> d.repo, #cssClasses := ["title"]] >>= \t -> GI.set header [#titleWidget := t]
  GI.set dialog [#titlebar := header]

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8, #cssClasses := ["form"]]
  forM_ d.reason $ \why -> GI.new Gtk.Label [#label := why, #xalign := 0, #wrap := True, #cssClasses := ["reason"]] >>= #append body
  caption "Owner" >>= #append body
  owners <- Gtk.dropDownNewFromStrings ["Loading…"]
  GI.set owners [#sensitive := False]
  #append body owners
  caption "Name" >>= #append body
  name <- GI.new Gtk.Entry [#text := T.takeWhileEnd (/= '/') d.repo, #cssClasses := ["mono"]]
  #append body name
  onlyRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #visible := d.branchOnly]
  only <- GI.new Gtk.Switch [#active := True, #valign := Gtk.AlignCenter]
  #append onlyRow only
  GI.new Gtk.Label [#label := "Only the default branch, " <> d.defaultBranch, #xalign := 0] >>= #append onlyRow
  #append body onlyRow
  -- A fork you have already, with a way to it.
  have <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #visible := False, #cssClasses := ["have"]]
  haveLabel <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #hexpand := True]
  openIt <- Button.new Button.defaults {Button.label = Just "Open it", Button.small = True}
  #append have haveLabel
  #append have openIt
  #append body have
  note <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #cssClasses := ["muted"], #visible := False]
  #append body note

  footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd, #cssClasses := ["footer"]]
  cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
  go <- Button.new Button.defaults {Button.label = Just "Create fork", Button.variant = Button.Primary}
  GI.set go [#sensitive := False]
  #append footer cancel
  #append footer go
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append content body
  #append content footer
  GI.set dialog [#child := content]

  places <- newIORef []
  existing <- newIORef Nothing
  let say text = GI.set note [#label := text, #visible := not (T.null text)]
      showOpen target = Gtk.windowDestroy dialog >> d.opened target
  void $ forkIO $ do
    got <- try d.options :: IO (Either SomeException (Either Text ([Text], [Text])))
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      case got of
        Right (Right (where', forks)) -> do
          writeIORef places where'
          model <- Gtk.stringListNew (Just [if i == 0 then o <> " (you)" else o | (i, o) <- zip [0 :: Int ..] where'])
          Gtk.dropDownSetModel owners (Just model)
          GI.set owners [#sensitive := length where' > 1]
          GI.set go [#sensitive := not (null where')]
          forM_ (take 1 forks) $ \f -> do
            writeIORef existing (Just f)
            GI.set haveLabel [#label := "You already have a fork: " <> f <> "."]
            GI.set have [#visible := True]
        Right (Left problem) -> say problem
        Left e -> say (T.pack (show e))
      pure False
  void $ GI.on openIt #clicked $ readIORef existing >>= mapM_ showOpen
  void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
  void $ GI.on go #clicked $ do
    i <- fromIntegral <$> Gtk.dropDownGetSelected owners
    where' <- readIORef places
    wanted <- T.strip <$> Gtk.editableGetText name
    branchOnly <- GI.get only #active
    when (not (T.null wanted)) $
      Button.busy go [cancel] "Forking…" (d.create (if i == 0 then Nothing else Just (where' !! i)) wanted (d.branchOnly && branchOnly)) $ \case
        Right made -> showOpen made
        Left problem -> say ("The forge did not fork it: " <> problem)
  void $ GI.on name #activate (void (Gtk.widgetActivate go))
  #present dialog
  where
    caption t = GI.new Gtk.Label [#label := t, #xalign := 0, #cssClasses := ["step"]]

css :: Text
css = mconcat
  [ "window.u-fork headerbar { background: var(--surface); border-bottom: 1px solid var(--border); box-shadow: none; }"
  , "window.u-fork box.form { padding: 18px 20px; background: var(--surface); }"
  , "window.u-fork label.step { font-size: 12px; color: var(--text-soft); margin-top: 4px; }"
  , "window.u-fork label.muted { color: var(--text-muted); font-size: 12px; }"
  , "window.u-fork label.reason { padding: 8px 10px; margin-bottom: 4px; border-radius: 6px; background: var(--accent-bg); }"
  , "window.u-fork box.have { padding: 8px 10px; margin-top: 6px; border-radius: 6px; background: var(--accent-bg); }"
  , "window.u-fork box.footer { padding: 12px 20px; background: var(--window); border-top: 1px solid var(--border); }"
  , "window.u-fork dropdown > button { min-height: 34px; border: 1px solid var(--input-border); border-radius: 6px; background: var(--surface); background-image: none; box-shadow: none; }"
  , "window.u-fork entry { min-height: 34px; border: 1px solid var(--input-border); border-radius: 6px; }"
  ]
