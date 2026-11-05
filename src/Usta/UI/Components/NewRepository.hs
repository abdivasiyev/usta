{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | New repository: on which forge, whose (you or an organization), its
-- name and description, public or private, and whether it starts with a
-- README.
module Usta.UI.Components.NewRepository
  ( Dialog (..)
  , open
  , nameOf
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Data.Char (isAscii, isAlphaNum)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Platform as Platform

type Dialog :: Type
data Dialog = Dialog
  { forges :: [Text]
    -- ^ The hosts signed in to.
  , owners :: Text -> IO (Either Text [Text])
    -- ^ Where it can go on a forge, you first; off the main thread.
  , create :: Text -> Maybe Text -> Text -> Text -> Bool -> Bool -> IO (Either Text Text)
    -- ^ On a host, in an organization or yours: name, description,
    -- private, with a README; the new repository's full name.
  , opened :: Text -> Text -> IO ()
    -- ^ Shows it, by host and full name.
  }

-- | A name forges take: letters, digits, "-", "_" and ".", not "." or "..".
nameOf :: Text -> Either Text Text
nameOf raw
  | T.null name = Left "Give it a name."
  | name `elem` [".", ".."] = Left "That name is taken by git."
  | T.all allowed name = Right name
  | otherwise = Left "A name has only letters, digits, \"-\", \"_\" and \".\"."
  where
    name = T.strip raw
    allowed c = isAscii c && (isAlphaNum c || c `elem` ("-_." :: String))

-- | The dialog over the window @anchor@ is in.
open :: Gtk.Widget -> Dialog -> IO ()
open anchor d = do
  root <- Gtk.widgetGetRoot anchor
  parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
  dialog <- GI.new Gtk.Window [ #title := "New repository", #modal := True, #defaultWidth := 460
                              , #destroyWithParent := True, #cssClasses := ["usta", "u-new-repo"] ]
  forM_ parent $ \p -> Gtk.windowSetTransientFor dialog (Just p)
  header <- GI.new Gtk.HeaderBar []
  Platform.configureHeaderBar Platform.current header
  GI.new Gtk.Label [#label := "New repository", #cssClasses := ["title"]] >>= \t -> GI.set header [#titleWidget := t]
  GI.set dialog [#titlebar := header]

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8, #cssClasses := ["form"]]
  -- The forge shows only when there is a choice.
  forgeCaption <- caption "Forge"
  forgePick <- Gtk.dropDownNewFromStrings d.forges
  forM_ [Gtk.toWidget forgeCaption, Gtk.toWidget forgePick] $ \w -> w >>= \w' -> GI.set w' [#visible := length d.forges > 1] >> #append body w'
  caption "Owner" >>= #append body
  owners <- Gtk.dropDownNewFromStrings ["Loading…"]
  GI.set owners [#sensitive := False]
  #append body owners
  caption "Name" >>= #append body
  name <- GI.new Gtk.Entry [#placeholderText := "dotfiles", #cssClasses := ["mono"]]
  #append body name
  caption "Description · optional" >>= #append body
  description <- GI.new Gtk.Entry []
  #append body description
  caption "Visibility" >>= #append body
  (visibility, choices) <- Segmented.new ["Public", "Private"] 1
  GI.set visibility [#halign := Gtk.AlignStart]
  #append body visibility
  readmeRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["switch-row"]]
  readme <- GI.new Gtk.Switch [#active := True, #valign := Gtk.AlignCenter]
  #append readmeRow readme
  GI.new Gtk.Label [#label := "Start with a README, on main", #xalign := 0] >>= #append readmeRow
  #append body readmeRow
  hint <- GI.new Gtk.Label [#label := "Without it the repository is empty: push to it to start.", #xalign := 0, #wrap := True, #cssClasses := ["muted"], #visible := False]
  #append body hint
  note <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #cssClasses := ["problem"], #visible := False]
  #append body note

  footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd, #cssClasses := ["footer"]]
  cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
  go <- Button.new Button.defaults {Button.label = Just "Create repository", Button.variant = Button.Primary}
  GI.set go [#sensitive := False]
  #append footer cancel
  #append footer go
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append content body
  #append content footer
  GI.set dialog [#child := content]

  places <- newIORef []
  asked <- newIORef (0 :: Int)
  let say text = GI.set note [#label := text, #visible := not (T.null text)]
      hostChosen = do
        i <- fromIntegral <$> Gtk.dropDownGetSelected forgePick
        pure (if i < length d.forges then Just (d.forges !! i) else Nothing)
      -- The owners of the forge chosen; only the latest answer shows.
      loadOwners = hostChosen >>= mapM_ (\host -> do
        turn <- (+ 1) <$> readIORef asked
        writeIORef asked turn
        GI.set owners [#sensitive := False]
        GI.set go [#sensitive := False]
        void $ forkIO $ do
          got <- try (d.owners host) :: IO (Either SomeException (Either Text [Text]))
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            latest <- readIORef asked
            if latest /= turn then pure False else do
              case got of
                Right (Right where') -> do
                  writeIORef places where'
                  model <- Gtk.stringListNew (Just [if i == 0 then o <> " (you)" else o | (i, o) <- zip [0 :: Int ..] where'])
                  Gtk.dropDownSetModel owners (Just model)
                  GI.set owners [#sensitive := length where' > 1]
                  GI.set go [#sensitive := not (null where')]
                  say ""
                Right (Left problem) -> say problem
                Left e -> say (T.pack (show e))
              pure False)
  loadOwners
  void $ GI.on forgePick (GI.PropertyNotify #selected) (const loadOwners)
  void $ GI.on readme (GI.PropertyNotify #active) $ \_ -> GI.get readme #active >>= \on -> GI.set hint [#visible := not on]
  -- The name is checked as it is typed.
  void $ GI.on name #changed $ do
    typed <- Gtk.editableGetText name
    say (if T.null (T.strip typed) then "" else either id (const "") (nameOf typed))
  void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
  void $ GI.on go #clicked $ do
    host <- hostChosen
    i <- fromIntegral <$> Gtk.dropDownGetSelected owners
    where' <- readIORef places
    typed <- Gtk.editableGetText name
    about <- T.strip <$> Gtk.editableGetText description
    private <- case drop 1 choices of
      b : _ -> GI.get b #active
      [] -> pure True
    withReadme <- GI.get readme #active
    case (host, nameOf typed) of
      (_, Left problem) -> say problem
      (Nothing, _) -> say "Choose a forge."
      (Just h, Right n) ->
        Button.busy go [cancel] "Creating…" (d.create h (if i == 0 || i >= length where' then Nothing else Just (where' !! i)) n about private withReadme) $ \case
          Right made -> Gtk.windowDestroy dialog >> d.opened h made
          Left problem -> say ("The forge did not create it: " <> problem)
  void $ GI.on name #activate (void (Gtk.widgetActivate go))
  #present dialog
  where
    caption t = GI.new Gtk.Label [#label := t, #xalign := 0, #cssClasses := ["step"]]

css :: Text
css = mconcat
  [ "window.u-new-repo headerbar { background: var(--surface); border-bottom: 1px solid var(--border); box-shadow: none; }"
  , "window.u-new-repo box.form { padding: 18px 20px; background: var(--surface); }"
  , "window.u-new-repo label.step { font-size: 12px; color: var(--text-soft); margin-top: 4px; }"
  , "window.u-new-repo label.muted { color: var(--text-muted); font-size: 12px; }"
  , "window.u-new-repo label.problem { color: var(--danger); font-size: 12px; }"
  , "window.u-new-repo box.switch-row { margin-top: 8px; }"
  , "window.u-new-repo box.footer { padding: 12px 20px; background: var(--window); border-top: 1px solid var(--border); }"
  , "window.u-new-repo dropdown > button { min-height: 34px; border: 1px solid var(--input-border); border-radius: 6px; background: var(--surface); background-image: none; box-shadow: none; }"
  , "window.u-new-repo entry { min-height: 34px; border: 1px solid var(--input-border); border-radius: 6px; }"
  ]
