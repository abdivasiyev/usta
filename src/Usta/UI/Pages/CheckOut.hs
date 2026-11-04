{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Check out locally: a pull request's branch in a worktree of the local
-- mirror, or in a separate clone, at a folder the user chooses, on a
-- local branch that tracks the pull request's. It shares the mirror
-- dialog's look.
module Usta.UI.Pages.CheckOut
  ( Request (..)
  , open
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Platform as Platform

type Request :: Type
data Request = Request
  { repo :: Text
    -- ^ e.g. "abdivasiyev/usta".
  , number :: Int
  , branch :: Text
    -- ^ The pull request's branch.
  , mirrored :: Bool
    -- ^ A worktree needs the mirror.
  , worktree :: FilePath -> Text -> IO (Either Text ())
    -- ^ At a folder, on a local branch; off the main thread.
  , clone :: FilePath -> Text -> IO (Either Text ())
  }

open :: Gtk.Widget -> Request -> IO ()
open anchor q = do
  root <- Gtk.widgetGetRoot anchor
  parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
  dialog <- GI.new Gtk.Window [ #title := "Check out locally", #modal := True, #defaultWidth := 600
                              , #resizable := False, #destroyWithParent := True, #cssClasses := ["usta", "u-mirror"] ]
  forM_ parent $ \p -> Gtk.windowSetTransientFor dialog (Just p)
  header <- GI.new Gtk.HeaderBar []
  Platform.configureHeaderBar Platform.current header
  titles <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #valign := Gtk.AlignCenter]
  label "Check out locally" ["title"] >>= #append titles
  label (q.branch <> " from " <> q.repo <> " #" <> T.pack (show q.number)) ["subtitle", "mono"] >>= #append titles
  GI.set header [#titleWidget := titles]
  GI.set dialog [#titlebar := header]

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #cssClasses := ["form"]]
  label "How" ["step"] >>= #append body
  worktreeCard <- optionCard "A worktree of the mirror" "Fast and small: shares the local mirror's objects. The folder gets its own branch; delete it when done."
  cloneCard <- optionCard "A separate clone" "A full copy with its own history, for working on it apart from usta."
  Gtk.checkButtonSetGroup cloneCard (Just worktreeCard)
  GI.set worktreeCard [#active := q.mirrored, #sensitive := q.mirrored]
  GI.set cloneCard [#active := not q.mirrored]
  mapM_ (#append body) [worktreeCard, cloneCard]
  unless' q.mirrored $ label "The repository is not mirrored, so only a clone is offered." ["muted"] >>= #append body

  home <- getHomeDirectory
  let short = T.takeWhileEnd (/= '/') q.repo
      suggested = home </> "Development" </> T.unpack (short <> "-" <> T.map (\c -> if c == '/' then '-' else c) q.branch)
  label "Where" ["step"] >>= #append body
  whereRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  folder <- GI.new Gtk.Entry [#text := T.pack suggested, #hexpand := True]
  choose <- Button.new Button.defaults {Button.label = Just "Choose…"}
  mapM_ (#append whereRow) =<< sequence [Gtk.toWidget folder, Gtk.toWidget choose]
  #append body whereRow
  label "Local branch" ["step"] >>= #append body
  local <- GI.new Gtk.Entry [#text := q.branch]
  #append body local
  label "Tracks the pull request's branch, so git push updates it." ["muted"] >>= #append body
  label "Then" ["step"] >>= #append body
  showFolder <- GI.new Gtk.CheckButton [#label := "Show the folder", #active := True]
  copyCd <- GI.new Gtk.CheckButton [#label := "Copy the cd command"]
  mapM_ (#append body) [showFolder, copyCd]
  problem <- label "" ["problem-text"]
  GI.set problem [#visible := False, #wrap := True]
  #append body problem

  footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignEnd, #cssClasses := ["footer"]]
  cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
  go <- Button.new Button.defaults {Button.label = Just "Check out", Button.variant = Button.Primary}
  mapM_ (#append footer) [cancel, go]
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  mapM_ (#append content) [body, footer]
  GI.set dialog [#child := content]

  void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
  void $ GI.on choose #clicked $ do
    chooser <- GI.new Gtk.FileDialog [#title := "Where to check it out", #modal := True]
    Gtk.fileDialogSelectFolder chooser (Just dialog) (Nothing :: Maybe Gio.Cancellable) $ Just $ \_ result -> do
      picked <- try (Gtk.fileDialogSelectFolderFinish chooser result)
      case picked of
        Right file -> Gio.fileGetPath file >>= mapM_ (\p -> Gtk.editableSetText folder (T.pack (p </> T.unpack (T.map (\c -> if c == '/' then '-' else c) q.branch))))
        Left (_ :: SomeException) -> pure ()
  void $ GI.on go #clicked $ do
    dir <- T.unpack . T.strip <$> Gtk.editableGetText folder
    name <- T.strip <$> Gtk.editableGetText local
    asWorktree <- GI.get worktreeCard #active
    GI.set problem [#visible := False]
    Button.busy go [cancel] "Checking out…" ((if asWorktree then q.worktree else q.clone) dir name) $ \case
      Left why -> GI.set problem [#label := why, #visible := True]
      Right () -> do
        copying <- GI.get copyCd #active
        when copying $ do
          clipboard <- Gtk.widgetGetClipboard go
          provider <- GI.toGValue (Just ("cd " <> T.pack dir)) >>= Gdk.contentProviderNewForValue
          void (Gdk.clipboardSetContent clipboard (Just provider))
        showing <- GI.get showFolder #active
        when showing $ do
          launcher <- Gtk.fileLauncherNew . Just =<< Gio.fileNewForPath dir
          Gtk.fileLauncherLaunch launcher parent (Nothing :: Maybe Gio.Cancellable) Nothing
        Gtk.windowDestroy dialog
  #present dialog
  where
    unless' c act = if c then pure () else act

optionCard :: Text -> Text -> IO Gtk.CheckButton
optionCard title text = do
  texts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  GI.new Gtk.Label [#label := title, #xalign := 0, #cssClasses := ["name"]] >>= #append texts
  GI.new Gtk.Label [#label := text, #xalign := 0, #wrap := True, #cssClasses := ["reach"]] >>= #append texts
  GI.new Gtk.CheckButton [#child := texts, #cssClasses := ["option-card"]]

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]
