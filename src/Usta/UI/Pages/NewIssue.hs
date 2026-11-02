{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Filing an issue, in a modal window over the app.
module Usta.UI.Pages.NewIssue
  ( Model (..)
  , sample
  , open
  , css
  ) where

import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import qualified Usta.UI.Platform as Platform

type Model :: Type
data Model = Model
  { repositories :: [Text]
  , assignee :: Text
  }

sample :: Model
sample = Model
  { repositories = ["github.com/abdivasiyev/usta", "git.azizovich.uz/abdivasiyev/homelab"]
  , assignee = "abdivasiyev"
  }

-- | Opens the dialog over the window that @anchor@ is in.
open :: Gtk.Widget -> Model -> IO ()
open anchor m = do
  root <- Gtk.widgetGetRoot anchor
  parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root

  dialog <- GI.new Gtk.Window [ #title := "New issue"
                              , #modal := True
                              , #defaultWidth := 720
                              , #defaultHeight := 600
                              , #destroyWithParent := True
                              , #cssClasses := ["usta", "u-new-issue"]
                              ]
  forM_ parent $ \p -> Gtk.windowSetTransientFor dialog (Just p)

  -- Window controls follow the OS, like the main window.
  header <- GI.new Gtk.HeaderBar []
  Platform.configureHeaderBar Platform.current header
  GI.set dialog [#titlebar := header]

  body <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 16
                         , #vexpand := True
                         , #cssClasses := ["form"]
                         ]
  repo <- Gtk.dropDownNewFromStrings m.repositories
  Gtk.toWidget repo >>= field "Repository" >>= #append body
  title <- GI.new Gtk.Entry [#placeholderText := "Short summary of the problem or idea"]
  Gtk.toWidget title >>= field "Title" >>= #append body
  (editor, _) <- MarkdownEditor.new MarkdownEditor.defaults
    { MarkdownEditor.hint = Just "Markdown supported; paste images to attach them."
    , MarkdownEditor.height = 160
    }
  #append body editor
  pair <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                         , #spacing := 12
                         , #homogeneous := True
                         ]
  labelsEntry <- GI.new Gtk.Entry [#placeholderText := "Add labels"]
  Gtk.toWidget labelsEntry >>= field "Labels" >>= #append pair
  assigneeEntry <- GI.new Gtk.Entry [#text := m.assignee]
  Gtk.toWidget assigneeEntry >>= field "Assignee" >>= #append pair
  #append body pair

  footer <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 8
                           , #cssClasses := ["footer"]
                           ]
  note <- GI.new Gtk.Label [ #label := "Saved as a draft if you go offline"
                           , #xalign := 0
                           , #hexpand := True
                           , #cssClasses := ["note"]
                           ]
  #append footer note
  cancel <- Button.new Button.defaults { Button.label = Just "Cancel" }
  create <- Button.new Button.defaults { Button.label = Just "Create issue", Button.variant = Button.Primary }
  -- Creating closes the dialog for now; filing it needs the forge client.
  forM_ [cancel, create] $ \b -> void $ GI.on b #clicked (Gtk.windowDestroy dialog)
  #append footer cancel
  #append footer create

  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append content body
  #append content footer
  GI.set dialog [#child := content]
  #present dialog
  Gtk.widgetGrabFocus title >>= \_ -> pure ()

field :: Text -> Gtk.Widget -> IO Gtk.Box
field caption input = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  GI.new Gtk.Label [#label := caption, #xalign := 0, #cssClasses := ["caption"]] >>= #append box
  #append box input
  pure box

css :: Text
css = mconcat
  [ "window.u-new-issue headerbar {"
  , "  background: var(--surface); border-bottom: 1px solid var(--border); box-shadow: none;"
  , "}"
  , "window.u-new-issue box.form { padding: 20px; }"
  , "window.u-new-issue label.caption { font-size: 13px; color: var(--text-soft); }"
  , "window.u-new-issue entry {"
  , "  min-height: 38px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none;"
  , "}"
  , "window.u-new-issue dropdown > button {"
  , "  min-height: 40px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
  , "  font-family: 'JetBrains Mono', monospace; font-size: 13px;"
  , "}"
  , "window.u-new-issue box.footer {"
  , "  padding: 14px 20px; background: var(--window); border-top: 1px solid var(--border);"
  , "}"
  , "window.u-new-issue label.note { font-size: 12px; color: var(--text-muted); }"
  ]
