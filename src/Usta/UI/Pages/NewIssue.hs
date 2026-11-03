{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Filing an issue, in a modal window over the app.
module Usta.UI.Pages.NewIssue
  ( Model (..)
  , Place (..)
  , sample
  , open
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Platform as Platform
import Usta.Types.Discussion (NewIssue (..))
import Usta.Types.Parse (parse)

-- | A repository an issue can go to.
type Place :: Type
data Place = Place
  { forge :: Forge.Kind
  , host :: Text
  , repo :: Text
  , login :: Text
    -- ^ The account's own login there, the assignee to start with.
  }

type Model :: Type
data Model = Model
  { places :: [Place]
  , chosen :: Int
    -- ^ The one selected at first.
  }

sample :: Model
sample = Model
  { places =
      [ Place Forge.GitHub "github.com" "abdivasiyev/usta" "abdivasiyev"
      , Place Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" "abdivasiyev"
      ]
  , chosen = 0
  }

-- | Opens the dialog over the window that @anchor@ is in. @create@ files
-- the issue, off the main thread; @opened@ runs with its number once it
-- is filed, and the dialog closes.
open :: Gtk.Widget -> (Place -> NewIssue -> IO (Either Text Int)) -> (Place -> Int -> Text -> IO ()) -> Model -> IO ()
open anchor create opened m = do
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
  repo <- Gtk.dropDownNewFromStrings [p.host <> "/" <> p.repo | p <- m.places]
  Gtk.dropDownSetSelected repo (fromIntegral m.chosen)
  Gtk.toWidget repo >>= field "Repository" >>= #append body
  title <- GI.new Gtk.Entry [#placeholderText := "Short summary of the problem or idea"]
  Gtk.toWidget title >>= field "Title" >>= #append body
  (editor, buffer) <- MarkdownEditor.new MarkdownEditor.defaults
    { MarkdownEditor.hint = Just "Markdown supported; paste images to attach them."
    , MarkdownEditor.height = 160
    }
  #append body editor
  pair <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                         , #spacing := 12
                         , #homogeneous := True
                         ]
  labelsEntry <- GI.new Gtk.Entry [#placeholderText := "Add labels, separated by commas"]
  Gtk.toWidget labelsEntry >>= field "Labels" >>= #append pair
  let placeAt i = if i < length m.places then Just (m.places !! i) else Nothing
  assigneeEntry <- GI.new Gtk.Entry [#text := maybe "" (.login) (placeAt m.chosen)]
  Gtk.toWidget assigneeEntry >>= field "Assignee" >>= #append pair
  #append body pair

  footer <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 8
                           , #cssClasses := ["footer"]
                           ]
  -- Says what is missing, or why the forge refused.
  note <- GI.new Gtk.Label [ #xalign := 0
                           , #hexpand := True
                           , #wrap := True
                           , #cssClasses := ["note", "problem"]
                           ]
  #append footer note
  cancel <- Button.new Button.defaults { Button.label = Just "Cancel" }
  void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
  createButton <- Button.new Button.defaults { Button.label = Just "Create issue", Button.variant = Button.Primary }
  void $ GI.on createButton #clicked $ do
    index <- fromIntegral <$> Gtk.dropDownGetSelected repo
    titleText <- T.strip <$> Gtk.editableGetText title
    bodyText <- maybe "" id <$> GI.get buffer #text
    labelsText <- Gtk.editableGetText labelsEntry
    assigneeText <- T.strip <$> Gtk.editableGetText assigneeEntry
    let assignees = if T.null assigneeText then Right [] else (: []) <$> parse assigneeText
    case (placeAt index, assignees) of
      _ | T.null titleText -> GI.set note [#label := "Give the issue a title."]
      (Nothing, _) -> GI.set note [#label := "Choose a repository."]
      (_, Left _) -> GI.set note [#label := "The assignee is not a user name."]
      (Just place, Right who) -> do
        GI.set note [#label := ""]
        GI.set createButton [#sensitive := False]
        let new = NewIssue
              { title = titleText
              , body = bodyText
              , labels = filter (not . T.null) (map T.strip (T.splitOn "," labelsText))
              , assignees = who
              }
        void $ forkIO $ do
          result <- create place new
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            case result of
              Right number -> Gtk.windowDestroy dialog >> opened place number titleText
              Left problem -> do
                GI.set note [#label := problem]
                GI.set createButton [#sensitive := True]
            pure False
  #append footer cancel
  #append footer createButton

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
  , "window.u-new-issue label.note.problem { color: var(--danger); }"
  ]
