{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Checks tab of a pull request: the checks on its latest commit on
-- the left and the selected one's log on the right, read from the forge
-- when it is selected.
module Usta.UI.Pages.PullRequest.Checks
  ( LogLine (..)
  , Model (..)
  , parseLog
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void)
import Data.Char (isDigit)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Icon as Icon
import Usta.UI.Style (Tone (..))
import Usta.Types.Check (Check (..), Job, State (..))

-- | One line of a job's log.
type LogLine :: Type
data LogLine
  = Step Text
    -- ^ The start of a step, e.g. "nix flake check".
  | Output Text
  | Failure Text
    -- ^ A line that is part of the error.
  | Note Text
    -- ^ The runner's own lines, e.g. "Job failed".
  deriving stock (Eq, Show)

type Model :: Type
data Model = Model
  { provider :: Text
    -- ^ Where the checks come from, e.g. "Forgejo Actions".
  , checks :: [Check]
  , logOf :: Job -> IO (Either Text Text)
  }

new :: Model -> IO Gtk.Widget
new m = case m.checks of
  [] -> do
    w <- label "No checks ran on the latest commit." ["muted"]
    GI.set w [#marginTop := 24, #marginStart := 24]
    Gtk.toWidget w
  _ -> do
    columns <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                              , #cssClasses := ["u-checks"]
                              ]
    side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #widthRequest := 280
                           , #hexpand := False
                           , #cssClasses := ["runs"]
                           ]
    heading <- Card.sectionTitle m.provider
    Gtk.widgetAddCssClass heading "provider"
    #append side heading
    #append columns side

    detail <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #hexpand := True
                             , #cssClasses := ["run-detail"]
                             ]
    #append columns detail

    rows <- forM m.checks $ \check -> do
      button <- checkRow check
      #append side button
      pure (check, button)

    let select check = do
          forM_ rows $ \(other, button) ->
            if other.name == check.name
              then Gtk.widgetAddCssClass button "selected"
              else Gtk.widgetRemoveCssClass button "selected"
          old <- Gtk.widgetGetFirstChild detail
          forM_ old $ Gtk.boxRemove detail
          checkDetail m check >>= #append detail
    forM_ rows $ \(check, button) -> void $ GI.on button #clicked (select check)

    -- Open on the first failure, since that is what needs attention.
    case filter ((== Failed) . (.state)) m.checks <> m.checks of
      check : _ -> select check
      [] -> pure ()
    Gtk.toWidget columns

checkRow :: Check -> IO Gtk.Button
checkRow check = do
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  stateIcon check.state >>= #append content
  title <- label check.name ["mono"]
  GI.set title [#hexpand := True, #ellipsize := Pango.EllipsizeModeEnd]
  #append content title
  label (shortState check) ["muted", "small"] >>= #append content
  GI.new Gtk.Button [ #child := content
                    , #hasFrame := False
                    , #tooltipText := check.description
                    , #cssClasses := ["run", stateClass check.state]
                    ]

-- | What a row says on its right: how long a finished check took, as its
-- description ends ("Successful in 48s"), or where it is.
shortState :: Check -> Text
shortState check = case check.state of
  Skipped -> "Skipped"
  Waiting -> "Waiting"
  Running -> "Running"
  _ -> case reverse (T.words check.description) of
    d : _ | T.any isDigit (T.take 1 d), T.all (\c -> isDigit c || c `elem` ("hms" :: String)) d -> d
    _ -> ""

stateIcon :: State -> IO Gtk.Image
stateIcon state = case state of
  Passed -> Icon.toned 16 Success Icon.Check
  Failed -> Icon.toned 16 Danger Icon.Cross
  Skipped -> Icon.toned 16 Neutral Icon.Skipped
  Waiting -> Icon.toned 16 Neutral Icon.Skipped
  Running -> Icon.toned 16 Warning Icon.Skipped

stateClass :: State -> Text
stateClass = T.toLower . T.pack . show

checkDetail :: Model -> Check -> IO Gtk.Box
checkDetail m check = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12, #vexpand := True]
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  title <- label check.name ["mono", "run-title"]
  GI.set title [#hexpand := True]
  #append header title
  summary <- label check.description ["small", stateClass check.state]
  #append header summary
  copy <- Button.new Button.defaults { Button.label = Just "Copy log" }
  GI.set copy [#visible := False]
  #append header copy
  forM_ check.url $ \url -> do
    open <- Button.new Button.defaults { Button.label = Just "Open in browser", Button.variant = Button.Primary }
    GI.set open [#tooltipText := "Its page on the forge, where it can be run again"]
    void $ GI.on open #clicked (openUri url)
    #append header open
  #append box header

  let say text = label text ["muted"] >>= \l -> GI.set l [#wrap := True] >> #append box l
  case (check.state, check.job) of
    (Skipped, _) -> say "This job was skipped, so it has no log."
    (Waiting, _) -> say "This job has not started yet."
    (_, Nothing) -> say "This check runs outside the forge; its details are on its own page."
    (_, Just job) -> do
      waiting <- label "Loading the log…" ["muted"]
      #append box waiting
      void $ forkIO $ do
        got <- try (m.logOf job)
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          Gtk.boxRemove box waiting
          case got of
            Right (Right raw) -> do
              let ls = parseLog (check.state == Failed) raw
              logView ls >>= #append box
              GI.set copy [#visible := True]
              void $ GI.on copy #clicked $ do
                clipboard <- Gtk.widgetGetClipboard copy
                provider <- GI.toGValue (Just (T.unlines (map body ls))) >>= Gdk.contentProviderNewForValue
                void (Gdk.clipboardSetContent clipboard (Just provider))
            Right (Left _) -> say unreadable
            Left (_ :: SomeException) -> say unreadable
          pure False
  pure box
  where
    unreadable = "usta could not read this job's log. Forgejo shows the logs of a private repository's jobs only in the browser; Open in browser shows it there."

openUri :: Text -> IO ()
openUri url = do
  launcher <- Gtk.uriLauncherNew (Just url)
  Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing

-- | A job's log as Forgejo gives it: each line after its time, steps as
-- "⭐ Run Main <name>", and the runner's own lines. In a failed job, lines
-- that speak of an error are marked.
parseLog :: Bool -> Text -> [LogLine]
parseLog failed raw = map (classify . untimed) (T.lines raw)
  where
    untimed line = case T.breakOn " " line of
      (t, rest) | isTime t -> T.drop 1 rest
      _ -> line
    isTime t = T.length t >= 20 && "Z" `T.isSuffixOf` t && T.all (\c -> isDigit c || c `elem` ("-:.TZ" :: String)) t
    classify l
      | Just step <- T.stripPrefix "⭐ Run " l = Step (dropPhase step)
      | any (`T.isPrefixOf` l) ["🏁", "Job '"] || "received task" `T.isInfixOf` l || l == "workflow prepared" = Note l
      | failed && any (`T.isInfixOf` T.toLower l) ["error", "fail", "fatal", "panic", "exception"] = Failure l
      | otherwise = Output l
    dropPhase step = case T.breakOn " " step of
      (phase, rest) | phase `elem` ["Main", "Pre", "Post"] -> T.drop 1 rest
      _ -> step

body :: LogLine -> Text
body l = case l of
  Step t -> t
  Output t -> t
  Failure t -> t
  Note t -> t

-- | A dark, numbered log, steps in their own color and error lines
-- marked. Forgejo marks only some steps in a log, so none fold away.
logView :: [LogLine] -> IO Gtk.ScrolledWindow
logView ls = do
  lines' <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-log"]]
  forM_ (zip [1 :: Int ..] ls) $ \(n, l) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["log-line", kind l]]
    GI.new Gtk.Label [ #label := T.pack (show n), #xalign := 1, #widthRequest := 48
                     , #cssClasses := ["log-number"] ] >>= #append row
    text <- label (marked l) ["log-text"]
    GI.set text [#selectable := True, #hexpand := True]
    #append row text
    #append lines' row
  GI.new Gtk.ScrolledWindow [ #child := lines'
                            , #vexpand := True
                            , #cssClasses := ["log-scroll"]
                            ]
  where
    kind l = case l of
      Step _ -> "step"
      Output _ -> "out"
      Failure _ -> "failure"
      Note _ -> "note"
    marked l = case l of
      Step t -> "▸ " <> t
      _ -> body l

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-checks label.small { font-size: 12px; }"
  , "box.u-checks box.runs { background: var(--surface); border-right: 1px solid var(--border); }"
  , "box.u-checks label.provider { margin: 12px 16px; }"
  , "box.u-checks button.run {"
  , "  min-height: 48px; padding: 0 16px; border-radius: 0; border: none; box-shadow: none;"
  , "  background: transparent; background-image: none; color: var(--text); font-weight: 400;"
  , "}"
  , "box.u-checks button.run:hover { background: var(--window); }"
  , "box.u-checks button.run.skipped { color: var(--text-muted); }"
  , "box.u-checks button.run.selected { background: var(--selected); box-shadow: inset 3px 0 0 var(--accent); }"
  , "box.u-checks button.run.failed.selected {"
  , "  background: var(--danger-bg); box-shadow: inset 3px 0 0 var(--danger);"
  , "}"
  , "box.u-checks box.run-detail { padding: 16px; }"
  , "box.u-checks label.run-title { font-size: 16px; font-weight: 600; }"
  , "box.u-checks label.failed { color: var(--danger); font-weight: 500; }"
  , "box.u-checks label.passed { color: var(--success); font-weight: 500; }"
  , "box.u-checks label.running { color: var(--warning); font-weight: 500; }"
  , "box.u-checks button.run.waiting { color: var(--text-muted); }"
  -- The log stays dark in both themes, like a terminal.
  , "box.u-checks scrolledwindow.log-scroll { border-radius: 10px; background: #16181C; }"
  , "box.u-log { padding: 12px 0; background: #16181C; }"
  , "box.u-log box.log-line { font-family: 'JetBrains Mono', monospace; font-size: 13px; color: #D7DAE0; }"
  , "box.u-log label.log-number { padding-right: 14px; color: #6B717B; }"
  , "box.u-log label.log-text { padding: 1px 16px 1px 0; }"
  , "box.u-log box.log-line.step { color: #8AB4F8; }"
  , "box.u-log box.log-line.failure { background: #3A1D1D; color: #F6B4AC; }"
  , "box.u-log box.log-line.note { color: #9AA0A9; }"
  ]
