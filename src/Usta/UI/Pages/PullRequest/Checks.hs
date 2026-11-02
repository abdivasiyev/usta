{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Checks tab of a pull request: the check runs on the left and the
-- selected run's log on the right.
module Usta.UI.Pages.PullRequest.Checks
  ( Status (..)
  , LogLine (..)
  , Run (..)
  , Model (..)
  , passing
  , failing
  , new
  , css
  ) where

import Control.Monad (forM, forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Icon as Icon
import Usta.UI.Style (Tone (..))

type Status :: Type
data Status = Passed | Failed | Skipped
  deriving (Eq, Show, Enum, Bounded)

-- | One line of a run's output.
type LogLine :: Type
data LogLine
  = Step Text
    -- ^ The start of a step, e.g. "▸ nix flake check".
  | Output Text
  | Failure Text
    -- ^ A line that is part of the error.
  | Note Text
    -- ^ Quiet output, e.g. the exit code.

type Run :: Type
data Run = Run
  { name :: Text
  , status :: Status
  , duration :: Text
  , output :: [LogLine]
  }

type Model :: Type
data Model = Model
  { provider :: Text
    -- ^ Where the runs come from, e.g. "Forgejo Actions".
  , runs :: [Run]
  }

new :: Model -> IO Gtk.Widget
new m = do
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

  rows <- forM m.runs $ \run -> do
    button <- runRow run
    #append side button
    pure (run, button)

  let select run = do
        forM_ rows $ \(other, button) ->
          if other.name == run.name
            then Gtk.widgetAddCssClass button "selected"
            else Gtk.widgetRemoveCssClass button "selected"
        old <- Gtk.widgetGetFirstChild detail
        forM_ old $ Gtk.boxRemove detail
        runDetail run >>= #append detail
  forM_ rows $ \(run, button) -> void $ GI.on button #clicked (select run)

  -- Open on the first failure, since that is what needs attention.
  case filter ((== Failed) . (.status)) m.runs <> m.runs of
    run : _ -> select run
    [] -> pure ()
  Gtk.toWidget columns

runRow :: Run -> IO Gtk.Button
runRow run = do
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  statusIcon run >>= #append content
  title <- label run.name ["mono"]
  GI.set title [#hexpand := True]
  #append content title
  label (if run.status == Skipped then "Skipped" else run.duration) ["muted", "small"]
    >>= #append content
  GI.new Gtk.Button [ #child := content
                    , #hasFrame := False
                    , #cssClasses := ["run", statusClass run.status]
                    ]

statusIcon :: Run -> IO Gtk.Image
statusIcon run = case run.status of
  Passed -> Icon.toned 16 Success Icon.Check
  Failed -> Icon.toned 16 Danger Icon.Cross
  Skipped -> Icon.toned 16 Neutral Icon.Skipped

statusClass :: Status -> Text
statusClass = T.toLower . T.pack . show

runDetail :: Run -> IO Gtk.Box
runDetail run = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12, #vexpand := True]
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  title <- label run.name ["mono", "run-title"]
  GI.set title [#hexpand := True]
  #append header title
  summary <- label (summaryText run) ["small", statusClass run.status]
  #append header summary
  Button.new Button.defaults { Button.label = Just "Copy log" } >>= #append header
  Button.new Button.defaults { Button.label = Just "Re-run job", Button.variant = Button.Primary }
    >>= #append header
  #append box header
  logView run.output >>= #append box
  pure box

-- | "Failed on step 3 of 4", counted from the step markers in the log.
summaryText :: Run -> Text
summaryText run = case run.status of
  Passed -> "Passed in " <> run.duration
  Skipped -> "Skipped"
  Failed -> case findIndex isFailure run.output of
    Nothing -> "Failed"
    Just at ->
      let failedStep = length (filter isStep (take at run.output))
      in "Failed on step " <> T.pack (show failedStep) <> " of " <> T.pack (show totalSteps)
  where
    totalSteps = length (filter isStep run.output)
    isStep (Step _) = True
    isStep _ = False
    isFailure (Failure _) = True
    isFailure _ = False

-- | A dark, numbered log. Steps are highlighted, error lines marked.
--
-- Each step is a section that opens and closes from its header. Steps
-- that failed start open, the rest closed. Line numbers count every line,
-- so they stay the same whichever steps are open.
logView :: [LogLine] -> IO Gtk.ScrolledWindow
logView ls = do
  lines' <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-log"]]
  let (before, steps) = sections (zip [1 :: Int ..] ls)
  forM_ before $ \(n, l) -> logLine n l >>= #append lines'
  forM_ steps $ \((n, title), stepLines) -> do
    let failed = any (isFailure . snd) stepLines
    content <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                              , #visible := failed
                              ]
    forM_ stepLines $ \(m, l) -> logLine m l >>= #append content

    arrow <- label (if failed then "▾" else "▸") ["log-arrow"]
    header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    number n >>= #append header
    #append header arrow
    stepTitle <- label title ["log-text"]
    GI.set stepTitle [#hexpand := True]
    #append header stepTitle
    button <- GI.new Gtk.Button [ #child := header
                                , #hasFrame := False
                                , #cssClasses := ["log-step"] <> ["failed" | failed]
                                ]
    void $ GI.on button #clicked $ do
      open <- GI.get content #visible
      GI.set content [#visible := not open]
      GI.set arrow [#label := if open then "▸" else "▾"]
    #append lines' button
    #append lines' content
  GI.new Gtk.ScrolledWindow [ #child := lines'
                            , #vexpand := True
                            , #cssClasses := ["log-scroll"]
                            ]
  where
    logLine n l = do
      row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["log-line", kind l]]
      number n >>= #append row
      text <- label (body l) ["log-text"]
      GI.set text [#selectable := True, #hexpand := True]
      #append row text
      pure row
    number n = GI.new Gtk.Label [ #label := T.pack (show n)
                                , #xalign := 1
                                , #widthRequest := 48
                                , #cssClasses := ["log-number"]
                                ]
    isFailure (Failure _) = True
    isFailure _ = False
    kind l = case l of
      Step _ -> "step"
      Output _ -> "out"
      Failure _ -> "failure"
      Note _ -> "note"
    body l = case l of
      Step t -> t
      Output t -> t
      Failure t -> t
      Note t -> t

-- | Lines before the first step, then each step's numbered title with the
-- lines that belong to it.
sections :: [(Int, LogLine)] -> ([(Int, LogLine)], [((Int, Text), [(Int, LogLine)])])
sections numbered = (before, go rest)
  where
    (before, rest) = break (isStep . snd) numbered
    go ((n, Step title) : more) =
      let (body, next) = break (isStep . snd) more
      in ((n, title), body) : go next
    go _ = []
    isStep (Step _) = True
    isStep _ = False

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

-- | Runs for a pull request whose checks passed.
passing :: Model
passing = Model "Forgejo Actions"
  [ Run "nix build" Passed "3m 24s"
      [ Step "Checkout"
      , Output "  Fetched feat/gtk4-dep at e1c90fa"
      , Step "Install Nix"
      , Output "  nix (Nix) 2.31.2"
      , Step "nix build"
      , Output "  building '/nix/store/…-usta-0.1.0.0.drv'..."
      , Output "  usta> [1 of 2] Compiling Main"
      , Output "  usta> [2 of 2] Linking dist/build/usta/usta"
      , Note "  Process exited with code 0"
      ]
  ]

-- | Runs for a pull request whose checks failed.
failing :: Model
failing = Model "Forgejo Actions"
  [ Run "nix flake check" Failed "48s"
      [ Step "Checkout"
      , Output "  Fetched cache-priorities at 3f2a9d1"
      , Step "Install Nix"
      , Output "  nix (Nix) 2.31.2"
      , Step "nix flake check"
      , Output "  evaluating flake..."
      , Output "  checking NixOS configuration 'homelab'..."
      , Failure "  error:"
      , Failure "         … while evaluating the option `nix.settings.substituters'"
      , Failure "         error: A definition for option `nix.settings.substituters'"
      , Failure "         is not of type `list of string'."
      , Failure "         Definition value: \"https://cache.nixos.org?priority=1\""
      , Failure "         defined in `modules/cache.nix', line 7"
      , Step "Upload results"
      , Note "  Process exited with code 1"
      ]
  , Run "nixfmt" Passed "6s"
      [ Step "nixfmt --check ."
      , Output "  All files are formatted."
      , Note "  Process exited with code 0"
      ]
  , Run "deploy (dry run)" Skipped "" []
  ]

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
  -- The log stays dark in both themes, like a terminal.
  , "box.u-checks scrolledwindow.log-scroll { border-radius: 10px; background: #16181C; }"
  , "box.u-log { padding: 12px 0; background: #16181C; }"
  , "box.u-log box.log-line { font-family: 'JetBrains Mono', monospace; font-size: 13px; color: #D7DAE0; }"
  , "box.u-log label.log-number { padding-right: 14px; color: #6B717B; }"
  , "box.u-log label.log-text { padding: 1px 16px 1px 0; }"
  , "box.u-log box.log-line.step { color: #8AB4F8; }"
  , "box.u-log box.log-line.failure { background: #3A1D1D; color: #F6B4AC; }"
  , "box.u-log box.log-line.note { color: #9AA0A9; }"
  , "box.u-log button.log-step {"
  , "  min-height: 0; padding: 0; border: none; border-radius: 0; box-shadow: none;"
  , "  background: transparent; background-image: none;"
  , "  font-family: 'JetBrains Mono', monospace; font-size: 13px; font-weight: 400; color: #8AB4F8;"
  , "}"
  , "box.u-log button.log-step:hover { background: #22252B; }"
  , "box.u-log button.log-step.failed { color: #F6B4AC; }"
  , "box.u-log label.log-arrow { min-width: 16px; }"
  ]
