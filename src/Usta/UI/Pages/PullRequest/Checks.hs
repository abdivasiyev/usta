{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The Checks tab of a pull request: the checks on its latest commit on
the left and the selected one's log on the right, read from the forge
when it is selected.
-}
module Usta.UI.Pages.PullRequest.Checks
    ( LogLine (..)
    , Model (..)
    , parseLog
    , ansiPieces
    , ansiMarkup
    , logView
    , new
    , css
    )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void, when)
import Data.Char (isDigit)
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import Text.Read (readMaybe)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.Gdk qualified as Gdk
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Usta.Types.Check (Check (..), Job, State (..))
import Usta.UI.Style (Tone (..))

import Usta.UI.Browser qualified as Browser
import Usta.UI.Components.Button qualified as Button
import Usta.UI.Components.Card qualified as Card
import Usta.UI.Components.Icon qualified as Icon
import Usta.UI.Components.Skeleton qualified as Skeleton

-- | One line of a job's log.
type LogLine :: Type
data LogLine
    = -- | The start of a step, e.g. "nix flake check".
      Step Text
    | Output Text
    | -- | A line that is part of the error.
      Failure Text
    | -- | The runner's own lines, e.g. "Job failed".
      Note Text
    | -- | A command a step runs, as GitHub shows it before its output.
      Command Text
    | -- | GitHub's "##[group]": the lines up to 'EndGroup' fold under it.
      Group Text
    | EndGroup
    deriving stock (Eq, Show)

type Model :: Type
data Model = Model
    { provider :: Text
    -- ^ Where the checks come from, e.g. "Forgejo Actions".
    , checks :: [Check]
    , logOf :: Job -> IO (Either Text Text)
    , stepsOf :: Job -> IO (Either Text (State, [(Text, State)]))
    {- ^ A running job's state and steps, asked again every 5 seconds;
    its log comes once it ends.
    -}
    , rerunJob :: Maybe (Job -> IO (Either Text ()))
    -- ^ A finished job again; 'Nothing' where the forge cannot.
    , rerunDone :: IO ()
    -- ^ Once a job was started again, e.g. to list the runs anew.
    }

new :: Model -> IO Gtk.Widget
new m = case m.checks of
    [] -> do
        w <- label "No checks ran on the latest commit." ["muted"]
        GI.set w [#marginTop := 24, #marginStart := 24]
        Gtk.toWidget w
    _ -> do
        columns <-
            GI.new
                Gtk.Box
                [ #orientation := Gtk.OrientationHorizontal
                , #cssClasses := ["u-checks"]
                ]
        side <-
            GI.new
                Gtk.Box
                [ #orientation := Gtk.OrientationVertical
                , #widthRequest := 280
                , #hexpand := False
                , #cssClasses := ["runs"]
                ]
        heading <- Card.sectionTitle m.provider
        Gtk.widgetAddCssClass heading "provider"
        #append side heading
        #append columns side

        detail <-
            GI.new
                Gtk.Box
                [ #orientation := Gtk.OrientationVertical
                , #hexpand := True
                , #cssClasses := ["run-detail"]
                ]
        #append columns detail

        rows <- forM m.checks $ \check -> do
            button <- checkRow check
            #append side button
            pure (check, button)

        let
            select check = do
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
    GI.new
        Gtk.Button
        [ #child := content
        , #hasFrame := False
        , #tooltipText := check.description
        , #cssClasses := ["run", stateClass check.state]
        ]

{- | What a row says on its right: how long a finished check took, as its
description ends ("Successful in 48s"), or where it is.
-}
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
    copy <- Button.new Button.defaults {Button.label = Just "Copy log"}
    GI.set copy [#visible := False]
    #append header copy
    -- A finished job of the forge's own CI can run again from here.
    forM_ ((,) <$> m.rerunJob <*> check.job) $ \(again, job) -> when (check.state `elem` [Passed, Failed, Skipped]) $ do
        b <- Button.new Button.defaults {Button.label = Just "↻ Re-run job"}
        void $ GI.on b #clicked $ Button.busy b ([] :: [Gtk.Widget]) "Starting…" (again job) $ \case
            Right () -> m.rerunDone
            Left why -> GI.set b [#tooltipText := why]
        #append header b
    forM_ check.url $ \url -> do
        open <- Button.new Button.defaults {Button.label = Just "Open in browser", Button.variant = Button.Primary}
        GI.set open [#tooltipText := "Its page on the forge"]
        void $ GI.on open #clicked (openUri url)
        #append header open
    #append box header

    let
        say text = label text ["muted"] >>= \l -> GI.set l [#wrap := True] >> #append box l
        loadLog job = do
            waiting <- Skeleton.block 220
            #append box waiting
            void $ forkIO $ do
                got <- try (m.logOf job)
                void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    Gtk.boxRemove box waiting
                    case got of
                        Right (Right raw) -> do
                            let
                                ls = parseLog (check.state == Failed) raw
                            logView ls >>= #append box
                            GI.set copy [#visible := True]
                            void $ GI.on copy #clicked $ do
                                clipboard <- Gtk.widgetGetClipboard copy
                                provider <- GI.toGValue (Just (T.unlines (map body (filter (/= EndGroup) ls)))) >>= Gdk.contentProviderNewForValue
                                void (Gdk.clipboardSetContent clipboard (Just provider))
                        Right (Left _) -> say unreadable
                        Left (_ :: SomeException) -> say unreadable
                    pure False
        -- While the job runs, its steps; the log once it ends. A forge that
        -- does not list steps leaves the job as it was.
        follow job fallback = do
            steps <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #cssClasses := ["u-steps"]]
            #append box steps
            let
                ask = void $ forkIO $ do
                    got <- try (m.stepsOf job) :: IO (Either SomeException (Either Text (State, [(Text, State)])))
                    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                        attached <- Gtk.widgetGetRoot steps
                        case (attached, got) of
                            (Nothing, _) -> pure ()
                            (_, Right (Right (state, ss)))
                                | state `elem` [Running, Waiting] -> do
                                    drawSteps steps state ss
                                    void $ GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT 5 (ask >> pure False)
                                | otherwise -> Gtk.boxRemove box steps >> loadLog job
                            _ -> Gtk.boxRemove box steps >> fallback
                        pure False
            ask
    case (check.state, check.job) of
        (Skipped, _) -> say "This job was skipped, so it has no log."
        (Waiting, Just job) -> follow job (say "This job has not started yet.")
        (Waiting, Nothing) -> say "This job has not started yet."
        (_, Nothing) -> say "This check runs outside the forge; its details are on its own page."
        (Running, Just job) -> follow job (loadLog job)
        (_, Just job) -> loadLog job
    pure box
    where
        unreadable
            | m.provider == "Forgejo Actions" = "usta could not read this job's log. Forgejo shows the logs of a private repository's jobs only in the browser; Open in browser shows it there."
            | otherwise = "usta could not read this job's log; Open in browser shows it on the forge."

openUri :: Text -> IO ()
openUri url = do
    Browser.open (url)

{- | A job's log as Forgejo or GitHub gives it: each line after its time.
Forgejo starts steps with "⭐ Run Main <name>"; GitHub folds lines into
"##[group]" and marks "##[error]" and "##[command]" lines. In a failed
job, lines that speak of an error are marked. Colors stay in the text,
as ANSI codes, for 'logView' to draw.
-}
parseLog :: Bool -> Text -> [LogLine]
parseLog failed raw = map (classify . untimed . T.dropWhile (== '\xFEFF')) (T.lines raw)
    where
        untimed line = case T.breakOn " " line of
            (t, rest) | isTime t -> T.drop 1 rest
            _ -> line
        isTime t = T.length t >= 20 && "Z" `T.isSuffixOf` t && T.all (\c -> isDigit c || c `elem` ("-:.TZ" :: String)) t
        classify l
            | Just title <- T.stripPrefix "##[group]" l = Group title
            | "##[endgroup]" `T.isPrefixOf` l = EndGroup
            | Just e <- T.stripPrefix "##[error]" l = Failure e
            | Just w <- T.stripPrefix "##[warning]" l = Note ("Warning: " <> w)
            | Just c <- T.stripPrefix "##[command]" l = Command c
            | Just n <- T.stripPrefix "##[notice]" l = Note n
            | Just step <- T.stripPrefix "⭐ Run " l = Step (dropPhase step)
            | any (`T.isPrefixOf` l) ["🏁", "Job '"] || "received task" `T.isInfixOf` l || l == "workflow prepared" = Note l
            | failed && any (`T.isInfixOf` T.toLower l) ["error", "fail", "fatal", "panic", "exception"] = Failure l
            | otherwise = Output l
        dropPhase step = case T.breakOn " " step of
            (phase, rest) | phase `elem` ["Main", "Pre", "Post"] -> T.drop 1 rest
            _ -> step

-- | A line as plain text, without its colors.
body :: LogLine -> Text
body l = plain $ case l of
    Step t -> t
    Output t -> t
    Failure t -> t
    Note t -> t
    Command t -> t
    Group t -> t
    EndGroup -> ""

-- | The text without ANSI escape codes.
plain :: Text -> Text
plain t = T.concat (map snd (ansiPieces t))

{- | Text split where ANSI SGR codes ("\ESC[36;1m") change its look: each
piece with its color and whether it is bold. Other escape codes are
dropped.
-}
ansiPieces :: Text -> [((Maybe Text, Bool), Text)]
ansiPieces t = go (Nothing, False) (T.splitOn "\ESC[" t)
    where
        go _ [] = []
        go look (first : rest) = (look, first) : concatMap' look rest
        concatMap' _ [] = []
        concatMap' look (chunk : more) =
            let
                (codes, after) = T.span (\c -> isDigit c || c == ';') chunk
                command = T.take 1 after
                text = T.drop 1 after
                look' = if command == "m" then styles look (codesOf codes) else look
            in
                (look', text) : concatMap' look' more
        codesOf c = if T.null c then [0] else [n | p <- T.splitOn ";" c, Just n <- [readMaybe (T.unpack p) :: Maybe Int]]
        -- 38 and 48 take a 256-color ("5;n") or RGB ("2;r;g;b") color after
        -- them: its numbers are no codes of their own, and usta keeps its
        -- palette for them.
        styles look codes = case codes of
            [] -> look
            c : 5 : _ : more | c `elem` [38, 48] -> styles look more
            c : 2 : _ : _ : _ : more | c `elem` [38, 48] -> styles look more
            c : more -> styles (apply look c) more
        apply (fg, bold) code
            | code == 0 = (Nothing, False)
            | code == 1 = (fg, True)
            | code == 22 = (fg, False)
            | code == 39 = (Nothing, bold)
            | code >= 30 && code <= 37 = (Just (palette !! (code - 30)), bold)
            | code >= 90 && code <= 97 = (Just (palette !! (code - 90)), bold)
            | otherwise = (fg, bold)
        -- Read on the log's dark background.
        palette = ["#9AA0A9", "#F28B82", "#81C995", "#FDD663", "#8AB4F8", "#C58AF9", "#78D9EC", "#E8EAED"]

-- | Pango markup for a line with ANSI colors.
ansiMarkup :: Text -> Text
ansiMarkup t = T.concat [styled look (escape piece) | (look, piece) <- ansiPieces t, not (T.null piece)]
    where
        styled (fg, bold) x = case (fg, bold) of
            (Nothing, False) -> x
            _ -> "<span" <> maybe "" (\c -> " foreground=\"" <> c <> "\"") fg <> (if bold then " weight=\"bold\"" else "") <> ">" <> x <> "</span>"
        escape = T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

{- | A dark, numbered log, steps in their own color and error lines
marked. Forgejo marks only some steps in a log, so none fold away.
-}
logView :: [LogLine] -> IO Gtk.ScrolledWindow
logView ls = do
    lines' <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-log"]]
    -- A GitHub group's lines go in a box under its row, which folds it; a
    -- group with an error in it starts open.
    let
        draw n l = do
            row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #cssClasses := ["log-line", kind l]]
            GI.new
                Gtk.Label
                [ #label := T.pack (show n)
                , #xalign := 1
                , #yalign := 0
                , #valign := Gtk.AlignStart
                , #widthRequest := 48
                , #cssClasses := ["log-number"]
                ]
                >>= #append row
            -- Wrapping, and asking for little width, so a long line neither
            -- scrolls sideways nor widens the page.
            text <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #wrapMode := Pango.WrapModeWordChar, #maxWidthChars := 20, #cssClasses := ["log-text"]]
            Gtk.labelSetMarkup text (ansiMarkup (marked l))
            GI.set text [#selectable := True, #hexpand := True]
            #append row text
            pure (row, text)
        go _ [] = pure ()
        go n (l : rest) = case l of
            EndGroup -> go n rest
            Group title -> do
                let
                    (inside, after) = break (== EndGroup) rest
                    failing = any isFailure inside
                (row, text) <- draw n (Group title)
                members <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #visible := failing]
                forM_ (zip [n + 1 ..] inside) $ \(i, x) -> draw i x >>= #append members . fst
                let
                    arrow open = if open then "▾ " else "▸ "
                    mark open = Gtk.labelSetMarkup text (arrow open <> ansiMarkup title)
                mark failing
                click <- Gtk.gestureClickNew
                void $ GI.on click #released $ \_ _ _ -> do
                    open <- not <$> GI.get members #visible
                    GI.set members [#visible := open]
                    mark open
                Gtk.widgetAddController row click
                Gtk.widgetSetCursorFromName row (Just "pointer")
                #append lines' row
                #append lines' members
                go (n + 1 + length inside) (drop 1 after)
            _ -> draw n l >>= #append lines' . fst >> go (n + 1) rest
    go (1 :: Int) ls
    GI.new
        Gtk.ScrolledWindow
        [ #child := lines'
        , #vexpand := True
        , #hscrollbarPolicy := Gtk.PolicyTypeNever
        , #cssClasses := ["log-scroll"]
        ]
    where
        isFailure x = case x of
            Failure _ -> True
            _ -> False
        kind l = case l of
            Step _ -> "step"
            Group _ -> "step"
            Output _ -> "out"
            Failure _ -> "failure"
            Note _ -> "note"
            Command _ -> "command"
            EndGroup -> "out"
        marked l = case l of
            Step t -> "▸ " <> t
            Command t -> "$ " <> t
            _ -> body' l
        body' l = case l of
            Output t -> t
            Failure t -> t
            Note t -> t
            Group t -> t
            _ -> ""

-- | A running job's steps: done, running or waiting, each with its name.
drawSteps :: Gtk.Box -> State -> [(Text, State)] -> IO ()
drawSteps box state ss = do
    let
        clear = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear)
    clear
    let
        doneCount = length [() | (_, st) <- ss, st `elem` [Passed, Failed, Skipped]]
    label (if state == Waiting then "Waiting for a runner…" else "Running · step " <> T.pack (show (min (length ss) (doneCount + 1))) <> " of " <> T.pack (show (length ss)) <> " · the log comes when it ends") ["muted"]
        >>= #append box
    forM_ ss $ \(name, st) -> do
        row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["step-row", stepClass st]]
        mark <- case st of
            Running -> Gtk.toWidget =<< GI.new Gtk.Spinner [#spinning := True]
            _ -> Gtk.toWidget =<< label (stepMark st) ["mark"]
        #append row mark
        label name [] >>= #append row
        #append box row
    where
        stepMark st = case st of
            Passed -> "✓"
            Failed -> "✕"
            Skipped -> "–"
            _ -> "○"
        stepClass st = case st of
            Passed -> "passed"
            Failed -> "failed"
            Running -> "running"
            Skipped -> "skipped"
            Waiting -> "waiting"

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css =
    mconcat
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
        , -- The log stays dark in both themes, like a terminal.
          "box.u-checks scrolledwindow.log-scroll { border-radius: 10px; background: #16181C; }"
        , "box.u-log { padding: 12px 0; background: #16181C; }"
        , "box.u-log box.log-line { font-family: 'JetBrains Mono', monospace; font-size: 13px; color: #D7DAE0; }"
        , "box.u-log label.log-number { padding-right: 14px; color: #6B717B; }"
        , "box.u-log label.log-text { padding: 1px 16px 1px 0; }"
        , "box.u-log box.log-line.step { color: #8AB4F8; }"
        , "box.u-log box.log-line.failure { background: #3A1D1D; color: #F6B4AC; }"
        , "box.u-log box.log-line.note { color: #9AA0A9; }"
        , "box.u-log box.log-line.command { color: #78D9EC; }"
        , "box.u-checks box.u-steps { padding: 12px 16px; border: 1px solid var(--border); border-radius: 10px; background: var(--surface); }"
        , "box.u-checks box.step-row { padding: 3px 0; }"
        , "box.u-checks box.step-row label.mark { min-width: 16px; font-weight: 600; }"
        , "box.u-checks box.step-row.passed label.mark { color: var(--success); }"
        , "box.u-checks box.step-row.failed label.mark { color: var(--danger); }"
        , "box.u-checks box.step-row.waiting, box.u-checks box.step-row.skipped { color: var(--text-muted); }"
        , "box.u-checks box.step-row.running { font-weight: 600; }"
        ]
