{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Opening a pull request: branches, title, description, what it will
contain, and reviewers and labels.
-}
module Usta.UI.Pages.NewPullRequest
    ( Model (..)
    , Place (..)
    , Actions (..)
    , sample
    , new
    , css
    )
where

import Control.Concurrent (forkIO)
import Control.Monad (forM, forM_, join, void, when)
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.Graphene qualified as Graphene
import GI.Gtk qualified as Gtk
import GI.Pango qualified as Pango

import Usta.Types.Branch (Branch (..))
import Usta.Types.Discussion (NewPull (..))
import Usta.Types.Parse (parse)
import Usta.UI.Format (clock)

import Usta.UI.Components.BranchPicker qualified as BranchPicker
import Usta.UI.Components.Button qualified as Button
import Usta.UI.Components.Card qualified as Card
import Usta.UI.Components.Diff qualified as Diff
import Usta.UI.Components.FileIcon qualified as FileIcon
import Usta.UI.Components.Forge qualified as Forge
import Usta.UI.Components.MarkdownEditor qualified as MarkdownEditor
import Usta.UI.Components.Segmented qualified as Segmented
import Usta.UI.Components.Skeleton qualified as Skeleton

-- | A repository a pull request can be opened in.
type Place :: Type
data Place = Place
    { forge :: Forge.Kind
    , host :: Text
    , repo :: Text
    , defaultBranch :: Text
    -- ^ Where it merges into unless chosen otherwise.
    }

type Model :: Type
data Model = Model
    { places :: [Place]
    , chosen :: Int
    -- ^ The one selected at first.
    }

-- | What the form asks of the forge. Each runs off the main thread.
type Actions :: Type
data Actions = Actions
    { branchesOf :: Place -> IO (Either Text [Branch])
    , compareOf :: Place -> Text -> Text -> IO (Either Text ([(Text, Text)], [Diff.File]))
    {- ^ Into and from: the commits (short hash and subject) and the
    changes the pull request would have.
    -}
    , forksOf :: Place -> IO (Either Text [Text])
    -- ^ Your forks of the repository, by full name.
    , parentOf :: Place -> IO (Either Text (Maybe Text))
    -- ^ The repository a fork was made from.
    , create :: Place -> NewPull -> IO (Either Text Int)
    , opened :: Place -> Int -> Text -> (Text, Text) -> IO ()
    {- ^ With the new pull request's number, title and branches (head
    and base).
    -}
    , cancel :: IO ()
    }

new :: Actions -> Model -> IO Gtk.Widget
new actions m = do
    page <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #hexpand := True
            , #vexpand := True
            , #cssClasses := ["u-new-pr"]
            ]
    header <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationHorizontal
            , #spacing := 12
            , #cssClasses := ["form-header"]
            ]
    cancelButton <-
        Button.new
            Button.defaults
                { Button.label = Just "Cancel"
                , Button.icon = Just "go-previous-symbolic"
                , Button.variant = Button.Flat
                }
    void $ GI.on cancelButton #clicked actions.cancel
    #append header cancelButton
    label "New pull request" ["form-title"] >>= #append header
    #append page header

    columns <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
    main <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #spacing := 16
            , #hexpand := True
            , #cssClasses := ["form-main"]
            ]

    -- Repository, then the branches it has, loaded when it is chosen.
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
    repo <- Gtk.dropDownNewFromStrings [p.host <> "/" <> p.repo | p <- m.places]
    GI.set repo [#cssClasses := ["mono"]]
    repoField <- Gtk.toWidget repo >>= field "Repository"
    GI.set repoField [#hexpand := True]
    #append row repoField
    -- Each side is a repository and a branch: into the repository or the
    -- one it was forked from, from it or a fork of yours. The repository
    -- shows only when there is a choice. Each picker is drawn again when a
    -- branch is chosen in it.
    let
        branchSide = do
            repos <- Gtk.dropDownNewFromStrings []
            GI.set repos [#visible := False, #cssClasses := ["mono", "side-repo"]]
            slot <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
            both <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
            #append both repos
            #append both slot
            pure (repos, slot, both)
    (intoRepos, intoSlot, intoSide) <- branchSide
    Gtk.toWidget intoSide >>= field "Into" >>= #append row
    arrow <- label "←" ["muted", "arrow"]
    GI.set arrow [#valign := Gtk.AlignEnd]
    #append row arrow
    (fromRepos, fromSlot, fromSide) <- branchSide
    Gtk.toWidget fromSide >>= field "From" >>= #append row
    #append main row
    status <- label "" ["small", "muted"]
    GI.set status [#visible := False]
    #append main status

    titleEntry <- GI.new Gtk.Entry [#placeholderText := "What it does, e.g. feat: render the main window", #cssClasses := ["title-entry"]]
    Gtk.toWidget titleEntry >>= field "Title" >>= #append main
    (editorBox, buffer) <- MarkdownEditor.new MarkdownEditor.defaults {MarkdownEditor.height = 160}
    #append main editorBox
    contentsSlot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
    #append main contentsSlot
    scroll <-
        GI.new
            Gtk.ScrolledWindow
            [ #child := main
            , #hexpand := True
            , #hscrollbarPolicy := Gtk.PolicyTypeNever
            ]
    #append columns scroll

    side <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #spacing := 18
            , #widthRequest := 280
            , #hexpand := False
            , #cssClasses := ["form-side"]
            ]
    reviewersEntry <- GI.new Gtk.Entry [#placeholderText := "User names, separated by commas"]
    Gtk.toWidget reviewersEntry >>= field "Reviewers" >>= #append side
    labelsEntry <- GI.new Gtk.Entry [#placeholderText := "Label names, separated by commas"]
    Gtk.toWidget labelsEntry >>= field "Labels" >>= #append side
    bottom <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #spacing := 8
            , #vexpand := True
            , #valign := Gtk.AlignEnd
            ]
    problem <- label "" ["small", "problem"]
    GI.set problem [#wrap := True]
    #append bottom problem
    createButton <- Button.new Button.defaults {Button.label = Just "Create pull request", Button.variant = Button.Primary}
    #append bottom createButton
    -- Enter in a field and Cmd Enter in the description create it.
    let
        createIt = void (Gtk.widgetActivate createButton)
    mapM_ (\e -> GI.on e #activate createIt) [titleEntry, reviewersEntry, labelsEntry]
    MarkdownEditor.sendOn editorBox False createIt
    draftButton <- Button.new Button.defaults {Button.label = Just "Create as draft"}
    #append bottom draftButton
    #append side bottom
    #append columns side
    #append page columns

    let
        placeAt i = if i >= 0 && i < length m.places then Just (m.places !! i) else Nothing
    into <- newIORef (Nothing :: Maybe Text)
    from <- newIORef (Nothing :: Maybe Text)
    -- Each side's repository choices, and the one chosen.
    intoChoices <- newIORef []
    fromChoices <- newIORef []
    -- Each side's branches, fetched once a repository is chosen.
    knownInto <- newIORef (pure (Left "Loading branches\8230") :: IO (Either Text [Branch]))
    knownFrom <- newIORef (pure (Left "Loading branches\8230") :: IO (Either Text [Branch]))
    c <- clock
    previewRef <- newIORef (pure ())
    -- Only the latest fetch of branches is shown.
    fetches <- newIORef (0 :: Int)
    let
        chosenIn d choices = do
            i <- fromIntegral <$> Gtk.dropDownGetSelected d
            names <- readIORef choices
            pure (if i < length names then Just (names !! i) else Nothing)
        -- The place each side is in: the chosen one, on another repository.
        sides = do
            index <- fromIntegral <$> Gtk.dropDownGetSelected repo
            case placeAt index of
                Nothing -> pure Nothing
                Just place -> do
                    i <- fromMaybe place.repo <$> chosenIn intoRepos intoChoices
                    f <- fromMaybe place.repo <$> chosenIn fromRepos fromChoices
                    pure (Just (place {repo = i}, place {repo = f}))
        -- The branch with the changes as the forge names it: "owner:branch"
        -- from another repository.
        headOf intoPlace fromPlace t = if fromPlace.repo == intoPlace.repo then t else T.takeWhile (/= '/') fromPlace.repo <> ":" <> t
        picker slot ref known place = do
            old <- Gtk.widgetGetFirstChild slot
            forM_ old (Gtk.boxRemove slot)
            shown <- readIORef ref
            fetch <- readIORef known
            b <-
                BranchPicker.new
                    BranchPicker.Picker
                        { shown = maybe "Choose a branch" id shown
                        , defaultBranch = place.defaultBranch
                        , clock = c
                        , branches = fetch
                        , tags = Nothing
                        , choose = \name -> writeIORef ref (Just name) >> picker slot ref known place >> join (readIORef previewRef)
                        , note = ""
                        }
            #append slot b
        pickers =
            sides
                >>= mapM_
                    ( \(intoPlace, fromPlace) ->
                        picker intoSlot into knownInto intoPlace >> picker fromSlot from knownFrom fromPlace
                    )
        selected = readIORef
        offer d choices names = do
            writeIORef choices names
            model <- Gtk.stringListNew (Just names)
            Gtk.dropDownSetModel d (Just model)
            GI.set d [#visible := length names > 1]
        -- The branches of both sides; one fetch when they are one repository.
        branches =
            sides
                >>= mapM_
                    ( \(intoPlace, fromPlace) -> do
                        writeIORef into Nothing >> writeIORef from Nothing
                        mapM_ (\k -> writeIORef k (pure (Left "Loading branches\8230"))) [knownInto, knownFrom]
                        pickers
                        say status "Loading branches\8230"
                        mine <- (+ 1) <$> readIORef fetches
                        writeIORef fetches mine
                        void $ forkIO $ do
                            foundInto <- actions.branchesOf intoPlace
                            foundFrom <- if fromPlace.repo == intoPlace.repo then pure foundInto else actions.branchesOf fromPlace
                            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                                latest <- readIORef fetches
                                when (latest == mine) $ do
                                    writeIORef knownInto (pure foundInto)
                                    writeIORef knownFrom (pure foundFrom)
                                    case (foundInto, foundFrom) of
                                        (Right intoBranches, Right fromBranches) -> do
                                            let
                                                base = if any ((== intoPlace.defaultBranch) . (.name)) intoBranches then Just intoPlace.defaultBranch else take 1 (map (.name) intoBranches) `orFirst` Nothing
                                                same = fromPlace.repo == intoPlace.repo
                                                others = [b.name | b <- fromBranches, not same || Just b.name /= base]
                                                -- A branch other than the default, as the one with the changes.
                                                tip = [n | n <- others, n /= fromPlace.defaultBranch] `orFirst` (take 1 others `orFirst` Nothing)
                                            writeIORef into base
                                            writeIORef from tip
                                            pickers
                                            say status (if null others then "The repository has one branch: push another to open a pull request." else "")
                                            join (readIORef previewRef)
                                        (Left text, _) -> say status text
                                        (_, Left text) -> say status text
                                pure False
                    )
        -- A fork goes into the one it was made from, from itself; any other
        -- into itself, from itself or a fork of yours.
        load = do
            index <- fromIntegral <$> Gtk.dropDownGetSelected repo
            forM_ (placeAt index) $ \place -> do
                offer intoRepos intoChoices [place.repo]
                offer fromRepos fromChoices [place.repo]
                branches
                void $ forkIO $ do
                    parent <- either (const Nothing) id <$> actions.parentOf place
                    let
                        target = fromMaybe place.repo parent
                    forks <- either (const []) id <$> actions.forksOf place {repo = target}
                    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                        offer intoRepos intoChoices (nub [target, place.repo])
                        offer fromRepos fromChoices (nub ([place.repo, target] <> forks))
                        when (target /= place.repo || not (null forks)) branches
                        pure False
        submit draft = do
            found <- sides
            base <- selected into
            tip <- selected from
            titleText <- T.strip <$> Gtk.editableGetText titleEntry
            bodyText <- maybe "" id <$> GI.get buffer #text
            reviewersText <- Gtk.editableGetText reviewersEntry
            labelsText <- Gtk.editableGetText labelsEntry
            let
                reviewers = traverse parse (names' reviewersText)
            case (found, base, tip, reviewers) of
                _ | T.null titleText -> GI.set problem [#label := "Give the pull request a title."]
                (Just (place, fromPlace), Just b, Just t, Right who)
                    | b == t && fromPlace.repo == place.repo -> GI.set problem [#label := "Choose two different branches."]
                    | otherwise -> do
                        GI.set problem [#label := ""]
                        let
                            pull =
                                NewPull
                                    { title = titleText
                                    , body = bodyText
                                    , from = headOf place fromPlace t
                                    , into = b
                                    , labels = names' labelsText
                                    , reviewers = who
                                    , draft = draft
                                    }
                            (pressed, other) = if draft then (draftButton, createButton) else (createButton, draftButton)
                        Button.busy pressed [other] "Creating…" (actions.create place pull)
                            $ either (\text -> GI.set problem [#label := text]) (\number -> actions.opened place number titleText (headOf place fromPlace t, b))
                (_, _, _, Left _) -> GI.set problem [#label := "A reviewer is not a user name."]
                _ -> GI.set problem [#label := "Choose the repository and both branches."]
    -- Each change of branch asks again; only the latest answer is shown.
    asked <- newIORef (0 :: Int)
    let
        preview = do
            found <- sides
            base <- selected into
            tip <- selected from
            turn <- (+ 1) <$> readIORef asked
            writeIORef asked turn
            let
                replace widget = do
                    old <- Gtk.widgetGetFirstChild contentsSlot
                    forM_ old (Gtk.boxRemove contentsSlot)
                    forM_ widget (#append contentsSlot)
            case (found, base, tip) of
                (Just (place, fromPlace), Just b, Just t) | b /= t || fromPlace.repo /= place.repo -> do
                    Skeleton.block 88 >>= replace . Just
                    void $ forkIO $ do
                        compared <- actions.compareOf place b (headOf place fromPlace t)
                        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                            latest <- readIORef asked
                            when (latest == turn) $ case compared of
                                Right (commits, changes) -> contents scroll b t commits changes >>= Gtk.toWidget >>= replace . Just
                                Left text -> label text ["small", "muted"] >>= Gtk.toWidget >>= replace . Just
                            pure False
                _ -> replace Nothing
    writeIORef previewRef preview
    Gtk.dropDownSetSelected repo (fromIntegral m.chosen)
    void $ GI.on repo (GI.PropertyNotify #selected) (const load)
    forM_ [intoRepos, fromRepos] $ \d -> void $ GI.on d (GI.PropertyNotify #selected) (const branches)
    load
    void $ GI.on createButton #clicked (submit False)
    void $ GI.on draftButton #clicked (submit True)
    Gtk.toWidget page
    where
        names' = filter (not . T.null) . map T.strip . T.splitOn ","
        orFirst (x : _) _ = Just x
        orFirst [] d = d

{- | What the pull request would contain: its commits, and the files it
changes, one a row, with their diffs under them. A row scrolls
@scroll@ to its diff.
-}
contents :: Gtk.ScrolledWindow -> Text -> Text -> [(Text, Text)] -> [Diff.File] -> IO Gtk.Box
contents scroll into from commits changes = do
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12]
    if null commits
        then
            label (from <> " has no commits that " <> into <> " lacks: there is nothing to merge.") ["small", "muted"]
                >>= #append box
        else do
            (tabs, buttons) <-
                Segmented.new
                    [ "Commits " <> T.pack (show (length commits))
                    , "Files changed " <> T.pack (show (length changes))
                    ]
                    0
            #append box tabs
            stack <- GI.new Gtk.Stack [#vhomogeneous := False, #interpolateSize := True]
            #append box stack

            commitCard <- Card.new
            forM_ commits $ \(short, subject) -> do
                row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["content-row"]]
                label short ["mono", "hash"] >>= #append row
                s' <- label subject []
                GI.set s' [#ellipsize := Pango.EllipsizeModeEnd]
                #append row s'
                #append commitCard row
            void $ Gtk.stackAddNamed stack commitCard (Just "commits")

            files <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 16]
            list <- Card.new
            #append files list
            diffs <- forM changes $ \f -> do
                d <- Diff.plain f
                #append files d
                pure d
            forM_ (zip changes diffs) $ \(f, d) -> do
                row <- GI.new Gtk.Button [#cssClasses := ["content-row", "file-row"]]
                inner <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
                FileIcon.file 16 f.path >>= #append inner
                name <- label f.path ["mono"]
                GI.set name [#hexpand := True, #ellipsize := Pango.EllipsizeModeMiddle]
                #append inner name
                let
                    (added, removed) = T.breakOn " " (Diff.stat f)
                label added ["mono", "added"] >>= #append inner
                label (T.strip removed) ["mono", "removed"] >>= #append inner
                GI.set row [#child := inner]
                void $ GI.on row #clicked $ do
                    (ok, point) <- Gtk.widgetComputePoint d scroll =<< pointAt 0 0
                    adjustment <- Gtk.scrolledWindowGetVadjustment scroll
                    now <- Gtk.adjustmentGetValue adjustment
                    when ok $ Graphene.getPointY point >>= \y -> Gtk.adjustmentSetValue adjustment (now + realToFrac y - 12)
                #append list row
            void $ Gtk.stackAddNamed stack files (Just "files")
            forM_ (zip buttons ["commits", "files"]) $ \(b, name) ->
                void $ GI.on b #toggled $ GI.get b #active >>= \on' -> when on' (Gtk.stackSetVisibleChildName stack name)
    pure box
    where
        pointAt x y = do
            p <- Graphene.newZeroPoint
            Graphene.pointInit p x y

-- | A small caption above an input.
field :: Text -> Gtk.Widget -> IO Gtk.Box
field caption input = do
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
    label caption ["caption"] >>= #append box
    #append box input
    pure box

-- | A note that takes no room when it says nothing.
say :: Gtk.Label -> Text -> IO ()
say l text = GI.set l [#label := text, #visible := not (T.null text)]

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: Model
sample =
    Model
        { places =
            [ Place Forge.GitHub "github.com" "abdivasiyev/usta" "master"
            , Place Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" "master"
            ]
        , chosen = 0
        }

css :: Text
css =
    mconcat
        [ "box.u-new-pr box.form-header {"
        , "  padding: 8px 16px; background: var(--surface); border-bottom: 1px solid var(--border);"
        , "}"
        , "box.u-new-pr label.form-title { font-size: 16px; font-weight: 600; }"
        , "box.u-new-pr box.form-main { padding: 20px 32px; }"
        , "box.u-new-pr box.form-side { padding: 20px 16px; }"
        , "box.u-new-pr label.caption { font-size: 13px; color: var(--text-soft); }"
        , "box.u-new-pr label.small { font-size: 13px; }"
        , "box.u-new-pr label.problem { color: var(--danger); }"
        , "box.u-new-pr label.arrow { margin-bottom: 10px; }"
        , "box.u-new-pr label.hash { color: var(--accent); }"
        , "box.u-new-pr label.added { color: var(--success); }"
        , "box.u-new-pr label.removed { color: var(--danger); }"
        , "box.u-new-pr entry {"
        , "  min-height: 38px; border: 1px solid var(--input-border); border-radius: 8px;"
        , "  background: var(--surface); color: var(--text); box-shadow: none;"
        , "}"
        , "box.u-new-pr entry.title-entry { min-height: 42px; font-size: 15px; }"
        , "box.u-new-pr menubutton.u-branch-button > button { min-height: 40px; border-radius: 8px; }"
        , "box.u-new-pr dropdown > button {"
        , "  min-height: 40px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 8px;"
        , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
        , "}"
        , "box.u-new-pr textview.editor {"
        , "  border: 1px solid var(--input-border); border-radius: 0 8px 8px 8px;"
        , "  font-family: 'JetBrains Mono', monospace; font-size: 13px;"
        , "}"
        , "box.u-new-pr textview.editor text { background: var(--surface); color: var(--text); }"
        , "box.u-new-pr label.preview {"
        , "  min-height: 160px; padding: 12px; border: 1px solid var(--input-border); border-radius: 8px;"
        , "  background: var(--surface);"
        , "}"
        , "box.u-new-pr box.content-row { padding: 10px 12px; border-bottom: 1px solid var(--border-subtle); }"
        , "box.u-new-pr box.content-row:last-child, box.u-new-pr button.content-row:last-child { border-bottom: none; }"
        , "box.u-new-pr button.file-row { padding: 8px 12px; border: none; border-radius: 0; border-bottom: 1px solid var(--border-subtle); background: transparent; background-image: none; box-shadow: none; }"
        , "box.u-new-pr button.file-row:hover { background: var(--border-subtle); }"
        ]
