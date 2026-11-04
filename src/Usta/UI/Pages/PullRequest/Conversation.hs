{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Conversation tab of a pull request: description, timeline,
-- review threads, the merge box and a comment field, with branch,
-- reviewers, labels and linked issues on the side.
module Usta.UI.Pages.PullRequest.Conversation
  ( Event (..)
  , MergeState (..)
  , Model (..)
  , Actions (..)
  , fromDiscussion
  , sample
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (filterM, forM, forM_, unless, void, when)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, utc)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.AssignPickers as AssignPickers
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.LabelPicker as LabelPicker
import qualified Usta.UI.Components.Thread as Thread
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Components.Skeleton as Skeleton
import Usta.UI.Format (Clock (..), ago)
import Usta.UI.Pages.Issue (Comment (..))
import Usta.UI.Pages.Issues (labelTone)
import Usta.UI.Style (Tone (..))
import qualified Usta.Types.Commit as C
import qualified Usta.Types.Discussion as D
import qualified Usta.Types.Label as L
import Usta.Types.Parse (unparse)
import Usta.Types.Milestone (Milestone)
import Usta.Types.Readiness (Readiness (..), UpdateStyle (..))
import Usta.Types.Reviewer (Candidate (..), Reviewer (..), Stance (..), Who (..))

-- | Something that happened on the pull request. Add a constructor and a
-- case in 'eventRow' for a new kind.
type Event :: Type
data Event
  = Committed Text Text
    -- ^ Short hash and message.
  | CheckFinished Bool Text
    -- ^ Passed or failed, and what ran.
  | ReviewRequestedFrom Text

-- | What the merge box knows.
type MergeState :: Type
data MergeState = MergeState
  { checksPassed :: Maybe Bool
    -- ^ 'Nothing' when the repository runs no checks, or they still run.
  , checksRunning :: Bool
  , readiness :: Maybe Readiness
    -- ^ 'Nothing' when the forge did not say.
  }

type Model :: Type
data Model = Model
  { opening :: Comment
    -- ^ The description.
  , comments :: [Comment]
  , events :: [Event]
  , threads :: [Thread.Shown]
    -- ^ The review threads, each with its file and code.
  , merge :: Maybe MergeState
    -- ^ 'Nothing' once it is merged or closed.
  , closed :: Bool
  , source :: Text
  , target :: Text
  , reviewers :: [Reviewer]
  , labels :: [(Tone, Text)]
  , linkedIssues :: [Text]
  , clock :: Clock
  }

-- | What the merge box and the comment box do. Each runs off the main
-- thread; a 'Left' is said under the box, and a 'Right' runs 'refresh'.
type Actions :: Type
data Actions = Actions
  { comment :: Text -> IO (Either Text ())
  , setClosed :: Bool -> IO (Either Text ())
  , merge :: D.Merge -> Bool -> IO (Either Text ())
    -- ^ 'True' deletes the branch after.
  , scheduleMerge :: D.Merge -> Bool -> IO (Either Text ())
    -- ^ Merges once the checks pass.
  , cancelScheduledMerge :: IO (Either Text ())
  , markReady :: IO (Either Text ())
  , updateBranch :: UpdateStyle -> IO (Either Text ())
  , requestReview :: [Who] -> IO (Either Text ())
  , unrequestReview :: Who -> IO (Either Text ())
  , labels :: [Text] -> LabelPicker.Picker
    -- ^ The labels picker, from the labels it has.
  , assigning :: AssignPickers.Assignees
  , milestoning :: AssignPickers.Milestones
  , milestoneNow :: Maybe Milestone
    -- ^ Its milestone in full, for the progress.
  , candidates :: IO (Either Text [Candidate])
    -- ^ Who can be asked to review.
  , refresh :: IO ()
  }

-- | The pull request as the forge has it, with its commits, its branches
-- (head and base), and whether its checks passed.
fromDiscussion :: Clock -> D.Discussion -> [C.Commit] -> (Text, Text) -> Maybe Bool -> Model
fromDiscussion c d commits (from, into) checks = Model
  { opening = said "opened this " d.opening
  , comments = map (said "commented ") d.comments
  , events = [Committed (T.take 7 (unparse x.sha)) x.subject | x <- commits]
  , threads = []
  , merge = if d.closed then Nothing else Just MergeState {checksPassed = checks, checksRunning = False, readiness = Nothing}
  , closed = d.closed && not d.merged
  , source = from
  , target = into
  , reviewers = []
  , labels = [(labelTone l.color, l.name) | l <- d.labels]
  , linkedIssues = []
  , clock = c
  }
  where
    said verb x = Comment
      { author = unparse x.author
      , initials = T.toUpper (T.take 2 (unparse x.author))
      , when' = verb <> ago c x.createdAt
      , body = x.body
      }

new :: Actions -> Model -> IO Gtk.Widget
new actions m = do
  columns <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #vexpand := True
                            , #cssClasses := ["u-conversation"]
                            ]
  main <- vbox 16
  GI.set main [#hexpand := True, #cssClasses := ["main"]]

  commentCard m.opening >>= #append main
  forM_ m.comments $ \x -> commentCard x >>= #append main
  timeline <- vbox 10
  Gtk.widgetAddCssClass timeline "timeline"
  forM_ m.events $ \e -> eventRow e >>= #append timeline
  #append main timeline
  forM_ m.threads $ \t -> Thread.new t >>= #append main . (.widget)
  problem <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #visible := False, #cssClasses := ["problem"]]
  -- Buttons wait for the forge, so a second click sends nothing twice.
  let run button row doing act = do
        GI.set problem [#visible := False]
        Button.busy button [row] doing act $
          either (\text -> GI.set problem [#label := text, #visible := True]) (const actions.refresh)
  forM_ m.merge $ \s -> do
    Card.sectionTitle "Merge" >>= #append main
    mergeBox run actions m s >>= #append main
  commentBox run actions m >>= #append main
  #append main problem

  -- Keep the reading column a comfortable width on wide windows.
  clamp <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #hexpand := True]
  GI.set main [#widthRequest := 560]
  #append clamp main
  scroll <- GI.new Gtk.ScrolledWindow [ #child := clamp
                                      , #hexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]
  #append columns scroll
  details run actions m >>= #append columns
  Gtk.toWidget columns

commentCard :: Comment -> IO Gtk.Box
commentCard x = do
  card <- Card.new
  top <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 10
                        , #cssClasses := ["card-header"]
                        ]
  Avatar.new 24 x.initials >>= #append top
  label x.author ["strong"] >>= #append top
  label x.when' ["muted"] >>= #append top
  #append card top
  text <- if T.null (T.strip x.body)
    then label "No description." ["card-body", "muted"] >>= Gtk.toWidget
    else Markdown.new x.body
  Gtk.widgetAddCssClass text "card-body"
  #append card text
  pure card

eventRow :: Event -> IO Gtk.Box
eventRow e = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  case e of
    Committed hash message -> do
      label hash ["mono", "hash"] >>= #append row
      label message ["soft"] >>= #append row
    CheckFinished passed what -> do
      Icon.toned 14 (if passed then Success else Danger) (if passed then Icon.Check else Icon.Cross)
        >>= #append row
      label what ["mono", "strong"] >>= #append row
      label (if passed then "passed" else "failed") ["soft"] >>= #append row
    ReviewRequestedFrom who -> do
      Badge.dot Warning >>= #append row
      label "Review requested from" ["soft"] >>= #append row
      label who ["strong"] >>= #append row
  pure row

-- | @run@ disables the buttons while an action runs.
-- | Only what applies: a draft, a branch behind its base, conflicts,
-- approvals still missing, the checks; then how to merge. The button
-- stays off with the reason beside it until nothing blocks.
mergeBox :: (Gtk.Button -> Gtk.Box -> Text -> IO (Either Text ()) -> IO ()) -> Actions -> Model -> MergeState -> IO Gtk.Box
mergeBox run actions m s = do
  card <- Card.new
  Gtk.widgetAddCssClass card "merge-box"
  let approved = length [r | r <- m.reviewers, r.stance == Approved, not r.stale]
      needed = maybe 0 (.requiredApprovals) s.readiness
      missing = needed - approved
      row tone title sub extras = do
        box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["merge-row", "state"]]
        dot <- Badge.dot tone
        GI.set dot [#valign := Gtk.AlignStart, #marginTop := 5]
        #append box dot
        texts <- vbox 2
        GI.set texts [#hexpand := True]
        label title ["state-title"] >>= #append texts
        unless (T.null sub) $ label sub ["muted", "small"] >>= \l -> GI.set l [#wrap := True] >> #append texts l
        #append box texts
        forM_ extras $ \w -> GI.set w [#valign := Gtk.AlignCenter] >> #append box w
        #append card box
        pure box
  forM_ s.readiness $ \r -> do
    when r.draft $ do
      ready <- Button.new Button.defaults {Button.label = Just "Ready for review", Button.variant = Button.Primary}
      box <- row Neutral "This pull request is a draft" "Reviewers are not asked until it is ready." =<< sequence [Gtk.toWidget ready]
      void $ GI.on ready #clicked (run ready box "Marking ready…" actions.markReady)
    when (r.behind > 0) $ do
      (styles, styleButtons) <- Segmented.new ["Merge", "Rebase"] 0
      update <- Button.new Button.defaults {Button.label = Just "Update branch"}
      box <- row Warning (m.source <> " is " <> plural r.behind "commit" <> " behind " <> m.target)
        ("Bring in the newer commits from " <> m.target <> " before merging.") =<< sequence [Gtk.toWidget styles, Gtk.toWidget update]
      void $ GI.on update #clicked $ do
        rebase <- maybe (pure False) (\b -> GI.get b #active) (lookup 1 (zip [0 :: Int ..] styleButtons))
        run update box "Updating…" (actions.updateBranch (if rebase then ByRebase else ByMerge))
    when r.scheduled $ do
      cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
      box <- row Success "Merges when the checks pass"
        "Scheduled: it merges by itself once the checks pass and approvals are in." =<< sequence [Gtk.toWidget cancel]
      void $ GI.on cancel #clicked (run cancel box "Cancelling…" actions.cancelScheduledMerge)
    when r.conflicts $
      void $ row Danger ("Conflicts with " <> m.target) "Resolve them on your computer, then push; the merge waits until then." []
  when (missing > 0) $
    void $ row Warning (T.pack (show approved) <> " of " <> plural needed "required approval")
      ("The protected branch " <> m.target <> " asks for them before merging.") []
  case (s.checksPassed, s.checksRunning) of
    (_, True) -> void $ row Warning "Checks are running" "Merging waits until they pass, or merges when they do." []
    (Just True, _) -> void $ row Success "All checks have passed" "" []
    (Just False, _) -> void $ row Danger "Some checks failed" "See the Checks tab for what failed." []
    _ -> pure ()

  buttons <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["merge-row"]]
  -- In the order of [minBound ..]: Squash, MergeCommit, Rebase.
  how <- Gtk.dropDownNewFromStrings ["Squash and merge", "Create a merge commit", "Rebase and merge"]
  #append buttons how
  delete <- GI.new Gtk.CheckButton [#label := "Delete branch after merge", #active := True]
  #append buttons delete
  let scheduled = maybe False (.scheduled) s.readiness
  auto <- GI.new Gtk.CheckButton [ #label := "Merge automatically when checks pass and approvals are in"
                                 , #visible := s.checksRunning && not scheduled ]
  #append buttons auto
  reason <- label "" ["muted", "small", "reason"]
  GI.set reason [#hexpand := True, #xalign := 1]
  #append buttons reason
  mergeButton <- Button.new Button.defaults { Button.label = Just "Merge", Button.variant = Button.Primary }
  #append buttons mergeButton
  let blockers waiting =
        [ "Already scheduled" | scheduled ]
        <> [ "Mark it ready first" | maybe False (.draft) s.readiness ]
        <> [ "Resolve the conflicts first" | maybe False (.conflicts) s.readiness ]
        <> [ "Some checks failed" | s.checksPassed == Just False ]
        <> [ "Needs " <> plural missing "more approval" | missing > 0, not waiting ]
        <> [ "Checks are still running" | s.checksRunning, not waiting ]
      refreshButton = do
        waiting <- GI.get auto #active
        let why = blockers waiting
        GI.set mergeButton [ #sensitive := null why
                           , #label := if waiting then "Merge when ready" else "Merge" ]
        GI.set reason [#label := T.intercalate " · " (take 1 why)]
  refreshButton
  void $ GI.on auto #toggled refreshButton
  void $ GI.on mergeButton #clicked $ do
    chosen <- Gtk.dropDownGetSelected how
    deleting <- GI.get delete #active
    waiting <- GI.get auto #active
    let ways = [minBound .. maxBound] :: [D.Merge]
        way = ways !! fromIntegral chosen
    when (fromIntegral chosen < length ways) $
      if waiting
        then run mergeButton buttons "Scheduling…" (actions.scheduleMerge way deleting)
        else run mergeButton buttons "Merging…" (actions.merge way deleting)
  #append card buttons
  pure card
  where
    plural n word = T.pack (show n) <> " " <> word <> (if n == 1 then "" else "s")

commentBox :: (Gtk.Button -> Gtk.Box -> Text -> IO (Either Text ()) -> IO ()) -> Actions -> Model -> IO Gtk.Box
commentBox run actions m = do
  box <- vbox 8
  label "Add a comment" ["strong"] >>= #append box
  editor <- GI.new Gtk.TextView [ #wrapMode := Gtk.WrapModeWordChar
                                , #topMargin := 10
                                , #bottomMargin := 10
                                , #leftMargin := 12
                                , #rightMargin := 12
                                , #heightRequest := 96
                                , #cssClasses := ["editor"]
                                ]
  #append box editor
  buttons <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 8
                            , #halign := Gtk.AlignEnd
                            ]
  -- A merged pull request can only be commented on.
  when (m.closed || isJust m.merge) $ do
    close <- Button.new Button.defaults
      { Button.label = Just (if m.closed then "Reopen pull request" else "Close pull request") }
    void $ GI.on close #clicked $ run close buttons (if m.closed then "Reopening…" else "Closing…") (actions.setClosed (not m.closed))
    #append buttons close
  send <- Button.new Button.defaults { Button.label = Just "Comment", Button.variant = Button.Primary }
  void $ GI.on send #clicked $ do
    buffer <- Gtk.textViewGetBuffer editor
    text <- maybe "" id <$> GI.get buffer #text
    unless (T.null (T.strip text)) $ run send buttons "Commenting…" (actions.comment text)
  #append buttons send
  #append box buttons
  pure box

details :: (Gtk.Button -> Gtk.Box -> Text -> IO (Either Text ()) -> IO ()) -> Actions -> Model -> IO Gtk.Box
details run actions m = do
  side <- vbox 20
  GI.set side [#widthRequest := 260, #hexpand := False, #cssClasses := ["details"]]

  section side "Branch" $ \box -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    label m.source ["chip", "mono"] >>= #append row
    label "→" ["muted"] >>= #append row
    label m.target ["chip", "mono"] >>= #append row
    #append box row
  reviewers <- vbox 6
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  title <- Card.sectionTitle "Reviewers"
  GI.set title [#hexpand := True]
  #append header title
  request <- GI.new Gtk.Button [#label := "Request", #tooltipText := "Request a review (Ctrl Shift R)", #cssClasses := ["link-button"]]
  #append header request
  #append reviewers header
  when (null m.reviewers) $ label "Nobody yet" ["muted"] >>= #append reviewers
  forM_ m.reviewers $ \r -> reviewerRow run actions r >>= #append reviewers
  #append side reviewers
  (people, openPeople) <- AssignPickers.assigneesSection actions.assigning
  #append side people
  open <- requestPicker actions m request
  void $ GI.on request #clicked open
  keys <- GI.new Gtk.ShortcutController [#scope := Gtk.ShortcutScopeManaged]
  shortcut keys "<Primary><Shift>r" open
  Gtk.widgetAddController side keys
  labelsBox <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  labelsHead <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  labelsTitle <- Card.sectionTitle "Labels"
  GI.set labelsTitle [#hexpand := True]
  #append labelsHead labelsTitle
  editLabels <- GI.new Gtk.Button [#label := "Edit", #tooltipText := "Edit labels (l)", #cssClasses := ["link-button"]]
  #append labelsHead editLabels
  #append labelsBox labelsHead
  labelRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  forM_ m.labels $ \(tone, name) -> Badge.new tone name >>= #append labelRow
  when (null m.labels) $ label "None" ["muted"] >>= #append labelRow
  #append labelsBox labelRow
  #append side labelsBox
  openLabels <- LabelPicker.attach (actions.labels (map snd m.labels)) editLabels
  void $ GI.on editLabels #clicked openLabels
  labelKeys <- GI.new Gtk.ShortcutController [#scope := Gtk.ShortcutScopeManaged]
  shortcut labelKeys "l" openLabels
  shortcut labelKeys "a" openPeople
  (goal, openGoal) <- AssignPickers.milestoneSection actions.milestoning actions.milestoneNow
  #append side goal
  shortcut labelKeys "m" openGoal
  Gtk.widgetAddController side labelKeys
  unless (null m.linkedIssues) $ section side "Linked issues" $ \box ->
    forM_ m.linkedIssues $ \issue -> label issue ["link"] >>= #append box
  pure side
  where
    section :: Gtk.Box -> Text -> (Gtk.Box -> IO ()) -> IO ()
    section side title fill = do
      box <- vbox 6
      Card.sectionTitle title >>= #append box
      fill box
      #append side box

-- | A popover under @anchor@ for asking people and teams to review, as
-- the design draws it: search, then those who changed the files before,
-- collaborators and teams. Those already reviewing are ticked for good.
-- Gives what opens it; the candidates load each time it opens.
requestPicker :: Actions -> Model -> Gtk.Button -> IO (IO ())
requestPicker actions m anchor = do
  outer <- vbox 0
  GI.set outer [#widthRequest := 360, #cssClasses := ["u-request-review"]]
  top <- vbox 10
  GI.set top [#cssClasses := ["top"]]
  label "Request a review" ["picker-title"] >>= #append top
  search <- GI.new Gtk.SearchEntry [#placeholderText := "Search people and teams", #cssClasses := ["search"]]
  #append top search
  list <- vbox 0
  GI.set list [#cssClasses := ["rows"]]
  scroll <- GI.new Gtk.ScrolledWindow [ #child := list, #maxContentHeight := 380, #propagateNaturalHeight := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever ]
  #append top scroll
  #append outer top
  bottom <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["bottom"]]
  counter <- label "" ["muted", "small"]
  GI.set counter [#hexpand := True]
  cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
  send <- Button.new Button.defaults {Button.label = Just "Request", Button.variant = Button.Primary}
  mapM_ (Gtk.widgetAddCssClass cancel) ["small"]
  mapM_ (Gtk.widgetAddCssClass send) ["small"]
  #append bottom counter >> #append bottom cancel >> #append bottom send
  #append outer bottom
  popover <- GI.new Gtk.Popover [#child := outer, #position := Gtk.PositionTypeBottom, #hasArrow := False, #cssClasses := ["u-request-review-pop"]]
  Gtk.widgetSetParent popover anchor
  void $ GI.on anchor #destroy (Gtk.widgetUnparent popover)

  chosen <- newIORef []
  -- Each row with what it is matched against and how it redraws.
  rows <- newIORef []
  let already = [(r.who, r.stance) | r <- m.reviewers]
      nameOf w = case w of
        Person l -> unparse l
        Team t -> t
      count = do
        n <- length <$> readIORef chosen
        let more = if null already then "" else " more"
        GI.set send [ #label := if n == 0 then "Request" else "Request " <> T.pack (show n) <> more
                    , #sensitive := n > 0 ]
      footer typed shown = GI.set counter [#label := case () of
        _ | not (T.null typed) -> T.pack (show shown) <> (if shown == 1 then " match" else " matches")
          | null already -> ""
          | otherwise -> T.pack (show (length already)) <> " asked already"]
      clear = do
        writeIORef rows []
        Gtk.widgetGetFirstChild list >>= mapM_ (\c -> Gtk.boxRemove list c >> clear)
      message title text retry = do
        clear
        box <- vbox 6
        GI.set box [#cssClasses := ["message"] <> ["failed" | isJust retry]]
        label title ["message-title"] >>= #append box
        t <- label text ["muted", "small"]
        GI.set t [#wrap := True]
        #append box t
        forM_ retry $ \again -> do
          b <- Button.new Button.defaults {Button.label = Just "Try again"}
          Gtk.widgetAddCssClass b "small"
          GI.set b [#halign := Gtk.AlignStart]
          void $ GI.on b #clicked again
          #append box b
        #append list box
      loading = do
        clear
        forM_ [120, 90, 140] $ \w -> do
          shape <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["shape"]]
          Skeleton.bar 16 16 >>= #append shape
          Skeleton.circle 24 >>= #append shape
          Skeleton.bar w 10 >>= \b -> GI.set b [#valign := Gtk.AlignCenter] >> #append shape b
          #append list shape
      row c = do
        let fixed = lookup c.who already
            sub = case (c.who, c.changed) of
              (_, files@(_ : _)) -> "changed " <> T.intercalate ", " (take 2 files) <> maybe "" (\t -> " · " <> ago m.clock t) c.lastChanged
              _ -> c.about
            tag = case (fixed, c.members) of
              (Just st, _) -> Just (stanceTag st, "state")
              (_, Just n) -> Just (T.pack (show n) <> (if n == 1 then " person" else " people"), "plain")
              _ -> Nothing
        button <- GI.new Gtk.Button [#cssClasses := ["candidate"] <> ["fixed" | isJust fixed], #sensitive := not (isJust fixed)]
        content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
        check <- GI.new Gtk.Box [#valign := Gtk.AlignCenter, #cssClasses := ["check"]]
        mark <- Icon.new 11 Icon.Check
        GI.set mark [#visible := isJust fixed]
        #append check mark
        #append content check
        Avatar.new 24 (T.toUpper (T.take 2 (nameOf c.who))) >>= #append content
        texts <- vbox 0
        GI.set texts [#hexpand := True, #valign := Gtk.AlignCenter]
        name <- GI.new Gtk.Label [#xalign := 0, #useMarkup := True, #ellipsize := Pango.EllipsizeModeEnd, #cssClasses := ["name"]]
        #append texts name
        unless (T.null sub) $ do
          l <- label sub ["muted", "small"]
          GI.set l [#ellipsize := Pango.EllipsizeModeEnd]
          #append texts l
        #append content texts
        newTag <- label "new" ["tag", "new"]
        GI.set newTag [#visible := False]
        #append content newTag
        forM_ tag $ \(text, kind) -> label text ["tag", kind] >>= #append content
        GI.set button [#child := content]
        let draw typed = GI.set name [#label := marked typed (prefix c.who) (nameOf c.who)]
            toggle = do
              on' <- (c.who `elem`) <$> readIORef chosen
              modifyIORef chosen (\xs -> if on' then filter (/= c.who) xs else c.who : xs)
              GI.set mark [#visible := not on']
              GI.set newTag [#visible := not on']
              (if on' then Gtk.widgetRemoveCssClass else Gtk.widgetAddCssClass) button "chosen"
              count
        void $ GI.on button #clicked toggle
        draw ""
        pure (button, T.toLower (nameOf c.who <> " " <> c.about), draw)
      fill cs = do
        clear
        let mine = [c | c <- cs, notAuthor c.who]
            groups =
              [ ("Suggested · changed these files before", [c | c <- mine, isPerson c.who, not (null c.changed)])
              , ("Collaborators", [c | c <- mine, isPerson c.who, null c.changed])
              , ("Teams", [c | c <- mine, not (isPerson c.who)]) ]
        if null mine || all (\c -> isJust (lookup c.who already)) mine
          then message "Everyone who can review is asked already" "Add people to the repository on the forge to ask more." Nothing
          else forM_ groups $ \(heading, group) -> unless (null group) $ do
            h <- label (T.toUpper heading) ["lbl"]
            #append list h
            made <- forM group row
            forM_ made $ \(b, _, _) -> #append list b
            modifyIORef rows (<> [(h, made)])
        footer "" 0
      filterBy = do
        typed <- T.toLower . T.strip <$> GI.get search #text
        groups <- readIORef rows
        shown <- forM groups $ \(h, made) -> do
          hits <- forM made $ \(b, haystack, draw) -> do
            let hit = typed `T.isInfixOf` haystack
            Gtk.widgetSetVisible b hit
            draw typed
            pure hit
          Gtk.widgetSetVisible h (or hits)
          pure (length (filter id hits))
        footer typed (sum shown)
      load = do
        loading
        void $ forkIO $ do
          got <- actions.candidates
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            either (\text -> message "Could not load who can review" text (Just load)) fill got
            filterBy
            pure False
      open = do
        writeIORef chosen []
        count
        GI.set search [#text := ""]
        footer "" 0
        Gtk.popoverPopup popover
        void (Gtk.widgetGrabFocus search)
        load
      sendNow = do
        whos <- readIORef chosen
        unless (null whos) $
          Button.busy send [cancel] "Requesting…" (actions.requestReview whos) $ \case
            Left text -> message "Could not ask for the review" text Nothing
            Right () -> Gtk.popoverPopdown popover >> actions.refresh
  void $ GI.on search #searchChanged filterBy
  void $ GI.on cancel #clicked (Gtk.popoverPopdown popover)
  void $ GI.on send #clicked sendNow
  -- Before the rows, so Enter asks wherever the focus is.
  keys <- GI.new Gtk.ShortcutController [#propagationPhase := Gtk.PropagationPhaseCapture]
  shortcut keys "<Primary>Return" sendNow
  shortcut keys "Return" sendNow
  Gtk.widgetAddController popover keys
  -- Down from the search goes to the first row shown.
  down <- GI.new Gtk.ShortcutController []
  shortcut down "Down" $ do
    groups <- readIORef rows
    visible <- filterM Gtk.widgetGetVisible [b | (_, made) <- groups, (b, _, _) <- made]
    forM_ (take 1 visible) Gtk.widgetGrabFocus
  Gtk.widgetAddController search down
  pure open
  where
    notAuthor w = case w of
      Person l -> unparse l /= m.opening.author
      Team _ -> True
    isPerson w = case w of
      Person _ -> True
      Team _ -> False
    prefix w = case w of
      Person _ -> ""
      Team _ -> "<span foreground='#5B616B'>team:</span> "
    stanceTag st = case st of
      Waiting -> "requested"
      Approved -> "approved"
      ChangesRequested -> "changes"
      Commented -> "commented"
    -- The name with the typed text marked, as Pango markup.
    marked typed before name
      | T.null typed = before <> escape name
      | otherwise = case T.breakOn typed (T.toLower name) of
          (pre, hit) | not (T.null hit) ->
            let (a, rest) = T.splitAt (T.length pre) name
                (b, c) = T.splitAt (T.length typed) rest
            in before <> escape a <> "<span background='#FBF0D9' foreground='#1B1D21'>" <> escape b <> "</span>" <> escape c
          _ -> before <> escape name
    escape = T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

-- | A reviewer: who, where they stand, and a button that asks them again
-- once they reviewed, or stops asking while they have not.
reviewerRow :: (Gtk.Button -> Gtk.Box -> Text -> IO (Either Text ()) -> IO ()) -> Actions -> Reviewer -> IO Gtk.Box
reviewerRow run actions r = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["reviewer"]]
  let (name, short) = case r.who of
        Person l -> (unparse l, unparse l)
        Team t -> ("team: " <> t, t)
  Avatar.new 22 (T.toUpper (T.take 2 short)) >>= #append row
  texts <- vbox 0
  GI.set texts [#hexpand := True]
  label name ["name"] >>= #append texts
  label (stanceText r.stance <> (if r.stale then " · on an older commit" else "")) ["small", stanceClass r.stance]
    >>= #append texts
  #append row texts
  (caption, act, doing) <- pure $ case r.stance of
    Waiting -> ("Remove", actions.unrequestReview r.who, "Removing…")
    _ -> ("Ask again", actions.requestReview [r.who], "Asking…")
  button <- Button.new Button.defaults {Button.label = Just caption, Button.variant = Button.Flat}
  GI.set button [#valign := Gtk.AlignCenter]
  Gtk.widgetAddCssClass button "small"
  void $ GI.on button #clicked (run button row doing act)
  #append row button
  pure row
  where
    stanceText s = case s of
      Waiting -> "Waiting for a review"
      Approved -> "Approved"
      ChangesRequested -> "Requested changes"
      Commented -> "Commented"
    stanceClass s = case s of
      Waiting -> "waiting"
      Approved -> "approved"
      ChangesRequested -> "changes"
      Commented -> "muted"

vbox :: Int -> IO Gtk.Box
vbox gap = GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := fromIntegral gap]

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: Model
sample = Model
  { opening = Comment "abdivasiyev" "AA" "opened this 5 min ago"
      "Adds the GTK4 bindings so the next PR can open the main window.\n\n• gi-gtk4 in usta.cabal\n• Same dependency in the flake derivation"
  , comments = []
  , events =
      [ Committed "e1c90fa" "feat: add gi-gtk4 dependency"
      , CheckFinished True "nix build"
      , ReviewRequestedFrom "abdivasiyev"
      ]
  , threads = []
  , merge = Just MergeState {checksPassed = Just True, checksRunning = False, readiness = Nothing}
  , closed = False
  , source = "feat/gtk4-dep"
  , target = "master"
  , reviewers = []
  , labels = [(Accent, "gtk"), (Neutral, "build")]
  , linkedIssues = ["#5 Render the main window with GTK4"]
  , clock = Clock {now = UTCTime (fromGregorian 2026 1 1) 0, zone = utc}
  }

css :: Text
css = mconcat
  [ "box.u-conversation box.merge-row.state { border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-conversation label.state-title { font-weight: 600; }"
  , "box.u-conversation label.reason { font-size: 12px; }"
  , "box.u-conversation button.link-button { min-height: 0; padding: 0 2px; border: none; box-shadow: none; background: transparent; color: var(--accent); font-size: 13px; }"
  , "popover.u-request-review-pop > contents { padding: 0; border-radius: 12px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: 0 12px 32px rgba(27,29,33,0.16); }"
  , "box.u-request-review box.top { padding: 14px 14px 8px; }"
  , "box.u-request-review label.picker-title { font-size: 15px; font-weight: 600; color: var(--text); }"
  , "box.u-request-review entry.search, box.u-request-review searchbar entry { min-height: 34px; border-radius: 8px; }"
  , "box.u-request-review entry.search:focus-within { box-shadow: 0 0 0 2px var(--accent); }"
  , "box.u-request-review label.lbl { margin: 8px 8px 2px; font-size: 11px; font-weight: 600; color: var(--text-muted); }"
  , "box.u-request-review button.candidate { min-height: 40px; padding: 4px 8px; border: none; border-radius: 6px; box-shadow: none; background: transparent; background-image: none; color: var(--text); }"
  , "box.u-request-review button.candidate:hover { background: var(--border-subtle); }"
  , "box.u-request-review button.candidate:focus-visible { box-shadow: inset 0 0 0 2px var(--accent); }"
  , "box.u-request-review button.candidate.chosen { background: var(--accent-bg); }"
  , "box.u-request-review button.candidate.fixed { opacity: 0.85; }"
  , "box.u-request-review box.check { min-width: 16px; min-height: 16px; border-radius: 4px; border: 1.5px solid var(--input-border); background: var(--surface); color: #FFFFFF; }"
  , "box.u-request-review button.chosen box.check { border-color: var(--accent); background: var(--accent); }"
  , "box.u-request-review button.fixed box.check { border-color: var(--border); background: var(--border); }"
  , "box.u-request-review label.name { font-weight: 500; }"
  , "box.u-request-review label.small { font-size: 12px; }"
  , "box.u-request-review label.tag { padding: 1px 8px; border-radius: 10px; font-size: 12px; font-weight: 500; }"
  , "box.u-request-review label.tag.new { background: var(--accent-bg); color: var(--accent-soft); }"
  , "box.u-request-review label.tag.state { background: var(--border-subtle); color: var(--text-muted); }"
  , "box.u-request-review label.tag.plain { color: var(--text-muted); font-weight: 400; }"
  , "box.u-request-review box.shape { margin: 6px 8px; }"
  , "box.u-request-review box.message { margin: 8px 6px 6px; }"
  , "box.u-request-review box.message label.message-title { font-weight: 600; color: var(--text); }"
  , "box.u-request-review box.bottom { padding: 10px 14px; border-top: 1px solid var(--border-subtle); background: var(--window); border-radius: 0 0 12px 12px; }"
  , "box.u-request-review button.small { min-height: 28px; padding: 0 10px; font-size: 13px; border-radius: 6px; }"
  , "box.u-conversation box.reviewer label.name { font-weight: 500; }"
  , "box.u-conversation box.reviewer label.small { font-size: 12px; }"
  , "box.u-conversation box.reviewer label.waiting { color: var(--warning); }"
  , "box.u-conversation box.reviewer label.approved { color: var(--success); }"
  , "box.u-conversation box.reviewer label.changes { color: var(--danger); }"
  , "box.u-conversation box.reviewer button.small { min-height: 26px; padding: 0 8px; font-size: 12px; }"
  , "box.u-conversation box.main { padding: 20px 32px; }"
  , "box.u-conversation box.details { padding: 20px 16px; }"
  , "box.u-conversation label.strong { font-weight: 600; }"
  , "box.u-conversation label.link { color: var(--accent); }"
  , "box.u-conversation label.problem { color: var(--danger); font-size: 13px; }"
  , "box.u-conversation label.hash { color: var(--accent); }"
  , "box.u-conversation box.card-header {"
  , "  padding: 10px 14px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: var(--window); border-radius: 8px 8px 0 0; font-size: 13px;"
  , "}"
  , "box.u-conversation box.card-header.plain { background: transparent; }"
  , "box.u-conversation .card-body { padding: 14px 16px; }"
  , "box.u-conversation box.timeline {"
  , "  margin-left: 12px; padding-left: 14px; border-left: 2px solid var(--border); font-size: 13px;"
  , "}"
  , "box.u-conversation label.diff-add {"
  , "  padding: 6px 14px; background: var(--success-bg); color: var(--success);"
  , "}"
  , "box.u-conversation box.merge-row { padding: 12px 16px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-conversation box.merge-row:last-child { border-bottom: none; }"
  , "box.u-conversation dropdown > button {"
  , "  min-height: 36px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
  , "}"
  , "box.u-conversation textview.editor {"
  , "  border: 1px solid var(--input-border); border-radius: 8px;"
  , "}"
  , "box.u-conversation textview.editor text { background: var(--surface); color: var(--text); }"
  , "box.u-conversation label.chip {"
  , "  background: var(--neutral-bg); color: var(--text); padding: 1px 6px; border-radius: 4px;"
  , "  font-size: 12px;"
  , "}"
  ]
