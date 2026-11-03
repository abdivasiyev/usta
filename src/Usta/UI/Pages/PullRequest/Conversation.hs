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

import Control.Monad (forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Thread as Thread
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Markdown as Markdown
import Usta.UI.Format (Clock, ago)
import Usta.UI.Pages.Issue (Comment (..))
import Usta.UI.Pages.Issues (labelTone)
import Usta.UI.Style (Tone (..))
import qualified Usta.Types.Commit as C
import qualified Usta.Types.Discussion as D
import qualified Usta.Types.Label as L
import Usta.Types.Parse (unparse)

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
newtype MergeState = MergeState
  { checksPassed :: Maybe Bool
    -- ^ 'Nothing' when the repository runs no checks.
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
  , reviewers :: [(Tone, Text)]
  , labels :: [(Tone, Text)]
  , linkedIssues :: [Text]
  }

-- | What the merge box and the comment box do. Each runs off the main
-- thread; a 'Left' is said under the box, and a 'Right' runs 'refresh'.
type Actions :: Type
data Actions = Actions
  { comment :: Text -> IO (Either Text ())
  , setClosed :: Bool -> IO (Either Text ())
  , merge :: D.Merge -> Bool -> IO (Either Text ())
    -- ^ 'True' deletes the branch after.
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
  , merge = if d.closed then Nothing else Just MergeState {checksPassed = checks}
  , closed = d.closed && not d.merged
  , source = from
  , target = into
  , reviewers = []
  , labels = [(labelTone l.color, l.name) | l <- d.labels]
  , linkedIssues = []
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
  forM_ m.merge $ \s -> mergeBox run actions s >>= #append main
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
  details m >>= #append columns
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
mergeBox :: (Gtk.Button -> Gtk.Box -> Text -> IO (Either Text ()) -> IO ()) -> Actions -> MergeState -> IO Gtk.Box
mergeBox run actions s = do
  card <- Card.new
  Gtk.widgetAddCssClass card "merge-box"
  forM_ s.checksPassed $ \passed -> do
    status <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #cssClasses := ["merge-row"]]
    if passed
      then do
        Icon.toned 16 Success Icon.Check >>= #append status
        label "All checks have passed" [] >>= #append status
      else do
        Icon.toned 16 Danger Icon.Cross >>= #append status
        label "Some checks failed" [] >>= #append status
    #append card status
  buttons <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["merge-row"]]
  -- In the order of [minBound ..]: Squash, MergeCommit, Rebase.
  how <- Gtk.dropDownNewFromStrings ["Squash and merge", "Create a merge commit", "Rebase and merge"]
  #append buttons how
  delete <- GI.new Gtk.CheckButton [#label := "Delete branch after merge", #active := True]
  #append buttons delete
  mergeButton <- Button.new Button.defaults { Button.label = Just "Merge", Button.variant = Button.Primary }
  -- The forge decides the rest (approvals, protected branches) and says so.
  GI.set mergeButton [ #hexpand := True
                     , #halign := Gtk.AlignEnd
                     , #sensitive := s.checksPassed /= Just False
                     ]
  void $ GI.on mergeButton #clicked $ do
    chosen <- Gtk.dropDownGetSelected how
    deleting <- GI.get delete #active
    let ways = [minBound .. maxBound] :: [D.Merge]
    when (fromIntegral chosen < length ways) $
      run mergeButton buttons "Merging…" (actions.merge (ways !! fromIntegral chosen) deleting)
  #append buttons mergeButton
  #append card buttons
  pure card

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

details :: Model -> IO Gtk.Box
details m = do
  side <- vbox 20
  GI.set side [#widthRequest := 260, #hexpand := False, #cssClasses := ["details"]]

  section side "Branch" $ \box -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    label m.source ["chip", "mono"] >>= #append row
    label "→" ["muted"] >>= #append row
    label m.target ["chip", "mono"] >>= #append row
    #append box row
  -- Sections usta has nothing for are left out.
  unless (null m.reviewers) $ section side "Reviewers" $ \box ->
    forM_ m.reviewers $ \(tone, name) -> do
      row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
      Badge.dot tone >>= #append row
      label name [] >>= #append row
      #append box row
  section side "Labels" $ \box -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    forM_ m.labels $ \(tone, name) -> Badge.new tone name >>= #append row
    when (null m.labels) $ label "None" ["muted"] >>= #append row
    #append box row
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
  , merge = Just MergeState {checksPassed = Just True}
  , closed = False
  , source = "feat/gtk4-dep"
  , target = "master"
  , reviewers = [(Warning, "abdivasiyev")]
  , labels = [(Accent, "gtk"), (Neutral, "build")]
  , linkedIssues = ["#5 Render the main window with GTK4"]
  }

css :: Text
css = mconcat
  [ "box.u-conversation box.main { padding: 20px 32px; }"
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
