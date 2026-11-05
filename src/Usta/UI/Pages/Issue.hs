{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | One issue: its thread, timeline and a reply box, with assignees,
-- labels, milestone and linked pull requests on the side.
module Usta.UI.Pages.Issue
  ( Comment (..)
  , Talk (..)
  , commentWidget
  , conversation
  , editableTitle
  , Watch (..)
  , watchButton
  , centred
  , Event (..)
  , Model (..)
  , Actions (..)
  , fromDiscussion
  , new
  , thread
  , css
  ) where

import Control.Monad (forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Control.Concurrent (forkIO)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.AssignPickers as AssignPickers
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Attach as Attach
import qualified Usta.UI.Components.CommentCard as CommentCard
import qualified Usta.UI.Components.TimelineRow as TimelineRow
import qualified Usta.UI.Components.LabelPicker as LabelPicker
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import Usta.UI.Components.CommandPalette (shortcut)
import Usta.UI.Format (Clock, ago)
import Usta.UI.Pages.Issues (labelTone)
import Usta.UI.Style (Tone (..))
import Usta.Types.Discussion (Feeling, Watching (..))
import qualified Usta.Types.Timeline as Tl
import qualified Usta.Types.Discussion as D
import qualified Usta.Types.Label as L
import Usta.Types.Milestone (Milestone)
import Usta.Types.Parse (unparse)

type Comment :: Type
data Comment = Comment
  { author :: Text
  , initials :: Text
  , when' :: Text
    -- ^ E.g. "opened 1 day ago" or "commented 1 hour ago".
  , body :: Text
  , forge :: Maybe (Text, D.Comment)
    -- ^ The comment as the forge has it, with its verb ("commented"), to
    -- edit, delete and react to; 'Nothing' for a sample.
  }

-- | What comments can do on a page: the user, to tell their own, and the
-- forge's edits, deletes and reactions by the comment's id ('Nothing'
-- for the description).
type Talk :: Type
data Talk = Talk
  { me :: Text
  , clock :: Clock
  , editComment :: Maybe Int64 -> Text -> IO (Either Text ())
  , deleteComment :: Int64 -> IO (Either Text ())
  , react :: Maybe Int64 -> Feeling -> Bool -> IO (Either Text ())
  , refresh :: IO ()
  , upload :: Maybe Attach.Uploader
    -- ^ Where files attached to its comments go.
  , draftKey :: Text
    -- ^ Where its unsent comments are kept, e.g. "host/owner/repo#12".
  , place :: Maybe Avatar.Place
    -- ^ The forge the authors' pictures come from.
  }

-- | A comment as a card that edits, deletes and reacts when the forge's
-- comment is known; Quote reply puts the quote into @reply@. @opener@ is
-- who opened the issue or pull request, marked Author on their comments.
commentWidget :: Talk -> Gtk.TextBuffer -> Text -> Comment -> IO Gtk.Widget
commentWidget t reply opener c = case c.forge of
  Nothing -> commentCard c >>= Gtk.toWidget
  Just (verb, x) -> CommentCard.new CommentCard.Card
    { comment = x
    , clock = t.clock
    , verb = verb
    , mine = unparse x.author == t.me
    , badge = if unparse x.author == opener && x.id /= Nothing then Just "Author" else Nothing
    , upload = t.upload
    -- A review's text is edited on the forge's page.
    , edit = if x.review == Nothing then t.editComment x.id else const (pure (Left "Edit a review on the pull request's page on the forge."))
    , delete = if x.review == Nothing then t.deleteComment <$> x.id else Nothing
    , quote = \q -> do
        end <- Gtk.textBufferGetEndIter reply
        Gtk.textBufferInsert reply end (q <> "\n") (-1)
    , react = if x.review == Nothing then t.react x.id else \_ _ -> pure (Left "React to a review on the pull request's page on the forge.")
    , changed = t.refresh
    , place = t.place
    } >>= Gtk.toWidget

-- | How much of a thread reaches the inbox, read and set on the forge.
type Watch :: Type
data Watch = Watch
  { current :: IO (Either Text Watching)
  , set :: Watching -> IO (Either Text ())
  }

-- | The header's Subscribed button: a menu of Subscribed, Participating
-- and Muted, the current one ticked. It reads the forge when made; a
-- choice the forge refuses says why in the menu.
watchButton :: Watch -> IO Gtk.Widget
watchButton w = do
  caption <- GI.new Gtk.Label [#label := "Subscribed"]
  inner <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  GI.new Gtk.Label [#label := "🔔"] >>= #append inner
  #append inner caption
  items <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #widthRequest := 280]
  popover <- GI.new Gtk.Popover [#child := items, #hasArrow := False, #cssClasses := ["u-watch-menu"]]
  button <- GI.new Gtk.MenuButton [#child := inner, #popover := popover, #sensitive := False, #cssClasses := ["u-watch"]]
  problem <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #visible := False, #maxWidthChars := 34, #cssClasses := ["problem"]]
  let choices =
        [ (Subscribed, "Subscribed", "Every comment and change")
        , (Participating, "Participating", "Only when you are mentioned or take part")
        , (Muted, "Muted", "Nothing from this thread, ever") ]
      draw now = do
        GI.set caption [#label := maybe "Subscribed" (\(_, t, _) -> t) (lookup' now)]
        clear
        forM_ choices $ \(value, title, sub) -> do
          row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
          GI.new Gtk.Label [#label := if value == now then "✓" else "", #widthRequest := 14, #cssClasses := ["tick"]] >>= #append row
          texts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #hexpand := True]
          GI.new Gtk.Label [#label := title, #xalign := 0, #cssClasses := ["title"]] >>= #append texts
          GI.new Gtk.Label [#label := sub, #xalign := 0, #wrap := True, #cssClasses := ["muted", "small"]] >>= #append texts
          #append row texts
          b <- GI.new Gtk.Button [#child := row, #cssClasses := ["choice"] <> ["on" | value == now]]
          void $ GI.on b #clicked $ unless (value == now) $ do
            GI.set problem [#visible := False]
            GI.set button [#sensitive := False]
            void $ forkIO $ do
              result <- w.set value
              void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                GI.set button [#sensitive := True]
                case result of
                  Right () -> GI.set problem [#visible := False] >> Gtk.popoverPopdown popover >> draw value
                  Left e -> GI.set problem [#label := e, #visible := True]
                pure False
          #append items b
        #append items problem
      lookup' now = case [c | c@(v, _, _) <- choices, v == now] of
        c : _ -> Just c
        [] -> Nothing
      clear = Gtk.widgetGetFirstChild items >>= mapM_ (\c -> Gtk.boxRemove items c >> clear)
  void $ forkIO $ do
    got <- w.current
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      case got of
        Right now -> GI.set button [#sensitive := True] >> draw now
        Left e -> GI.set button [#tooltipText := e]
      pure False
  Gtk.toWidget button

-- | A title with a pencil that edits it in place: Enter or Save renames,
-- Esc or Cancel keeps it.
-- | Centres a header's buttons on its row, so they keep their size when
-- the title's editor makes the row taller.
centred :: Gtk.Box -> IO ()
centred bar = go =<< Gtk.widgetGetFirstChild bar
  where
    go = mapM_ (\w -> GI.set w [#valign := Gtk.AlignCenter] >> Gtk.widgetGetNextSibling w >>= go)

editableTitle :: Text -> [Text] -> (Text -> IO (Either Text ())) -> IO Gtk.Box
editableTitle start classes rename = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6, #cssClasses := ["u-title-edit"]]
  shown <- GI.new Gtk.Label [#label := start, #xalign := 0, #ellipsize := Pango.EllipsizeModeEnd, #cssClasses := classes]
  pencil <- GI.new Gtk.Button [#iconName := "document-edit-symbolic", #tooltipText := "Edit the title", #cssClasses := ["pencil"]]
  entry <- GI.new Gtk.Entry [#text := start, #hexpand := True, #visible := False]
  cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
  save <- Button.new Button.defaults {Button.label = Just "Save", Button.variant = Button.Primary}
  forM_ [cancel, save] $ \b -> GI.set b [#visible := False]
  mapM_ (#append box) =<< sequence [Gtk.toWidget shown, Gtk.toWidget pencil, Gtk.toWidget entry, Gtk.toWidget cancel, Gtk.toWidget save]
  let editing on' = do
        GI.set shown [#visible := not on']
        GI.set pencil [#visible := not on']
        forM_ [cancel, save] $ \b -> GI.set b [#visible := on']
        GI.set entry [#visible := on']
        when on' $ do
          now <- GI.get shown #label
          GI.set entry [#text := now, #tooltipText := ""]
          void (Gtk.widgetGrabFocus entry)
      commit = do
        typed <- T.strip <$> GI.get entry #text
        old <- GI.get shown #label
        if T.null typed || typed == old then editing False else
          Button.busy save [cancel] "Saving…" (rename typed) $ \case
            Left e -> GI.set entry [#tooltipText := e] >> Gtk.widgetAddCssClass entry "error"
            Right () -> GI.set shown [#label := typed] >> Gtk.widgetRemoveCssClass entry "error" >> editing False
  void $ GI.on pencil #clicked (editing True)
  void $ GI.on cancel #clicked (editing False)
  void $ GI.on save #clicked commit
  void $ GI.on entry #activate commit
  keys <- GI.new Gtk.ShortcutController []
  shortcut keys "Escape" (editing False)
  Gtk.widgetAddController entry keys
  pure box

-- | The description, then the comments and the timeline's events by
-- time, appended to @into@. Comments without a time (samples) keep their
-- place before the events.
conversation :: Talk -> Gtk.TextBuffer -> Text -> [Comment] -> [Tl.Event] -> Gtk.Box -> IO ()
conversation t reply opener comments events into = do
  let (first, rest) = splitAt 1 comments
      timed = [(maybe Nothing (Just . (.createdAt) . snd) c.forge, Left c) | c <- rest]
        <> [(Just e.at, Right e) | e <- events]
  forM_ first $ \c -> commentWidget t reply opener c >>= #append into
  forM_ (sortOn fst timed) $ \(_, item) -> case item of
    Left c -> commentWidget t reply opener c >>= #append into
    Right e -> TimelineRow.new t.clock e >>= #append into

-- | Something that happened on the issue besides comments.
type Event :: Type
data Event
  = LabelAdded Tone Text
  | ReferencedBy Text
    -- ^ A pull request that mentions the issue, e.g. "#6 feat: ...".

type Model :: Type
data Model = Model
  { forge :: Forge.Kind
  , host :: Text
  , repo :: Text
  , number :: Int
  , title :: Text
  , closed :: Bool
  , comments :: [Comment]
  , events :: [Event]
  , timeline :: [Tl.Event]
    -- ^ What happened besides comments, set between them by time.
  , assignees :: [(Text, Text)]
    -- ^ Initials and name.
  , labels :: [(Tone, Text)]
  , milestone :: Maybe Text
  , linkedPullRequests :: [Text]
  }

-- | What the reply box does. Both run off the main thread; a 'Left' is
-- said under the box, and a 'Right' runs 'refresh'.
type Actions :: Type
data Actions = Actions
  { comment :: Text -> IO (Either Text ())
  , setClosed :: Bool -> IO (Either Text ())
  , labels :: [Text] -> LabelPicker.Picker
    -- ^ The labels picker, from the labels it has.
  , talk :: Talk
  , assigning :: AssignPickers.Assignees
  , milestoning :: AssignPickers.Milestones
  , milestoneNow :: Maybe Milestone
    -- ^ Its milestone in full, for the progress.
  , refresh :: IO ()
    -- ^ Loads the page again, with the change.
  }

-- | The issue as the forge has it, over what the list knew.
fromDiscussion :: Clock -> D.Discussion -> Model -> Model
fromDiscussion c d m = m
  { closed = d.closed
  , comments = said "opened " d.opening : [said (verbOf x.review) x | x <- d.comments]
  , events = []
  , assignees = [(initials (unparse login), unparse login) | login <- d.assignees]
  , labels = [(labelTone l.color, l.name) | l <- d.labels]
  , milestone = d.milestone
  , linkedPullRequests = []
  , timeline = m.timeline
  }
  where
    said verb x = Comment
      { author = unparse x.author
      , initials = initials (unparse x.author)
      , when' = verb <> ago c x.createdAt
      , body = x.body
      , forge = Just (T.strip verb, x)
      }
    initials = T.toUpper . T.take 2
    verbOf r = case r of
      Just D.Approve -> "approved "
      Just D.RequestChanges -> "requested changes "
      Just D.CommentOnly -> "reviewed "
      Nothing -> "commented "

-- | The header over @content@, which is the 'thread' once it is loaded.
-- @content@ gets what sets the header's Open or Closed badge, for when the
-- forge says otherwise than the list did. @backLabel@ names where Back
-- goes, e.g. "Issues" or "Inbox".
new :: Text -> IO () -> (Text -> IO (Either Text ())) -> Watch -> Model -> ((Bool -> IO ()) -> IO Gtk.Widget) -> IO Gtk.Widget
new backLabel onBack rename watch m content = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #hexpand := True
                         , #vexpand := True
                         , #cssClasses := ["u-issue"]
                         ]
  (bar, showClosed) <- header m backLabel onBack rename watch
  #append page bar
  content showClosed >>= #append page
  Gtk.toWidget page

-- | The comments, the reply box and the side details.
thread :: Actions -> Model -> IO Gtk.Widget
thread actions m = do
  columns <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #vexpand := True
                            , #cssClasses := ["u-issue"]
                            ]
  main <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 16
                         , #cssClasses := ["main"]
                         ]
  -- The reply box comes first, so Quote reply has somewhere to go.
  (reply, buffer) <- replyBox actions m
  let opener = maybe "" (.author) (listToMaybe m.comments)
  conversation actions.talk buffer opener m.comments m.timeline main
  timeline <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #spacing := 10
                             , #cssClasses := ["timeline"]
                             ]
  forM_ m.events $ \e -> eventRow e >>= #append timeline
  #append main timeline
  #append main reply
  scroll <- GI.new Gtk.ScrolledWindow [ #child := main
                                      , #hexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]
  #append columns scroll
  details actions m >>= #append columns
  Gtk.toWidget columns

header :: Model -> Text -> IO () -> (Text -> IO (Either Text ())) -> Watch -> IO (Gtk.Box, Bool -> IO ())
header m backLabel onBack rename watch = do
  bar <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 12
                        , #cssClasses := ["issue-header"]
                        ]
  back <- Button.new Button.defaults
    { Button.label = Just backLabel
    , Button.icon = Just "go-previous-symbolic"
    , Button.variant = Button.Flat
    }
  void $ GI.on back #clicked onBack
  #append bar back

  titles <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 2
                           , #hexpand := True
                           , #valign := Gtk.AlignCenter
                           ]
  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Forge.smallDot m.forge >>= #append meta
  label (m.host <> "/" <> m.repo) ["meta", "mono"] >>= #append meta
  label ("#" <> T.pack (show m.number)) ["meta"] >>= #append meta
  state <- GI.new Gtk.Box []
  let showClosed closed = do
        old <- Gtk.widgetGetFirstChild state
        forM_ old (Gtk.boxRemove state)
        (if closed then Badge.new Merged "Closed" else Badge.new Success "Open") >>= #append state
  showClosed m.closed
  #append meta state
  #append titles meta
  editableTitle m.title ["issue-title"] rename >>= #append titles
  #append bar titles

  watchButton watch >>= #append bar
  browser <- Button.new Button.defaults { Button.label = Just "Open in browser" }
  void $ GI.on browser #clicked $ do
    launcher <- Gtk.uriLauncherNew (Just url)
    Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
  #append bar browser
  centred bar
  pure (bar, showClosed)
  where
    issues = if m.forge == Forge.GitLab then "/-/issues/" else "/issues/"
    url = "https://" <> m.host <> "/" <> m.repo <> issues <> T.pack (show m.number)

commentCard :: Comment -> IO Gtk.Box
commentCard c = do
  card <- Card.new
  top <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 10
                        , #cssClasses := ["card-header"]
                        ]
  Avatar.new 24 c.initials >>= #append top
  label c.author ["strong"] >>= #append top
  label c.when' ["muted"] >>= #append top
  #append card top
  text <- if T.null (T.strip c.body)
    then label "No description." ["card-body", "muted"] >>= Gtk.toWidget
    else Markdown.new c.body
  Gtk.widgetAddCssClass text "card-body"
  #append card text
  pure card

eventRow :: Event -> IO Gtk.Box
eventRow e = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  case e of
    LabelAdded tone name -> do
      Badge.new tone name >>= #append row
      label "label added" ["soft"] >>= #append row
    ReferencedBy pr -> do
      Icon.toned 14 Success Icon.PullRequest >>= #append row
      label "Referenced in" ["soft"] >>= #append row
      label pr ["link"] >>= #append row
  pure row

replyBox :: Actions -> Model -> IO (Gtk.Box, Gtk.TextBuffer)
replyBox actions m = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
  (editor, buffer) <- MarkdownEditor.new MarkdownEditor.defaults
    { MarkdownEditor.hint = Just "Leave a comment. Markdown supported; paste or drop files to attach them.", MarkdownEditor.upload = actions.talk.upload
    , MarkdownEditor.draft = Just (MarkdownEditor.draftAt (actions.talk.draftKey <> "/comment")) }
  #append box editor
  problem <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #visible := False, #cssClasses := ["problem"]]
  buttons <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 8
                            , #halign := Gtk.AlignEnd
                            ]
  close <- Button.new Button.defaults
    { Button.label = Just (if m.closed then "Reopen issue" else "Close issue") }
  #append buttons close
  send <- Button.new Button.defaults { Button.label = Just "Comment", Button.variant = Button.Primary }
  #append buttons send
  MarkdownEditor.sendOn editor True (void (Gtk.widgetActivate send))
  -- The buttons wait for the forge, so a second click sends nothing twice.
  let run button doing act = do
        GI.set problem [#visible := False]
        Button.busy button [buttons] doing act $
          either (\text -> GI.set problem [#label := text, #visible := True]) (const actions.refresh)
  void $ GI.on send #clicked $ do
    text <- maybe "" id <$> GI.get buffer #text
    -- Sent, the draft is done with.
    let sent r = r <$ either (const (pure ())) (const ((MarkdownEditor.draftAt (actions.talk.draftKey <> "/comment")).keep "")) r
    unless (T.null (T.strip text)) $ run send "Commenting…" (actions.comment text >>= sent)
  void $ GI.on close #clicked $ run close (if m.closed then "Reopening…" else "Closing…") (actions.setClosed (not m.closed))
  #append box problem
  #append box buttons
  pure (box, buffer)

details :: Actions -> Model -> IO Gtk.Box
details actions m = do
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 20
                         , #widthRequest := 260
                         , #hexpand := False
                         , #cssClasses := ["details"]
                         ]
  (people, openPeople) <- AssignPickers.assigneesSection actions.assigning
  #append side people
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
  (goal, openGoal) <- AssignPickers.milestoneSection actions.milestoning actions.milestoneNow
  #append side goal
  -- Every key before GTK takes the controller: it owns it after.
  labelKeys <- GI.new Gtk.ShortcutController [#scope := Gtk.ShortcutScopeManaged]
  shortcut labelKeys "l" openLabels
  shortcut labelKeys "a" openPeople
  shortcut labelKeys "m" openGoal
  Gtk.widgetAddController side labelKeys
  unless (null m.linkedPullRequests) $
    section side "Linked pull requests" $ \box ->
      forM_ m.linkedPullRequests $ \pr -> label pr ["link"] >>= #append box
  pure side
  where
    section :: Gtk.Box -> Text -> (Gtk.Box -> IO ()) -> IO ()
    section side title' fill = do
      box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
      Card.sectionTitle title' >>= #append box
      fill box
      #append side box

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "menubutton.u-watch > button { min-height: 40px; padding: 0 14px; border-radius: 8px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: none; background-image: none; }"
  , "popover.u-watch-menu > contents { padding: 6px; border-radius: 10px; border: 1px solid var(--input-border); background: var(--surface); box-shadow: 0 8px 24px rgba(27,29,33,0.14); }"
  , "popover.u-watch-menu button.choice { padding: 8px 10px; border: none; border-radius: 6px; box-shadow: none; background: transparent; background-image: none; }"
  , "popover.u-watch-menu button.choice:hover { background: var(--border-subtle); }"
  , "popover.u-watch-menu button.choice.on { background: var(--accent-bg); }"
  , "popover.u-watch-menu label.title { font-weight: 600; color: var(--text); }"
  , "popover.u-watch-menu label.tick { color: var(--accent); font-weight: 700; }"
  , "popover.u-watch-menu label.small { font-size: 12px; }"
  , "popover.u-watch-menu label.problem { margin: 6px 10px 2px; color: var(--danger); font-size: 12px; }"
  , "box.u-title-edit button.pencil { min-height: 24px; min-width: 24px; padding: 0; border: none; box-shadow: none; background: transparent; color: var(--text-muted); }"
  , "box.u-title-edit button.pencil:hover { color: var(--text); }"
  , "box.u-issue button.link-button { min-height: 0; padding: 0 2px; border: none; box-shadow: none; background: transparent; color: var(--accent); font-size: 13px; }"
  , "box.u-issue box.issue-header {"
  , "  padding: 8px 16px; background: var(--surface); border-bottom: 1px solid var(--border);"
  , "}"
  , "box.u-issue label.meta { font-size: 12px; color: var(--text-muted); }"
  , "box.u-issue label.issue-title { font-size: 16px; font-weight: 600; }"
  , "box.u-issue box.main { padding: 20px 32px; }"
  , "box.u-issue box.details { padding: 20px 16px; font-size: 13px; }"
  , "box.u-issue label.strong { font-weight: 600; }"
  , "box.u-issue label.link { color: var(--accent); }"
  , "box.u-issue label.problem { color: var(--danger); font-size: 13px; }"
  , "box.u-issue box.card-header {"
  , "  padding: 10px 14px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: var(--window); border-radius: 8px 8px 0 0; font-size: 13px;"
  , "}"
  , "box.u-issue .card-body { padding: 14px 16px; }"
  , "box.u-issue box.timeline {"
  , "  margin-left: 12px; padding-left: 14px; border-left: 2px solid var(--border); font-size: 13px;"
  , "}"
  ]
