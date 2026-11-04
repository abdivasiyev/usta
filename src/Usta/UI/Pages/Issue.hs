{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | One issue: its thread, timeline and a reply box, with assignees,
-- labels, milestone and linked pull requests on the side.
module Usta.UI.Pages.Issue
  ( Comment (..)
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
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gio as Gio
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.LabelPicker as LabelPicker
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import Usta.UI.Components.CommandPalette (shortcut)
import Usta.UI.Format (Clock, ago)
import Usta.UI.Pages.Issues (labelTone)
import Usta.UI.Style (Tone (..))
import qualified Usta.Types.Discussion as D
import qualified Usta.Types.Label as L
import Usta.Types.Parse (unparse)

type Comment :: Type
data Comment = Comment
  { author :: Text
  , initials :: Text
  , when' :: Text
    -- ^ E.g. "opened 1 day ago" or "commented 1 hour ago".
  , body :: Text
  }

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
  , refresh :: IO ()
    -- ^ Loads the page again, with the change.
  }

-- | The issue as the forge has it, over what the list knew.
fromDiscussion :: Clock -> D.Discussion -> Model -> Model
fromDiscussion c d m = m
  { closed = d.closed
  , comments = said "opened " d.opening : map (said "commented ") d.comments
  , events = []
  , assignees = [(initials (unparse login), unparse login) | login <- d.assignees]
  , labels = [(labelTone l.color, l.name) | l <- d.labels]
  , milestone = d.milestone
  , linkedPullRequests = []
  }
  where
    said verb x = Comment
      { author = unparse x.author
      , initials = initials (unparse x.author)
      , when' = verb <> ago c x.createdAt
      , body = x.body
      }
    initials = T.toUpper . T.take 2

-- | The header over @content@, which is the 'thread' once it is loaded.
-- @content@ gets what sets the header's Open or Closed badge, for when the
-- forge says otherwise than the list did. @backLabel@ names where Back
-- goes, e.g. "Issues" or "Inbox".
new :: Text -> IO () -> Model -> ((Bool -> IO ()) -> IO Gtk.Widget) -> IO Gtk.Widget
new backLabel onBack m content = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #hexpand := True
                         , #vexpand := True
                         , #cssClasses := ["u-issue"]
                         ]
  (bar, showClosed) <- header m backLabel onBack
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
  forM_ m.comments $ \c -> commentCard c >>= #append main
  timeline <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #spacing := 10
                             , #cssClasses := ["timeline"]
                             ]
  forM_ m.events $ \e -> eventRow e >>= #append timeline
  #append main timeline
  replyBox actions m >>= #append main
  scroll <- GI.new Gtk.ScrolledWindow [ #child := main
                                      , #hexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]
  #append columns scroll
  details actions m >>= #append columns
  Gtk.toWidget columns

header :: Model -> Text -> IO () -> IO (Gtk.Box, Bool -> IO ())
header m backLabel onBack = do
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
  titleLabel <- label m.title ["issue-title"]
  GI.set titleLabel [#ellipsize := Pango.EllipsizeModeEnd]
  #append titles titleLabel
  #append bar titles

  browser <- Button.new Button.defaults { Button.label = Just "Open in browser" }
  void $ GI.on browser #clicked $ do
    launcher <- Gtk.uriLauncherNew (Just url)
    Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing
  #append bar browser
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

replyBox :: Actions -> Model -> IO Gtk.Box
replyBox actions m = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
  (editor, buffer) <- MarkdownEditor.new MarkdownEditor.defaults
    { MarkdownEditor.hint = Just "Leave a comment. Markdown supported." }
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
  -- The buttons wait for the forge, so a second click sends nothing twice.
  let run button doing act = do
        GI.set problem [#visible := False]
        Button.busy button [buttons] doing act $
          either (\text -> GI.set problem [#label := text, #visible := True]) (const actions.refresh)
  void $ GI.on send #clicked $ do
    text <- maybe "" id <$> GI.get buffer #text
    unless (T.null (T.strip text)) $ run send "Commenting…" (actions.comment text)
  void $ GI.on close #clicked $ run close (if m.closed then "Reopening…" else "Closing…") (actions.setClosed (not m.closed))
  #append box problem
  #append box buttons
  pure box

details :: Actions -> Model -> IO Gtk.Box
details actions m = do
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 20
                         , #widthRequest := 260
                         , #hexpand := False
                         , #cssClasses := ["details"]
                         ]
  section side "Assignees" $ \box -> do
    when (null m.assignees) $ label "No one" ["muted"] >>= #append box
    forM_ m.assignees $ \(short, name) -> do
      row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
      Avatar.new 22 short >>= #append row
      label name [] >>= #append row
      #append box row
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
  Gtk.widgetAddController side labelKeys
  section side "Milestone" $ \box ->
    label (maybe "None" id m.milestone) [] >>= #append box
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
  [ "box.u-issue button.link-button { min-height: 0; padding: 0 2px; border: none; box-shadow: none; background: transparent; color: var(--accent); font-size: 13px; }"
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
