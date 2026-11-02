{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | One issue: its thread, timeline and a reply box, with assignees,
-- labels, milestone and linked pull requests on the side.
module Usta.UI.Pages.Issue
  ( Comment (..)
  , Event (..)
  , Model (..)
  , new
  , css
  ) where

import Control.Monad (forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.MarkdownEditor as MarkdownEditor
import Usta.UI.Style (Tone (..))

data Comment = Comment
  { author :: Text
  , initials :: Text
  , when' :: Text
    -- ^ E.g. "opened 1 day ago" or "commented 1 hour ago".
  , body :: Text
  }

-- | Something that happened on the issue besides comments.
data Event
  = LabelAdded Tone Text
  | ReferencedBy Text
    -- ^ A pull request that mentions the issue, e.g. "#6 feat: ...".

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

-- | @backLabel@ names where Back goes, e.g. "Issues" or "Inbox".
new :: Text -> IO () -> Model -> IO Gtk.Widget
new backLabel onBack m = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #hexpand := True
                         , #vexpand := True
                         , #cssClasses := ["u-issue"]
                         ]
  header m backLabel onBack >>= #append page

  columns <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
  main <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 16
                         , #cssClasses := ["main"]
                         ]
  forM_ (comments m) $ \c -> commentCard c >>= #append main
  timeline <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #spacing := 10
                             , #cssClasses := ["timeline"]
                             ]
  forM_ (events m) $ \e -> eventRow e >>= #append timeline
  #append main timeline
  replyBox m >>= #append main
  scroll <- GI.new Gtk.ScrolledWindow [ #child := main
                                      , #hexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]
  #append columns scroll
  details m >>= #append columns
  #append page columns
  Gtk.toWidget page

header :: Model -> Text -> IO () -> IO Gtk.Box
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
  Forge.smallDot (forge m) >>= #append meta
  label (host m <> "/" <> repo m) ["meta", "mono"] >>= #append meta
  label ("#" <> T.pack (show (number m))) ["meta"] >>= #append meta
  (if closed m then Badge.new Merged "Closed" else Badge.new Success "Open") >>= #append meta
  #append titles meta
  titleLabel <- label (title m) ["issue-title"]
  GI.set titleLabel [#ellipsize := Pango.EllipsizeModeEnd]
  #append titles titleLabel
  #append bar titles

  Button.new Button.defaults { Button.label = Just "Open in browser" } >>= #append bar
  pure bar

commentCard :: Comment -> IO Gtk.Box
commentCard c = do
  card <- Card.new
  top <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 10
                        , #cssClasses := ["card-header"]
                        ]
  Avatar.new 24 (initials c) >>= #append top
  label (author c) ["strong"] >>= #append top
  label (when' c) ["muted"] >>= #append top
  #append card top
  text <- label (body c) ["card-body"]
  GI.set text [#wrap := True, #selectable := True]
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

replyBox :: Model -> IO Gtk.Box
replyBox m = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
  (editor, _) <- MarkdownEditor.new MarkdownEditor.defaults
    { MarkdownEditor.hint = Just "Leave a comment. Markdown supported." }
  #append box editor
  buttons <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 8
                            , #halign := Gtk.AlignEnd
                            ]
  Button.new Button.defaults
    { Button.label = Just (if closed m then "Reopen issue" else "Close issue") }
    >>= #append buttons
  Button.new Button.defaults { Button.label = Just "Comment", Button.variant = Button.Primary }
    >>= #append buttons
  #append box buttons
  pure box

details :: Model -> IO Gtk.Box
details m = do
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 20
                         , #widthRequest := 260
                         , #hexpand := False
                         , #cssClasses := ["details"]
                         ]
  section side "Assignees" $ \box ->
    forM_ (assignees m) $ \(short, name) -> do
      row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
      Avatar.new 22 short >>= #append row
      label name [] >>= #append row
      #append box row
  section side "Labels" $ \box -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
    forM_ (labels m) $ \(tone, name) -> Badge.new tone name >>= #append row
    #append box row
  section side "Milestone" $ \box ->
    label (maybe "None" id (milestone m)) [] >>= #append box
  section side "Linked pull requests" $ \box ->
    forM_ (linkedPullRequests m) $ \pr -> label pr ["link"] >>= #append box
  GI.new Gtk.CheckButton [#label := "Notify me about activity", #active := True] >>= #append side
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
  [ "box.u-issue box.issue-header {"
  , "  padding: 8px 16px; background: var(--surface); border-bottom: 1px solid var(--border);"
  , "}"
  , "box.u-issue label.meta { font-size: 12px; color: var(--text-muted); }"
  , "box.u-issue label.issue-title { font-size: 16px; font-weight: 600; }"
  , "box.u-issue box.main { padding: 20px 32px; }"
  , "box.u-issue box.details { padding: 20px 16px; font-size: 13px; }"
  , "box.u-issue label.strong { font-weight: 600; }"
  , "box.u-issue label.link { color: var(--accent); }"
  , "box.u-issue box.card-header {"
  , "  padding: 10px 14px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: var(--window); border-radius: 8px 8px 0 0; font-size: 13px;"
  , "}"
  , "box.u-issue label.card-body { padding: 14px 16px; }"
  , "box.u-issue box.timeline {"
  , "  margin-left: 12px; padding-left: 14px; border-left: 2px solid var(--border); font-size: 13px;"
  , "}"
  ]
