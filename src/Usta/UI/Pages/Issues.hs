{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The issue list across every forge.
module Usta.UI.Pages.Issues
  ( Label (..)
  , Relation (..)
  , Issue (..)
  , sample
  , new
  , css
  ) where

import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.UI.Style (Tone (..))

-- | A label as the forge defines it, shown in a tone.
type Label :: Type
data Label = Label Tone Text

-- | How the issue involves you; decides which tab shows it.
type Relation :: Type
data Relation = AssignedToMe | CreatedByMe | MentionedMe
  deriving (Eq, Show, Enum, Bounded)

type Issue :: Type
data Issue = Issue
  { forge :: Forge.Kind
  , host :: Text
  , repo :: Text
    -- ^ Full name, e.g. "abdivasiyev/usta"; the list shows the part after
    -- the owner.
  , number :: Int
  , title :: Text
  , labels :: [Label]
  , author :: Text
  , opened :: Text
  , comments :: Int
  , updated :: Text
  , closed :: Bool
  , relation :: Relation
  }

type Tab :: Type
data Tab = AssignedTab | CreatedTab | MentionedTab | ClosedTab
  deriving (Eq, Enum, Bounded)

tabLabel :: Tab -> Text
tabLabel t = case t of
  AssignedTab -> "Assigned to me"
  CreatedTab -> "Created by me"
  MentionedTab -> "Mentioned"
  ClosedTab -> "Closed"

inTab :: Tab -> Issue -> Bool
inTab t i = case t of
  AssignedTab -> not i.closed && i.relation == AssignedToMe
  CreatedTab -> not i.closed && i.relation == CreatedByMe
  MentionedTab -> not i.closed && i.relation == MentionedMe
  ClosedTab -> i.closed

-- | @onNew@ runs for "New issue"; @onOpen@ when a row is clicked or
-- activated with Enter.
new :: IO () -> (Issue -> IO ()) -> [Issue] -> IO Gtk.Box
new onNew onOpen issues = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 14
                         , #hexpand := True
                         , #cssClasses := ["u-issues"]
                         ]

  heading <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  pageTitle <- GI.new Gtk.Label [ #label := "Issues"
                                , #xalign := 0
                                , #hexpand := True
                                , #cssClasses := ["page-title"]
                                ]
  #append heading pageTitle
  newButton <- Button.new Button.defaults
    { Button.label = Just "New issue", Button.variant = Button.Primary }
  void $ GI.on newButton #clicked onNew
  #append heading newButton
  #append page heading

  let tabText t = case length (filter (inTab t) issues) of
        n | n > 0 && t /= ClosedTab -> tabLabel t <> "  " <> T.pack (show n)
        _ -> tabLabel t
  (tabs, tabButtons) <- Segmented.new (map tabText [minBound ..]) 0
  Gtk.widgetAddCssClass tabs "tabs"

  filters <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 8
                            , #hexpand := True
                            , #halign := Gtk.AlignEnd
                            ]
  forM_ [ ["All repositories", "abdivasiyev/usta", "abdivasiyev/homelab", "abdivasiyev/nix"]
        , ["Any label", "gtk", "bug", "forge api", "nix"]
        ] $ \options -> Gtk.dropDownNewFromStrings options >>= #append filters

  toolbar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  #append toolbar tabs
  #append toolbar filters
  #append page toolbar

  list <- GI.new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone
                             , #cssClasses := ["u-issue-list"]
                             ]
  forM_ issues $ \issue -> do
    row <- GI.new Gtk.ListBoxRow [#activatable := True]
    content <- issueRow issue
    GI.set row [#child := content]
    #append list row
  empty <- GI.new Gtk.Label [#label := "Nothing here", #cssClasses := ["muted", "empty"]]
  Gtk.listBoxSetPlaceholder list (Just empty)
  void $ GI.on list #rowActivated $ \row -> do
    index <- Gtk.listBoxRowGetIndex row
    onOpen (issues !! fromIntegral index)

  current <- newIORef AssignedTab
  Gtk.listBoxSetFilterFunc list $ Just $ \row -> do
    tab <- readIORef current
    index <- Gtk.listBoxRowGetIndex row
    pure (inTab tab (issues !! fromIntegral index))
  forM_ (zip [minBound ..] tabButtons) $ \(tab, button) ->
    void $ GI.on button #toggled $ do
      active <- GI.get button #active
      when active $ do
        writeIORef current tab
        Gtk.listBoxInvalidateFilter list

  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-card", "list-card"]]
  #append card list
  scroll <- GI.new Gtk.ScrolledWindow [ #child := card
                                      , #vexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      , #propagateNaturalHeight := True
                                      , #valign := Gtk.AlignStart
                                      ]
  #append page scroll
  pure page

issueRow :: Issue -> IO Gtk.Box
issueRow issue = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 14]
  icon <- Icon.toned 16 (if issue.closed then Merged else Success) Icon.Issue
  #append row icon

  body <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 4
                         , #hexpand := True
                         ]
  titleLine <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  titleLabel <- label issue.title ["title"]
  GI.set titleLabel [#ellipsize := Pango.EllipsizeModeEnd]
  #append titleLine titleLabel
  forM_ issue.labels $ \(Label tone name) -> Badge.new tone name >>= #append titleLine
  #append body titleLine

  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Forge.smallDot issue.forge >>= #append meta
  let shortName = T.drop 1 (T.dropWhile (/= '/') issue.repo)
  label (shortName <> " #" <> T.pack (show issue.number)) ["meta", "mono"] >>= #append meta
  label ("· opened " <> issue.opened <> " by " <> issue.author) ["meta"] >>= #append meta
  #append body meta
  #append row body

  let column text width = do
        l <- label text ["meta", "number"]
        GI.set l [#widthRequest := width]
        #append row l
  column (T.pack (show issue.comments)) 32
  column issue.updated 72
  pure row

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [Issue]
sample =
  [ Issue Forge.GitHub "github.com" "abdivasiyev/usta" 5 "Render the main window with GTK4"
      [Label Accent "gtk"] "abdivasiyev" "1 day ago" 2 "1h" False AssignedToMe
  , Issue Forge.GitHub "github.com" "abdivasiyev/usta" 3 "Diff view: keep scroll position when switching files"
      [Label Danger "bug"] "abdivasiyev" "2 days ago" 1 "1h" False AssignedToMe
  , Issue Forge.GitHub "github.com" "abdivasiyev/usta" 4 "Support GitLab merge request discussions"
      [Label Merged "forge api"] "abdivasiyev" "2 days ago" 0 "Mon" False AssignedToMe
  , Issue Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix" 12 "Restart nix-daemon after substituter changes"
      [Label Neutral "nix"] "abdivasiyev" "3 days ago" 3 "Yesterday" False CreatedByMe
  , Issue Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" 30 "Binary cache answers as not a cache"
      [Label Danger "bug"] "abdivasiyev" "last week" 4 "Tue" False MentionedMe
  , Issue Forge.GitHub "github.com" "abdivasiyev/usta" 1 "Pick a name and a logo"
      [] "abdivasiyev" "last week" 6 "Oct 29" True CreatedByMe
  ]

css :: Text
css = mconcat
  [ "box.u-issues { padding: 20px 24px; }"
  , "box.u-issues label.page-title { font-size: 20px; font-weight: 600; }"
  , "box.u-issues box.tabs { background: var(--sidebar); border-radius: 8px; padding: 4px; }"
  , "box.u-issues box.tabs button:checked {"
  , "  background: var(--surface); color: var(--text); box-shadow: 0 1px 2px rgba(27, 29, 33, 0.12);"
  , "}"
  , "box.u-issues dropdown > button {"
  , "  min-height: 34px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 6px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
  , "  font-size: 13px;"
  , "}"
  , "box.u-issues box.list-card { border-radius: 10px; }"
  , "list.u-issue-list { background: transparent; }"
  , "list.u-issue-list row {"
  , "  padding: 12px 16px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: transparent; color: var(--text);"
  , "}"
  , "list.u-issue-list row:last-child { border-bottom: none; }"
  , "list.u-issue-list row:hover { background: var(--window); }"
  , "list.u-issue-list label.title { font-weight: 600; }"
  , "list.u-issue-list label.meta { font-size: 12px; color: var(--text-muted); }"
  , "list.u-issue-list label.number { font-size: 12px; }"
  , "list.u-issue-list label.empty { padding: 32px; }"
  ]
