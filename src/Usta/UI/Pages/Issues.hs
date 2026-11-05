{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The issue list across every forge.
module Usta.UI.Pages.Issues
  ( Label (..)
  , Relation (..)
  , Issue (..)
  , labelTone
  , fromIssue
  , sample
  , new
  , forRepository
  , css
  ) where

import Control.Monad (forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (nub, sortOn)
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango
import Numeric (readHex)

import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Pager as Pager
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.UI.Page (Page (..), refill)
import Usta.UI.Format (Clock, ago, short, thousands)
import Usta.UI.Style (Tone (..))
import Usta.Types.Involvement (Involvement (..))
import qualified Usta.Types.Issue as I
import qualified Usta.Types.Label as L
import qualified Usta.Types.Listing as Lt
import Usta.Types.Parse (unparse)
import Usta.Types.Ref (Ref (..))

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
  , relations :: [Relation]
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
  AssignedTab -> not i.closed && AssignedToMe `elem` i.relations
  CreatedTab -> not i.closed && CreatedByMe `elem` i.relations
  MentionedTab -> not i.closed && MentionedMe `elem` i.relations
  ClosedTab -> i.closed

-- | @onNew@ runs for "New issue"; @onOpen@ when a row is clicked or
-- activated with Enter.
new :: IO () -> (Issue -> IO ()) -> [Issue] -> IO (Page Issue)
new onNew onOpen issues = do
  items <- newIORef issues
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

  let tabText shown t = case length (filter (inTab t) shown) of
        n | n > 0 && t /= ClosedTab -> tabLabel t <> "  " <> T.pack (show n)
        _ -> tabLabel t
  (tabs, tabButtons) <- Segmented.new (map (tabText issues) [minBound ..]) 0
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
  refill list issueRow issues
  empty <- GI.new Gtk.Label [#label := "Nothing here", #cssClasses := ["muted", "empty"]]
  Gtk.listBoxSetPlaceholder list (Just empty)
  void $ GI.on list #rowActivated $ \row -> do
    index <- Gtk.listBoxRowGetIndex row
    shown <- readIORef items
    onOpen (shown !! fromIntegral index)

  current <- newIORef AssignedTab
  Gtk.listBoxSetFilterFunc list $ Just $ \row -> do
    tab <- readIORef current
    index <- Gtk.listBoxRowGetIndex row
    shown <- readIORef items
    pure (inTab tab (shown !! fromIntegral index))
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
  pure Page
    { widget = page
    , update = \shown -> do
        writeIORef items shown
        refill list issueRow shown
        forM_ (zip [minBound ..] tabButtons) $ \(tab, button) ->
          GI.set button [#label := tabText shown tab]
    }

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

-- | Which issues the repository tab's first control shows.
type Showing :: Type
data Showing = ShowOpen | ShowClosed | ShowAll
  deriving (Eq, Enum, Bounded)

shows' :: Showing -> Issue -> Bool
shows' s i = case s of
  ShowOpen -> not i.closed
  ShowClosed -> i.closed
  ShowAll -> True

showingLabel :: Showing -> Text
showingLabel s = case s of
  ShowOpen -> "Open"
  ShowClosed -> "Closed"
  ShowAll -> "All"

-- | A repository's own issues, for its tab: @synced@ at once, those that
-- involve the user, then the repository's from @load@ a page at a time,
-- for the state and order chosen; the forge's total shows on the state's
-- tab. The label filter and the filter box work on what is loaded.
forRepository :: [Issue] -> (Lt.Listing -> Maybe Text -> IO (Either Text (Lt.Paged Issue))) -> (Issue -> IO ()) -> IO Gtk.Box
forRepository synced load onOpen = do
  items <- newIORef synced
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 10
                         , #hexpand := True
                         , #cssClasses := ["u-issues", "in-repo"]
                         ]
  known <- newIORef Nothing
  let countWith k s shown = case (k, length (filter (shows' s) shown)) of
        (Just (s', t), _) | s' == s -> showingLabel s <> "  " <> thousands t
        (_, n) | n > 0, s == ShowOpen -> showingLabel s <> "  " <> T.pack (show n)
        _ -> showingLabel s
      count = countWith Nothing
  (states, stateButtons) <- Segmented.new (map (`count` synced) [minBound ..]) 0
  Gtk.widgetAddCssClass states "tabs"
  labelChoice <- Gtk.dropDownNewFromStrings ["Any label"]
  filterBox <- GI.new Gtk.SearchEntry [#placeholderText := "Filter issues", #hexpand := True, #halign := Gtk.AlignEnd, #widthRequest := 230]
  sorts <- Gtk.dropDownNewFromStrings ["Newest", "Oldest"]
  toolbar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  mapM_ (#append toolbar) =<< sequence [Gtk.toWidget states, Gtk.toWidget labelChoice, Gtk.toWidget filterBox, Gtk.toWidget sorts]
  #append page toolbar

  list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone, #cssClasses := ["u-issue-list"]]
  empty <- GI.new Gtk.Label [#label := "No issues here", #cssClasses := ["muted", "empty"]]
  Gtk.listBoxSetPlaceholder list (Just empty)
  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-card", "list-card"]]
  #append card list
  scroll <- GI.new Gtk.ScrolledWindow [ #child := card, #vexpand := True, #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      , #propagateNaturalHeight := True, #valign := Gtk.AlignStart ]
  #append page scroll

  showing <- newIORef ShowOpen
  shownRows <- newIORef []
  let draw = do
        everything <- readIORef items
        s <- readIORef showing
        wantedLabel <- selectedText labelChoice
        query <- T.toLower <$> Gtk.editableGetText filterBox
        order <- Gtk.dropDownGetSelected sorts
        let wanted i = shows' s i
              && maybe True (\l -> l `elem` [name | Label _ name <- i.labels]) wantedLabel
              && (T.null query || query `T.isInfixOf` T.toLower i.title || query `T.isInfixOf` T.pack (show i.number))
            sorted = if order == 1 then sortOn (.number) everything else sortOn (Down . (.number)) everything
            rows = filter wanted sorted
        writeIORef shownRows rows
        refill list repositoryRow rows
        k <- readIORef known
        forM_ (zip [minBound ..] stateButtons) $ \(st, b) -> GI.set b [#label := countWith k st everything]
      fill shown = do
        writeIORef items shown
        let names = "Any label" : nub [name | i <- shown, Label _ name <- i.labels]
        model <- Gtk.stringListNew (Just names)
        before <- selectedText labelChoice
        Gtk.dropDownSetModel labelChoice (Just model)
        Gtk.dropDownSetSelected labelChoice (maybe 0 (\n -> fromIntegral (length (takeWhile (/= n) names))) before)
        draw
  fill synced
  let listing = do
        s <- readIORef showing
        order <- Gtk.dropDownGetSelected sorts
        pure Lt.Listing
          { which = case s of {ShowOpen -> Lt.Opened; ShowClosed -> Lt.Shut; ShowAll -> Lt.Every}
          , order = if order == 1 then Lt.Oldest else Lt.Newest }
      got first p = do
        s <- readIORef showing
        forM_ p.total $ \t -> writeIORef known (Just (s, t))
        before <- if first then pure [] else readIORef items
        fill (before <> [i | i <- p.items, i.number `notElem` map (.number) before])
  pager <- Pager.new scroll (\cursor -> listing >>= \l -> load l cursor) got $ \n t -> case t of
    Just total | total > n -> "Showing " <> thousands n <> " of " <> thousands total
    _ -> if n == 0 then "" else "All " <> thousands n <> " shown"
  #append page pager.widget
  let again = do
        writeIORef known Nothing
        fill synced
        pager.restart
  forM_ (zip [minBound ..] stateButtons) $ \(st, b) -> void $ GI.on b #toggled $ do
    active <- GI.get b #active
    when active (writeIORef showing st >> again)
  void $ GI.on labelChoice (GI.PropertyNotify #selected) (const draw)
  void $ GI.on sorts (GI.PropertyNotify #selected) (const again)
  void $ GI.on filterBox #searchChanged draw
  void $ GI.on list #rowActivated $ \row -> do
    index <- Gtk.listBoxRowGetIndex row
    rows <- readIORef shownRows
    when (index >= 0 && fromIntegral index < length rows) $ onOpen (rows !! fromIntegral index)

  pager.restart
  pure page
  where
    selectedText d = do
      item <- Gtk.dropDownGetSelectedItem d
      index <- Gtk.dropDownGetSelected d
      case item of
        Just o | index > 0 -> do
          s <- GI.unsafeCastTo Gtk.StringObject o
          Just <$> Gtk.stringObjectGetString s
        _ -> pure Nothing

-- | A row in a repository's tab: no repository, but who opened it.
repositoryRow :: Issue -> IO Gtk.Box
repositoryRow issue = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  Icon.toned 16 (if issue.closed then Merged else Success) Icon.Issue >>= #append row
  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 3, #hexpand := True]
  titleLine <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  titleLabel <- label issue.title ["title"]
  GI.set titleLabel [#ellipsize := Pango.EllipsizeModeEnd]
  #append titleLine titleLabel
  forM_ issue.labels $ \(Label tone name) -> Badge.new tone name >>= #append titleLine
  #append body titleLine
  label ("#" <> T.pack (show issue.number) <> " · opened by " <> issue.author <> " " <> issue.opened) ["meta"] >>= #append body
  #append row body
  comments <- label (if issue.comments > 0 then "💬 " <> T.pack (show issue.comments) else "") ["meta", "number"]
  GI.set comments [#widthRequest := 40, #xalign := 1]
  #append row comments
  pure row

-- | The list row for a synced issue.
fromIssue :: Clock -> Forge.Kind -> I.Issue -> Issue
fromIssue c forge issue = Issue
  { forge = forge
  , host = unparse issue.ref.host
  , repo = unparse issue.ref.repo
  , number = unparse issue.ref.number
  , title = issue.title
  , labels = [Label (labelTone l.color) l.name | l <- issue.labels]
  , author = unparse issue.author
  , opened = ago c issue.createdAt
  , comments = unparse issue.comments
  , updated = short c issue.updatedAt
  , closed = isJust issue.closedAt
  , relations = [rel | Just rel <- map relation issue.involvement]
  }
  where
    relation i = case i of
      AsAssignee -> Just AssignedToMe
      AsAuthor -> Just CreatedByMe
      AsMentioned -> Just MentionedMe
      AsReviewer -> Nothing

-- | The tone nearest a label's color: grays, and colors too pale or dark
-- to tell apart, are neutral; the rest go by hue.
labelTone :: L.Color -> Tone
labelTone color
  | chroma < 0.15 || lightness > 0.9 || lightness < 0.1 = Neutral
  | hue < 20 || hue >= 340 = Danger
  | hue < 65 = Warning
  | hue < 170 = Success
  | hue < 245 = Accent
  | otherwise = Merged
  where
    channel i = case readHex (T.unpack (T.take 2 (T.drop i (unparse color)))) of
      [(n, "")] -> fromIntegral (n :: Int) / 255
      _ -> 0
    (r, g, b) = (channel 0, channel 2, channel 4) :: (Double, Double, Double)
    high = maximum [r, g, b]
    low = minimum [r, g, b]
    chroma = high - low
    lightness = (high + low) / 2
    hue
      | chroma == 0 = 0
      | high == r = 60 * fmod ((g - b) / chroma) 6
      | high == g = 60 * ((b - r) / chroma + 2)
      | otherwise = 60 * ((r - g) / chroma + 4)
    fmod x m = x - m * fromIntegral (floor (x / m) :: Int)

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [Issue]
sample =
  [ Issue Forge.GitHub "github.com" "abdivasiyev/usta" 5 "Render the main window with GTK4"
      [Label Accent "gtk"] "abdivasiyev" "1 day ago" 2 "1h" False [AssignedToMe]
  , Issue Forge.GitHub "github.com" "abdivasiyev/usta" 3 "Diff view: keep scroll position when switching files"
      [Label Danger "bug"] "abdivasiyev" "2 days ago" 1 "1h" False [AssignedToMe]
  , Issue Forge.GitHub "github.com" "abdivasiyev/usta" 4 "Support GitLab merge request discussions"
      [Label Merged "forge api"] "abdivasiyev" "2 days ago" 0 "Mon" False [AssignedToMe]
  , Issue Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix" 12 "Restart nix-daemon after substituter changes"
      [Label Neutral "nix"] "abdivasiyev" "3 days ago" 3 "Yesterday" False [CreatedByMe]
  , Issue Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" 30 "Binary cache answers as not a cache"
      [Label Danger "bug"] "abdivasiyev" "last week" 4 "Tue" False [MentionedMe]
  , Issue Forge.GitHub "github.com" "abdivasiyev/usta" 1 "Pick a name and a logo"
      [] "abdivasiyev" "last week" 6 "Oct 29" True [CreatedByMe]
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
  , "box.u-issues.in-repo { padding: 14px 20px; }"
  , "box.u-issues.in-repo label.note { font-size: 12px; color: var(--text-muted); padding: 2px 4px; }"
  ]
