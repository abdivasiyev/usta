{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The pull request list across every forge.
module Usta.UI.Pages.PullRequests
  ( Checks (..)
  , Review (..)
  , Relation (..)
  , PullRequest (..)
  , fromPullRequest
  , listed
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
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Pager as Pager
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.UI.Page (Page (..), refill)
import Usta.UI.Format (Clock, short, thousands)
import Usta.UI.Style (Tone (..))
import Usta.Types.Involvement (Involvement (..))
import qualified Usta.Types.Listing as Lt
import Usta.Types.Parse (unparse)
import qualified Usta.Types.PullRequest as P
import Usta.Types.Ref (Ref (..))

type Checks :: Type
data Checks = ChecksPassed | ChecksFailed | ChecksRunning | NoChecks
  deriving (Eq, Show, Enum, Bounded)

checksLabel :: Checks -> (Tone, Text)
checksLabel c = case c of
  ChecksPassed -> (Success, "Checks passed")
  ChecksFailed -> (Danger, "Checks failed")
  ChecksRunning -> (Warning, "Checks running")
  NoChecks -> (Neutral, "No checks")

type Review :: Type
data Review = ReviewRequested | Approved | ChangesRequested | NoReviewers
  deriving (Eq, Show, Enum, Bounded)

reviewLabel :: Review -> (Tone, Text)
reviewLabel r = case r of
  ReviewRequested -> (Warning, "Review requested")
  Approved -> (Success, "Approved")
  ChangesRequested -> (Danger, "Changes requested")
  NoReviewers -> (Neutral, "No reviewers")

-- | How the pull request involves you; decides which tabs show it.
type Relation :: Type
data Relation = WaitingForMyReview | CreatedByMe | AssignedToMe
  deriving (Eq, Show, Enum, Bounded)

type PullRequest :: Type
data PullRequest = PullRequest
  { forge :: Forge.Kind
  , host :: Text
  , repo :: Text
    -- ^ Full name, e.g. "abdivasiyev/usta"; the list shows the part after
    -- the owner.
  , number :: Int
  , title :: Text
  , branch :: Text
  , base :: Text
    -- ^ The branch it would merge into.
  , author :: Text
  , checks :: Checks
  , review :: Review
  , comments :: Int
  , updated :: Text
  , state :: P.State
  , relations :: [Relation]
  , changes :: (Int, Int)
    -- ^ Changed files and commits.
  , fetching :: Maybe Double
    -- ^ Percent done while the local mirror still fetches the branch.
  }

type Tab :: Type
data Tab = ReviewTab | CreatedTab | AssignedTab | AllTab
  deriving (Eq, Enum, Bounded)

tabLabel :: Tab -> Text
tabLabel t = case t of
  ReviewTab -> "Review requested"
  CreatedTab -> "Created by me"
  AssignedTab -> "Assigned"
  AllTab -> "All open"

inTab :: Tab -> PullRequest -> Bool
inTab t pr = case t of
  ReviewTab -> WaitingForMyReview `elem` pr.relations
  CreatedTab -> CreatedByMe `elem` pr.relations
  AssignedTab -> AssignedToMe `elem` pr.relations
  AllTab -> True

-- | @onNew@ runs for "New pull request"; @onOpen@ when a row is clicked
-- or activated with Enter.
new :: IO () -> (PullRequest -> IO ()) -> [PullRequest] -> IO (Page PullRequest)
new onNew onOpen prs = do
  items <- newIORef prs
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 14
                         , #hexpand := True
                         , #cssClasses := ["u-pull-requests"]
                         ]

  heading <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  title' <- GI.new Gtk.Label [ #label := "Pull requests"
                             , #xalign := 0
                             , #hexpand := True
                             , #cssClasses := ["page-title"]
                             ]
  #append heading title'
  newButton <- Button.new Button.defaults
    { Button.label = Just "New pull request", Button.variant = Button.Primary }
  void $ GI.on newButton #clicked onNew
  #append heading newButton
  #append page heading

  -- Tab labels carry their counts, e.g. "Review requested  2".
  let tabText shown t = case length (filter (inTab t) shown) of
        0 -> tabLabel t
        n | t == AllTab -> tabLabel t
          | otherwise -> tabLabel t <> "  " <> T.pack (show n)
  (tabs, tabButtons) <- Segmented.new (map (tabText prs) [minBound ..]) 0
  Gtk.widgetAddCssClass tabs "tabs"

  filters <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #spacing := 8
                            , #hexpand := True
                            , #halign := Gtk.AlignEnd
                            ]
  forM_ [ ["All forges", "github.com", "git.azizovich.uz"]
        , ["All repositories", "abdivasiyev/usta", "abdivasiyev/homelab", "abdivasiyev/nix"]
        , ["Recently updated", "Newest", "Oldest"]
        ] $ \options -> Gtk.dropDownNewFromStrings options >>= #append filters

  toolbar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  #append toolbar tabs
  #append toolbar filters
  #append page toolbar

  list <- GI.new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone
                             , #cssClasses := ["u-pr-list"]
                             ]
  refill list pullRequestRow prs
  empty <- GI.new Gtk.Label [#label := "Nothing here", #cssClasses := ["muted", "empty"]]
  Gtk.listBoxSetPlaceholder list (Just empty)
  void $ GI.on list #rowActivated $ \row -> do
    index <- Gtk.listBoxRowGetIndex row
    shown <- readIORef items
    onOpen (shown !! fromIntegral index)

  current <- newIORef ReviewTab
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
        refill list pullRequestRow shown
        forM_ (zip [minBound ..] tabButtons) $ \(tab, button) ->
          GI.set button [#label := tabText shown tab]
    }

pullRequestRow :: PullRequest -> IO Gtk.Box
pullRequestRow pr = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 14]

  icon <- Icon.toned 16 (stateTone pr.state) Icon.PullRequest
  #append row icon

  body <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 4
                         , #hexpand := True
                         ]
  titleLine <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  titleLabel <- label pr.title ["title"]
  GI.set titleLabel [#ellipsize := Pango.EllipsizeModeEnd]
  #append titleLine titleLabel
  when (pr.state == P.Draft) $ Badge.new Neutral "Draft" >>= #append titleLine
  #append body titleLine

  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Forge.smallDot pr.forge >>= #append meta
  let shortName = T.drop 1 (T.dropWhile (/= '/') pr.repo)
  label (shortName <> " #" <> T.pack (show pr.number)) ["meta", "mono"] >>= #append meta
  label "·" ["meta"] >>= #append meta
  label pr.branch ["meta", "mono"] >>= #append meta
  label "·" ["meta"] >>= #append meta
  label pr.author ["meta"] >>= #append meta
  #append body meta
  #append row body

  let column text classes width = do
        l <- label text classes
        GI.set l [#widthRequest := width]
        #append row l
      (checksTone, checksText) = checksLabel pr.checks
      (reviewTone, reviewText) = reviewLabel pr.review
  column checksText ["status", toneText checksTone] 120
  column reviewText ["status", toneText reviewTone] 140
  column (T.pack (show pr.comments)) ["meta", "number"] 32
  column pr.updated ["meta", "number"] 72
  pure row
  where
    toneText tone = "tone-" <> T.toLower (T.pack (show tone))

stateTone :: P.State -> Tone
stateTone s = case s of
  P.Open -> Success
  P.Draft -> Neutral
  P.Merged -> Merged
  P.Closed -> Danger

-- | The list row for a synced pull request; 'Nothing' once it is merged or
-- closed, since the list shows open ones.
fromPullRequest :: Clock -> Forge.Kind -> P.PullRequest -> Maybe PullRequest
fromPullRequest c forge pr
  | pr.state `elem` [P.Merged, P.Closed] = Nothing
  | otherwise = Just (listed c forge pr)

-- | The row for a pull request in any state.
listed :: Clock -> Forge.Kind -> P.PullRequest -> PullRequest
listed c forge pr = PullRequest
  { forge = forge
  , host = unparse pr.ref.host
  , repo = unparse pr.ref.repo
  , number = unparse pr.ref.number
  , title = pr.title
  , branch = pr.headBranch
  , base = pr.baseBranch
  , author = unparse pr.author
  , checks = maybe NoChecks checks pr.checks
  , review = maybe NoReviewers review pr.review
  , comments = unparse pr.comments
  , updated = short c pr.updatedAt
  , state = pr.state
  , relations = [rel | Just rel <- map relation pr.involvement]
  , changes = (maybe 0 unparse pr.changedFiles, maybe 0 unparse pr.commits)
    -- 0 until the details are fetched.
  , fetching = Nothing
  }
  where
    checks state = case state of
      P.ChecksPassed -> ChecksPassed
      P.ChecksFailed -> ChecksFailed
      P.ChecksRunning -> ChecksRunning
    review state = case state of
      P.AwaitingReview -> ReviewRequested
      P.Approved -> Approved
      P.ChangesRequested -> ChangesRequested
    relation i = case i of
      AsReviewer -> Just WaitingForMyReview
      AsAuthor -> Just CreatedByMe
      AsAssignee -> Just AssignedToMe
      AsMentioned -> Nothing

-- | Which pull requests the repository tab's first control shows.
type Showing :: Type
data Showing = ShowOpen | ShowDraft | ShowMerged | ShowClosed | ShowAll
  deriving (Eq, Enum, Bounded)

shows' :: Showing -> PullRequest -> Bool
shows' s pr = case s of
  ShowOpen -> pr.state == P.Open
  ShowDraft -> pr.state == P.Draft
  ShowMerged -> pr.state == P.Merged
  ShowClosed -> pr.state == P.Closed
  ShowAll -> True

showingLabel :: Showing -> Text
showingLabel s = case s of
  ShowOpen -> "Open"
  ShowDraft -> "Draft"
  ShowMerged -> "Merged"
  ShowClosed -> "Closed"
  ShowAll -> "All"

-- | A repository's own pull requests, for its tab: @synced@ at once, those
-- that involve the user, then the repository's from @load@ a page at a
-- time, for the state and order chosen; the forge's total shows on the
-- state's tab. The author and review filters and the filter box work on
-- what is loaded.
forRepository :: [PullRequest] -> (Lt.Listing -> Maybe Text -> IO (Either Text (Lt.Paged PullRequest))) -> (PullRequest -> IO ()) -> IO Gtk.Box
forRepository synced load onOpen = do
  items <- newIORef synced
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 10
                         , #hexpand := True
                         , #cssClasses := ["u-pull-requests", "in-repo"]
                         ]
  -- The forge's total for the state shown, once it says.
  known <- newIORef Nothing
  let countWith k s shown = case (k, length (filter (shows' s) shown)) of
        (Just (s', t), _) | s' == s -> showingLabel s <> "  " <> thousands t
        (_, n) | n > 0, s `elem` [ShowOpen, ShowDraft] -> showingLabel s <> "  " <> T.pack (show n)
        _ -> showingLabel s
      count = countWith Nothing
  (states, stateButtons) <- Segmented.new (map (`count` synced) [minBound ..]) 0
  Gtk.widgetAddCssClass states "tabs"
  authors <- Gtk.dropDownNewFromStrings ["All authors"]
  reviews <- Gtk.dropDownNewFromStrings ("Any review" : map (snd . reviewLabel) [minBound ..])
  filterBox <- GI.new Gtk.SearchEntry [#placeholderText := "Filter", #hexpand := True, #halign := Gtk.AlignEnd, #widthRequest := 200]
  sorts <- Gtk.dropDownNewFromStrings ["Recently updated", "Newest", "Oldest"]
  toolbar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  mapM_ (#append toolbar) =<< sequence [Gtk.toWidget states, Gtk.toWidget authors, Gtk.toWidget reviews, Gtk.toWidget filterBox, Gtk.toWidget sorts]
  #append page toolbar

  list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeNone, #cssClasses := ["u-pr-list"]]
  empty <- GI.new Gtk.Label [#label := "No pull requests here", #cssClasses := ["muted", "empty"]]
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
        who <- selectedText authors
        verdict <- Gtk.dropDownGetSelected reviews
        query <- T.toLower <$> Gtk.editableGetText filterBox
        order <- Gtk.dropDownGetSelected sorts
        let wanted pr = shows' s pr
              && maybe True (== pr.author) who
              && (verdict == 0 || fromIntegral verdict - 1 == fromEnum pr.review)
              && (T.null query || query `T.isInfixOf` T.toLower pr.title || query `T.isInfixOf` T.pack (show pr.number))
            sorted = case order of
              1 -> sortOn (Down . (.number)) everything
              2 -> sortOn (.number) everything
              _ -> everything
            rows = filter wanted sorted
        writeIORef shownRows rows
        refill list repositoryRow rows
        k <- readIORef known
        forM_ (zip [minBound ..] stateButtons) $ \(st, b) -> GI.set b [#label := countWith k st everything]
      fill shown = do
        writeIORef items shown
        let names = "All authors" : nub (map (.author) shown)
        model <- Gtk.stringListNew (Just names)
        before <- selectedText authors
        Gtk.dropDownSetModel authors (Just model)
        Gtk.dropDownSetSelected authors (maybe 0 (\n -> fromIntegral (length (takeWhile (/= n) names))) before)
        draw
  fill synced
  let listing = do
        s <- readIORef showing
        order <- Gtk.dropDownGetSelected sorts
        pure Lt.Listing {which = whichOf s, order = [Lt.RecentlyUpdated, Lt.Newest, Lt.Oldest] !! min 2 (fromIntegral order)}
      got first p = do
        s <- readIORef showing
        forM_ p.total $ \t -> writeIORef known (Just (s, t))
        before <- if first then pure [] else readIORef items
        let fresh = [pr | pr <- p.items, pr.number `notElem` map (.number) before]
        fill (before <> fresh)
  pager <- Pager.new scroll (\cursor -> listing >>= \l -> load l cursor) got $ \n t -> case t of
    Just total | total > n -> "Showing " <> thousands n <> " of " <> thousands total
    _ -> if n == 0 then "" else "All " <> thousands n <> " shown"
  #append page pager.widget
  -- Another state or order starts from the first page; until it comes the
  -- synced ones show.
  let again = do
        writeIORef known Nothing
        fill synced
        pager.restart
  forM_ (zip [minBound ..] stateButtons) $ \(st, b) -> void $ GI.on b #toggled $ do
    active <- GI.get b #active
    when active (writeIORef showing st >> again)
  forM_ [authors, reviews] $ \d -> void $ GI.on d (GI.PropertyNotify #selected) (const draw)
  void $ GI.on sorts (GI.PropertyNotify #selected) (const again)
  void $ GI.on filterBox #searchChanged draw
  void $ GI.on list #rowActivated $ \row -> do
    index <- Gtk.listBoxRowGetIndex row
    rows <- readIORef shownRows
    when (index >= 0 && fromIntegral index < length rows) $ onOpen (rows !! fromIntegral index)

  pager.restart
  pure page
  where
    whichOf s = case s of
      ShowOpen -> Lt.Opened
      ShowDraft -> Lt.Drafts
      ShowMerged -> Lt.Merged
      ShowClosed -> Lt.Shut
      ShowAll -> Lt.Every
    selectedText d = do
      item <- Gtk.dropDownGetSelectedItem d
      index <- Gtk.dropDownGetSelected d
      case item of
        Just o | index > 0 -> do
          s <- GI.unsafeCastTo Gtk.StringObject o
          Just <$> Gtk.stringObjectGetString s
        _ -> pure Nothing

-- | A row in a repository's tab: no repository, but where it would merge.
repositoryRow :: PullRequest -> IO Gtk.Box
repositoryRow pr = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  Icon.toned 16 (stateTone pr.state) Icon.PullRequest >>= #append row
  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 3, #hexpand := True]
  titleLine <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  titleLabel <- label pr.title ["title"]
  GI.set titleLabel [#ellipsize := Pango.EllipsizeModeEnd]
  #append titleLine titleLabel
  when (pr.state == P.Draft) $ Badge.new Neutral "Draft" >>= #append titleLine
  #append body titleLine
  label ("#" <> T.pack (show pr.number) <> " · " <> pr.branch <> " → " <> pr.base <> " · " <> pr.author <> " · updated " <> pr.updated) ["meta"]
    >>= #append body
  #append row body
  checksIcon <- case pr.checks of
    ChecksPassed -> Just <$> Icon.toned 15 Success Icon.Check
    ChecksFailed -> Just <$> Icon.toned 15 Danger Icon.Cross
    ChecksRunning -> Just <$> Icon.toned 15 Warning Icon.Clock
    NoChecks -> pure Nothing
  slot <- GI.new Gtk.Box [#widthRequest := 20, #halign := Gtk.AlignCenter]
  forM_ checksIcon $ \i -> GI.set i [#tooltipText := snd (checksLabel pr.checks)] >> #append slot i
  #append row slot
  let (tone, text) = reviewLabel pr.review
  -- Vertical, so the chip can sit at its right edge.
  reviewSlot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #widthRequest := 150, #valign := Gtk.AlignCenter]
  chip <- if pr.review == NoReviewers then label text ["meta"] else Badge.new tone text
  GI.set chip [#halign := Gtk.AlignEnd]
  #append reviewSlot chip
  #append row reviewSlot
  comments <- label (if pr.comments > 0 then "💬 " <> T.pack (show pr.comments) else "") ["meta", "number"]
  GI.set comments [#widthRequest := 40, #xalign := 1]
  #append row comments
  pure row

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [PullRequest]
sample =
  [ PullRequest Forge.GitHub "github.com" "abdivasiyev/usta" 2 "feat: add gi-gtk4 dependency" "feat/gtk4-dep" "master" "abdivasiyev"
      ChecksPassed ReviewRequested 1 "5m" P.Open [WaitingForMyReview] (2, 1) Nothing
  , PullRequest Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" 48 "chore: move cache priorities into nix.settings" "cache-priorities" "master" "abdivasiyev"
      ChecksFailed ReviewRequested 0 "22m" P.Open [WaitingForMyReview] (2, 1) Nothing
  , PullRequest Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" 47 "feat: add binary cache health check" "cache-health" "master" "abdivasiyev"
      ChecksRunning NoReviewers 2 "Yesterday" P.Open [CreatedByMe] (2, 1) Nothing
  , PullRequest Forge.GitHub "github.com" "abdivasiyev/usta" 6 "feat: render main window with GTK4" "feat/main-window" "master" "abdivasiyev"
      NoChecks NoReviewers 0 "Mon" P.Draft [CreatedByMe] (2, 1) Nothing
  , PullRequest Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix" 13 "fix: restart nix-daemon after config change" "daemon-restart" "master" "abdivasiyev"
      ChecksPassed Approved 3 "Mon" P.Open [CreatedByMe] (2, 1) Nothing
  , PullRequest Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix" 21 "refactor: move every host to den aspects" "refactor/den-aspects" "master" "abdivasiyev"
      ChecksRunning ReviewRequested 0 "1h" P.Open [WaitingForMyReview] (212, 14) (Just 64)
  ]

css :: Text
css = mconcat
  [ "box.u-pull-requests { padding: 20px 24px; }"
  , "box.u-pull-requests label.page-title { font-size: 20px; font-weight: 600; }"
  , "box.u-pull-requests box.tabs { background: var(--sidebar); border-radius: 8px; padding: 4px; }"
  , "box.u-pull-requests box.tabs button:checked {"
  , "  background: var(--surface); color: var(--text); box-shadow: 0 1px 2px rgba(27, 29, 33, 0.12);"
  , "}"
  , "box.u-pull-requests dropdown > button {"
  , "  min-height: 34px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 6px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
  , "  font-size: 13px;"
  , "}"
  , "box.u-pull-requests box.list-card { border-radius: 10px; }"
  , "list.u-pr-list { background: transparent; }"
  , "list.u-pr-list row {"
  , "  padding: 12px 16px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: transparent; color: var(--text);"
  , "}"
  , "list.u-pr-list row:last-child { border-bottom: none; }"
  , "list.u-pr-list row:hover { background: var(--window); }"
  , "list.u-pr-list label.title { font-weight: 600; }"
  , "list.u-pr-list label.meta { font-size: 12px; color: var(--text-muted); }"
  , "list.u-pr-list label.status { font-size: 12px; }"
  , "list.u-pr-list label.number { font-size: 12px; }"
  , "list.u-pr-list label.tone-neutral { color: var(--text-muted); }"
  , "list.u-pr-list label.tone-success { color: var(--success); }"
  , "list.u-pr-list label.tone-warning { color: var(--warning); }"
  , "list.u-pr-list label.tone-danger { color: var(--danger); }"
  , "list.u-pr-list label.empty { padding: 32px; }"
  , "box.u-pull-requests.in-repo { padding: 14px 20px; }"
  , "box.u-pull-requests.in-repo label.note { font-size: 12px; color: var(--text-muted); padding: 2px 4px; }"
  ]
