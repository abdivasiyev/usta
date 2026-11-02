{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The pull request list across every forge.
module Usta.UI.Pages.PullRequests
  ( Checks (..)
  , Review (..)
  , Relation (..)
  , PullRequest (..)
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

-- | How the pull request involves you; decides which tab shows it.
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
  , author :: Text
  , checks :: Checks
  , review :: Review
  , comments :: Int
  , updated :: Text
  , draft :: Bool
  , relation :: Relation
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
  ReviewTab -> relation pr == WaitingForMyReview
  CreatedTab -> relation pr == CreatedByMe
  AssignedTab -> relation pr == AssignedToMe
  AllTab -> True

-- | @onNew@ runs for "New pull request"; @onOpen@ when a row is clicked
-- or activated with Enter.
new :: IO () -> (PullRequest -> IO ()) -> [PullRequest] -> IO Gtk.Box
new onNew onOpen prs = do
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
  let tabText t = case length (filter (inTab t) prs) of
        0 -> tabLabel t
        n | t == AllTab -> tabLabel t
          | otherwise -> tabLabel t <> "  " <> T.pack (show n)
  (tabs, tabButtons) <- Segmented.new (map tabText [minBound ..]) 0
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
  forM_ prs $ \pr -> do
    row <- GI.new Gtk.ListBoxRow [#activatable := True]
    content <- pullRequestRow pr
    GI.set row [#child := content]
    #append list row
  empty <- GI.new Gtk.Label [#label := "Nothing here", #cssClasses := ["muted", "empty"]]
  Gtk.listBoxSetPlaceholder list (Just empty)
  void $ GI.on list #rowActivated $ \row -> do
    index <- Gtk.listBoxRowGetIndex row
    onOpen (prs !! fromIntegral index)

  current <- newIORef ReviewTab
  Gtk.listBoxSetFilterFunc list $ Just $ \row -> do
    tab <- readIORef current
    index <- Gtk.listBoxRowGetIndex row
    pure (inTab tab (prs !! fromIntegral index))
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

pullRequestRow :: PullRequest -> IO Gtk.Box
pullRequestRow pr = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 14]

  icon <- Icon.toned 16 (if draft pr then Neutral else Success) Icon.PullRequest
  #append row icon

  body <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 4
                         , #hexpand := True
                         ]
  titleLine <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  titleLabel <- label (title pr) ["title"]
  GI.set titleLabel [#ellipsize := Pango.EllipsizeModeEnd]
  #append titleLine titleLabel
  when (draft pr) $ Badge.new Neutral "Draft" >>= #append titleLine
  #append body titleLine

  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Forge.smallDot (forge pr) >>= #append meta
  let shortName = T.drop 1 (T.dropWhile (/= '/') (repo pr))
  label (shortName <> " #" <> T.pack (show (number pr))) ["meta", "mono"] >>= #append meta
  label "·" ["meta"] >>= #append meta
  label (branch pr) ["meta", "mono"] >>= #append meta
  label "·" ["meta"] >>= #append meta
  label (author pr) ["meta"] >>= #append meta
  #append body meta
  #append row body

  let column text classes width = do
        l <- label text classes
        GI.set l [#widthRequest := width]
        #append row l
      (checksTone, checksText) = checksLabel (checks pr)
      (reviewTone, reviewText) = reviewLabel (review pr)
  column checksText ["status", toneText checksTone] 120
  column reviewText ["status", toneText reviewTone] 140
  column (T.pack (show (comments pr))) ["meta", "number"] 32
  column (updated pr) ["meta", "number"] 72
  pure row
  where
    toneText tone = "tone-" <> T.toLower (T.pack (show tone))

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [PullRequest]
sample =
  [ PullRequest Forge.GitHub "github.com" "abdivasiyev/usta" 2 "feat: add gi-gtk4 dependency" "feat/gtk4-dep" "abdivasiyev"
      ChecksPassed ReviewRequested 1 "5m" False WaitingForMyReview (2, 1) Nothing
  , PullRequest Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" 48 "chore: move cache priorities into nix.settings" "cache-priorities" "abdivasiyev"
      ChecksFailed ReviewRequested 0 "22m" False WaitingForMyReview (2, 1) Nothing
  , PullRequest Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab" 47 "feat: add binary cache health check" "cache-health" "abdivasiyev"
      ChecksRunning NoReviewers 2 "Yesterday" False CreatedByMe (2, 1) Nothing
  , PullRequest Forge.GitHub "github.com" "abdivasiyev/usta" 6 "feat: render main window with GTK4" "feat/main-window" "abdivasiyev"
      NoChecks NoReviewers 0 "Mon" True CreatedByMe (2, 1) Nothing
  , PullRequest Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix" 13 "fix: restart nix-daemon after config change" "daemon-restart" "abdivasiyev"
      ChecksPassed Approved 3 "Mon" False CreatedByMe (2, 1) Nothing
  , PullRequest Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix" 21 "refactor: move every host to den aspects" "refactor/den-aspects" "abdivasiyev"
      ChecksRunning ReviewRequested 0 "1h" False WaitingForMyReview (212, 14) (Just 64)
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
  ]
