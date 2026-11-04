{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Repositories across every forge: a filterable list on the left, the
-- selected repository's overview on the right.
module Usta.UI.Pages.Repositories
  ( Commit (..)
  , Repository (..)
  , fromRepository
  , Actions (..)
  , Tab (..)
  , ChooseMirror
  , Origins (..)
  , LookUpOrigins
  , Contents (..)
  , LoadContents
  , sample
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Markdown as Markdown
import Usta.UI.Pages.RepositoryFiles (linkTarget)
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.SearchField as SearchField
import qualified Usta.UI.Components.Skeleton as Skeleton
import qualified Usta.UI.Platform as Platform
import Usta.UI.Page (Page (..), refill)
import Usta.UI.Style (Tone (..))
import qualified Usta.Git as Git
import Usta.Types.Mirror (Choice (..), Transport (..))
import Usta.Types.Parse (unparse)
import qualified Usta.Types.Repository as R

type Commit :: Type
data Commit = Commit
  { hash :: Text
  , message :: Text
  , age :: Text
  }

-- | The README, already parsed from Markdown into the blocks we show.

type Repository :: Type
data Repository = Repository
  { forge :: Forge.Kind
  , host :: Text
  , name :: Text
    -- ^ Full name, e.g. "abdivasiyev/usta".
  , description :: Text
  , pullRequests :: Int
  , issues :: Int
  , defaultBranch :: Text
  , mirror :: (Tone, Text)
    -- ^ State of the local mirror, e.g. (Success, "Up to date").
  , mirrorDetail :: Maybe Text
    -- ^ More about that state, such as why a fetch failed.
  , canMirror :: Bool
    -- ^ Offers "Mirror…": the repository has no mirror yet, or its mirror
    -- failed.
  , recentCommits :: [Commit]
  , readme :: Text
    -- ^ The README's Markdown; empty when there is none, or until loaded.
  }

-- | @onNewIssue@ and @onNewPullRequest@ run for the header buttons, with the
-- repository shown.
-- | The selected repository stays selected when the list is updated; the
-- first one is selected when it is gone.
-- | Records the user's answer for a repository's mirror; 'Left' says why
-- it did not work. Runs off the GTK main thread, since it asks the forge.
type ChooseMirror :: Type
type ChooseMirror = Repository -> Choice -> IO (Either Text ())

-- | Where a mirror of the repository would clone from over each transport,
-- or why it cannot, and the folder mirrors are kept in.
type Origins :: Type
data Origins = Origins
  { https :: Either Text Text
  , ssh :: Either Text Text
  , keptIn :: Text
  }

-- | Asks the forge for 'Origins'; 'Left' says why it could not. Runs off
-- the GTK main thread.
type LookUpOrigins :: Type
type LookUpOrigins = Repository -> IO (Either Text Origins)

-- | A repository's recent commits and README, loaded when it is shown.
type Contents :: Type
data Contents = Contents
  { commits :: [Commit]
  , readme :: Maybe (Text, Text)
    -- ^ The README's file name and text.
    -- ^ The README's file name and blocks.
  }

-- | Loads 'Contents'; 'Left' says why not. Runs off the GTK main thread.
type LoadContents :: Type
type LoadContents = Repository -> IO (Either Text Contents)

-- | What the page's buttons and overview do.
type Actions :: Type
data Actions = Actions
  { newIssue :: Repository -> IO ()
  , newPullRequest :: Repository -> IO ()
  , chooseMirror :: ChooseMirror
  , lookUpOrigins :: LookUpOrigins
  , loadContents :: LoadContents
  , files :: Repository -> Maybe Text -> IO Gtk.Widget
    -- ^ The Files tab, built the first time it is chosen, or again to
    -- open a file or folder a README links to.
  , tabs :: Repository -> [Tab]
    -- ^ The tabs after Files, such as Pull requests.
  }

-- | A tab after Files, built the first time it is chosen.
type Tab :: Type
data Tab = Tab
  { title :: Text
    -- ^ With its count, e.g. "Pull requests  3".
  , build :: IO Gtk.Widget
  }

new :: Actions -> [Repository] -> IO (Page Repository)
new actions repositories = do
  items <- newIORef repositories
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #hexpand := True]

  detail <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #hexpand := True, #vexpand := True]

  list <- GI.new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeSingle
                             , #cssClasses := ["u-repo-list"]
                             ]
  refill list repositoryRow repositories

  listPane <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                             , #widthRequest := 288
                             , #hexpand := False
                             , #cssClasses := ["u-repo-pane"]
                             ]
  selectedKey <- newIORef Nothing
  -- A sync refills the list, which drops the selection for a moment and
  -- selects the same repository again: its detail stays as it is, on the
  -- tab, file and scroll position the user left it.
  void $ GI.on list #rowSelected $ \selected -> forM_ selected $ \row -> do
      index <- Gtk.listBoxRowGetIndex row
      shown <- readIORef items
      before <- readIORef selectedKey
      when (index >= 0 && fromIntegral index < length shown && before /= Just (key (shown !! fromIntegral index))) $ do
        let r = shown !! fromIntegral index
        writeIORef selectedKey (Just (key r))
        clear detail
        GI.set listPane [#visible := True]
        -- Every tab but Overview takes the list's room, as the design draws it.
        repositoryDetail actions r (\wide -> GI.set listPane [#visible := not wide]) >>= #append detail

  (search, entry) <- SearchField.new SearchField.defaults
    { SearchField.placeholder = "Filter repositories"
    , SearchField.shortcut = Nothing
    , SearchField.width = -1
    }
  query <- newIORef ""
  Gtk.listBoxSetFilterFunc list $ Just $ \row -> do
    q <- readIORef query
    index <- Gtk.listBoxRowGetIndex row
    shown <- readIORef items
    pure (matches q (shown !! fromIntegral index))
  void $ GI.on entry #searchChanged $ do
    text <- Gtk.editableGetText entry
    writeIORef query (T.toLower text)
    Gtk.listBoxInvalidateFilter list

  searchBar <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["search-bar"]]
  #append searchBar search

  listScroll <- GI.new Gtk.ScrolledWindow [ #child := list
                                          , #vexpand := True
                                          , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                          ]
  #append listPane searchBar
  #append listPane listScroll

  #append page listPane
  #append page detail

  let select = do
        shown <- readIORef items
        wanted <- readIORef selectedKey
        let index = maybe 0 (\k -> length (takeWhile ((/= k) . key) shown)) wanted
            at = if index < length shown then index else 0
        row <- Gtk.listBoxGetRowAtIndex list (fromIntegral at)
        Gtk.listBoxSelectRow list row
  select
  pure Page
    { widget = page
    , update = \shown -> do
        writeIORef items shown
        refill list repositoryRow shown
        when (null shown) $ clear detail >> writeIORef selectedKey Nothing
        select
    }
  where
    key r = (r.host, r.name)

-- | The Overview, Files and later tabs over the one chosen; @onWide@
-- runs with whether a tab other than Overview is shown.
repositoryDetail :: Actions -> Repository -> (Bool -> IO ()) -> IO Gtk.Box
repositoryDetail actions r onWide = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #hexpand := True, #vexpand := True]
  tabBar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 4, #cssClasses := ["repo-tabs"]]
  stack <- GI.new Gtk.Stack [ #vexpand := True
                            , #hexpand := True
                            , #transitionType := Gtk.StackTransitionTypeCrossfade
                            , #transitionDuration := 150
                            ]
  overviewScroll <- GI.new Gtk.ScrolledWindow [#hexpand := True, #vexpand := True]
  -- The README's links to files open them in the Files tab, once it exists.
  openFiles <- newIORef (const (pure ()))
  overview actions r (\path -> readIORef openFiles >>= ($ path)) >>= \o -> GI.set overviewScroll [#child := o]
  void $ Gtk.stackAddNamed stack overviewScroll (Just "overview")
  -- Every other tab is built the first time it is chosen.
  let later = Tab {title = "Files", build = actions.files r Nothing} : actions.tabs r
  slots <- forM (zip [1 :: Int ..] later) $ \(i, tab) -> do
    slot <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #vexpand := True]
    void $ Gtk.stackAddNamed stack slot (Just (T.pack (show i)))
    built <- newIORef False
    pure (slot, built, tab)
  let choose name = do
        forM_ (zip [1 :: Int ..] slots) $ \(i, (slot, built, tab)) -> when (T.pack (show i) == name) $ do
          done <- readIORef built
          when (not done) $ do
            writeIORef built True
            tab.build >>= #append slot
        onWide (name /= "overview")
        Gtk.stackSetVisibleChildName stack name
  buttons <- forM (("Overview", "overview") : [(tab.title, T.pack (show i)) | (i, (_, _, tab)) <- zip [1 :: Int ..] slots]) $ \(text, name) -> do
    b <- GI.new Gtk.ToggleButton [#label := text, #active := name == "overview"]
    void $ GI.on b #toggled $ do
      active <- GI.get b #active
      when active (choose name)
    #append tabBar b
    pure b
  case buttons of
    first : rest -> forM_ rest $ \b -> GI.set b [#group := first]
    [] -> pure ()
  forM_ (take 1 slots) $ \(filesSlot, built, _) -> writeIORef openFiles $ \path -> do
    clear filesSlot
    writeIORef built True
    actions.files r (Just path) >>= #append filesSlot
    forM_ (take 1 (drop 1 buttons)) $ \b -> GI.set b [#active := True]
  #append box tabBar
  #append box stack
  pure box

-- | Removes everything in @box@.
clear :: Gtk.Box -> IO ()
clear box = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear box)

-- | Case-insensitive match on the name or host; @q@ is already lowercase.
matches :: Text -> Repository -> Bool
matches q r = q `T.isInfixOf` T.toLower r.name || q `T.isInfixOf` T.toLower r.host

repositoryRow :: Repository -> IO Gtk.Box
repositoryRow r = do
  row <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
  nameLine <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Forge.smallDot r.forge >>= #append nameLine
  label r.name ["mono", "name"] >>= #append nameLine
  #append row nameLine
  label (T.intercalate " · " (r.host : counts r)) ["meta"] >>= #append row
  pure row

-- | "1 PR", "3 issues"; nothing for zero.
counts :: Repository -> [Text]
counts r =
  [ plural r.pullRequests "PR" "PRs" | r.pullRequests > 0 ]
  <> [ plural r.issues "issue" "issues" | r.issues > 0 ]
  where
    plural n one many = T.pack (show n) <> " " <> if n == 1 then one else many

-- | @follow@ opens a path the README links to.
overview :: Actions -> Repository -> (Text -> IO ()) -> IO Gtk.Box
overview actions r follow = do
  pane <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 20
                         , #cssClasses := ["u-repo"]
                         ]

  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  titles <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 4
                           , #hexpand := True
                           ]
  label r.host ["muted"] >>= #append titles
  label r.name ["mono", "repo-name"] >>= #append titles
  descriptionLabel <- label r.description ["description"]
  GI.set descriptionLabel [#wrap := True]
  #append titles descriptionLabel
  #append header titles
  newIssue <- Button.new Button.defaults { Button.label = Just "New issue" }
  void $ GI.on newIssue #clicked (actions.newIssue r)
  newPullRequest <- Button.new Button.defaults
    { Button.label = Just "New pull request", Button.variant = Button.Primary }
  void $ GI.on newPullRequest #clicked (actions.newPullRequest r)
  mirrorButtons <- if r.canMirror
    then do
      b <- Button.new Button.defaults { Button.label = Just "Mirror…" }
      void $ GI.on b #clicked $ Gtk.toWidget b >>= \anchor -> askMirror anchor r actions.chooseMirror actions.lookUpOrigins
      pure [b]
    else pure []
  forM_ (mirrorButtons <> [newIssue, newPullRequest]) $ \b -> do
    GI.set b [#valign := Gtk.AlignStart]
    #append header b
  #append pane header

  stats <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 12
                          , #homogeneous := True
                          ]
  Card.stat Neutral "Open pull requests" (T.pack (show r.pullRequests)) >>= #append stats
  Card.stat Neutral "Open issues" (T.pack (show r.issues)) >>= #append stats
  branch <- Card.stat Neutral "Default branch" r.defaultBranch
  Gtk.widgetAddCssClass branch "branch"
  #append stats branch
  let (mirrorTone, mirrorText) = r.mirror
  mirrorCard <- Card.stat mirrorTone "Local mirror" mirrorText
  forM_ r.mirrorDetail $ \detail -> GI.set mirrorCard [#tooltipText := detail]
  #append stats mirrorCard
  #append pane stats

  -- The sample carries its contents; a synced repository loads them.
  contents <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 20]
  #append pane contents
  if not (null r.recentCommits && T.null r.readme)
    then sections r follow Contents {commits = r.recentCommits, readme = Just ("README.md", r.readme)} contents
    else do
      Skeleton.block 120 >>= #append contents
      void $ forkIO $ do
        loaded <- try (actions.loadContents r)
        let result = either (\(_ :: SomeException) -> Left "Loading failed unexpectedly.") id loaded
        void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
          clear contents
          Skeleton.appear contents
          case result of
            Right c -> sections r follow c contents
            Left message -> label message ["muted"] >>= #append contents
          pure False
  pure pane
  where

-- | The recent commits and README sections, into @into@.
sections :: Repository -> (Text -> IO ()) -> Contents -> Gtk.Box -> IO ()
sections r follow c into = do
  when (not (null c.commits)) $ do
    section <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
    Card.sectionTitle ("Recent commits on " <> r.defaultBranch) >>= #append section
    card <- Card.new
    forM_ c.commits $ \commit -> do
      line <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                             , #spacing := 12
                             , #cssClasses := ["commit-row"]
                             ]
      label (T.take 7 commit.hash) ["mono", "hash"] >>= #append line
      messageLabel <- label commit.message []
      GI.set messageLabel [#hexpand := True, #ellipsize := Pango.EllipsizeModeEnd]
      #append line messageLabel
      label commit.age ["muted", "age"] >>= #append line
      #append card line
    #append section card
    #append into section
  case c.readme of
    Just (name, text) | not (T.null (T.strip text)) -> do
      section <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
      Card.sectionTitle name >>= #append section
      card <- Card.new
      Gtk.widgetAddCssClass card "readme"
      -- A README in Markdown is rendered; any other is shown as written.
      body <- if any (`T.isSuffixOf` T.toLower name) [".md", ".markdown"] || T.toLower name == "readme"
        then Markdown.newIn (mapM_ follow . linkTarget name) text
        else label text ["mono"] >>= \l -> GI.set l [#wrap := True, #selectable := True] >> Gtk.toWidget l
      #append card body
      #append section card
      #append into section
    _ -> pure ()
  when (null c.commits && maybe True (T.null . T.strip . snd) c.readme) $
    label "No commits or README to show yet." ["muted"] >>= #append into

-- | Asks whether to mirror @r@, and how, as the add-account dialog asks
-- for an account. Its "Don't mirror" is kept, so the question is not asked
-- again; the page's button stays for changing one's mind.
askMirror :: Gtk.Widget -> Repository -> ChooseMirror -> LookUpOrigins -> IO ()
askMirror anchor r chooseMirror lookUp = do
  root <- Gtk.widgetGetRoot anchor
  parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
  dialog <- GI.new Gtk.Window [ #title := "Mirror this repository"
                              , #modal := True
                              , #defaultWidth := 640
                              , #resizable := False
                              , #destroyWithParent := True
                              , #cssClasses := ["usta", "u-mirror"]
                              ]
  forM_ parent $ \p -> Gtk.windowSetTransientFor dialog (Just p)
  header <- GI.new Gtk.HeaderBar []
  Platform.configureHeaderBar Platform.current header
  titles <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #valign := Gtk.AlignCenter]
  label "Mirror this repository" ["title"] >>= #append titles
  subtitle <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignCenter]
  label r.name ["subtitle", "mono"] >>= #append subtitle
  label (" on " <> r.host) ["subtitle"] >>= #append subtitle
  #append titles subtitle
  GI.set header [#titleWidget := titles]
  GI.set dialog [#titlebar := header]

  body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10, #cssClasses := ["form"]]
  wrapped "A local mirror is a copy of the repository on this computer. Diffs and history then open at once and work offline. Without one, usta asks the forge each time." ["intro"]
    >>= #append body

  step "1. What to download" >>= #append body
  everything <- optionCard "Everything" "The whole repository, once. Diffs work offline."
  historyOnly <- optionCard "History only" "Smaller. File contents are fetched the first time a diff needs them."
  Gtk.checkButtonSetGroup historyOnly (Just everything)
  GI.set everything [#active := True]
  cards [everything, historyOnly] >>= #append body

  step "2. Connect over" >>= #append body
  viaHttps <- optionCard "HTTPS" "With your account's token, from the system keyring."
  viaSsh <- optionCard "SSH" "With your own ssh keys. Connect to the server with ssh once first, so its host key is known."
  Gtk.checkButtonSetGroup viaSsh (Just viaHttps)
  GI.set viaHttps [#active := True]
  cards [viaHttps, viaSsh] >>= #append body
  origin <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["origin"]]
  GI.new Gtk.Image [#iconName := "insert-link-symbolic", #pixelSize := 16] >>= #append origin
  originText <- label "Looking up the clone URL…" ["muted"]
  #append origin originText
  originUrl <- label "" ["mono", "url"]
  GI.set originUrl [#visible := False, #wrap := True, #wrapMode := Pango.WrapModeChar, #hexpand := True]
  #append origin originUrl
  #append body origin
  problemBox <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["problem"], #visible := False]
  GI.new Gtk.Image [#iconName := "dialog-warning-symbolic", #pixelSize := 16, #valign := Gtk.AlignStart] >>= #append problemBox
  problemText <- wrapped "" []
  GI.set problemText [#hexpand := True]
  #append problemBox problemText
  #append body problemBox

  step "3. Where it is kept" >>= #append body
  kept <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  label "In " ["muted"] >>= #append kept
  keptPath <- label "" ["mono", "url"]
  #append kept keptPath
  label ". usta fetches it after each sync." ["muted"] >>= #append kept
  #append body kept

  footer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #cssClasses := ["footer"]]
  declineTexts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  label "Don't mirror" ["name"] >>= #append declineTexts
  label "Use the forge's API; usta won't ask again" ["muted"] >>= #append declineTexts
  decline <- GI.new Gtk.Button [#child := declineTexts, #cssClasses := ["flat", "decline"]]
  spacer <- GI.new Gtk.Box [#hexpand := True]
  cancel <- Button.new Button.defaults { Button.label = Just "Cancel" }
  go <- Button.new Button.defaults { Button.label = Just "Mirror", Button.variant = Button.Primary }
  GI.set go [#sensitive := False]
  forM_ [decline] (#append footer)
  #append footer spacer
  #append footer cancel
  #append footer go

  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append content body
  #append content footer
  GI.set dialog [#child := content]

  found <- newIORef Nothing
  busy <- newIORef False
  let showProblem message = GI.set problemText [#label := message] >> GI.set problemBox [#visible := True]
      refresh = do
        looked <- readIORef found
        https <- GI.get viaHttps #active
        working <- readIORef busy
        case looked of
          Nothing -> GI.set go [#sensitive := False]
          Just (Left message) -> do
            GI.set originText [#label := "The clone URL could not be looked up."]
            showProblem message
            GI.set go [#sensitive := False]
          Just (Right o) -> do
            GI.set keptPath [#label := o.keptIn]
            case (if https then o.https else o.ssh) of
              Right url -> do
                GI.set originText [#label := "Clones from"]
                GI.set originUrl [#label := url, #visible := True]
                GI.set problemBox [#visible := False]
                GI.set go [#sensitive := not working]
              Left message -> do
                GI.set originText [#label := "No clone URL over " <> (if https then "HTTPS" else "SSH") <> "."]
                GI.set originUrl [#visible := False]
                showProblem message
                GI.set go [#sensitive := False]
      later act = void $ GLib.idleAdd GLib.PRIORITY_DEFAULT (act >> pure False)
      answer button other doing choice = do
        writeIORef busy True
        Button.busy button [other] doing (try (chooseMirror r choice)) $ \outcome -> do
          writeIORef busy False
          case outcome of
            Right (Right ()) -> Gtk.windowDestroy dialog
            Right (Left message) -> showProblem message >> refresh
            Left (_ :: SomeException) -> showProblem "Choosing the mirror failed unexpectedly." >> refresh
  forM_ [viaHttps, viaSsh] $ \b -> void $ GI.on b #toggled refresh
  void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
  void $ GI.on decline #clicked (answer decline go "Saving…" Declined)
  void $ GI.on go #clicked $ do
    full <- GI.get everything #active
    https <- GI.get viaHttps #active
    answer go decline "Starting…" (Mirrored (if full then Git.Full else Git.Partial) (if https then Https else Ssh))
  void $ forkIO $ do
    outcome <- try (lookUp r)
    let result = case outcome of
          Left (_ :: SomeException) -> Left "Looking up the clone URL failed unexpectedly."
          Right o -> o
    later (writeIORef found (Just result) >> refresh)
  #present dialog
  -- GTK would focus "Don't mirror", drawing a ring the design does not have.
  Gtk.windowSetFocus dialog (Nothing :: Maybe Gtk.Widget)
  where
    wrapped text classes = do
      l <- label text classes
      GI.set l [#wrap := True]
      pure l
    step text = label text ["step"]
    cards buttons = do
      row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10, #homogeneous := True]
      forM_ buttons (#append row)
      pure row

-- | One choice: a radio, a bold title and a muted line, as the
-- add-account dialog's forge cards.
optionCard :: Text -> Text -> IO Gtk.CheckButton
optionCard title text = do
  texts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  GI.new Gtk.Label [#label := title, #xalign := 0, #cssClasses := ["name"]] >>= #append texts
  GI.new Gtk.Label [#label := text, #xalign := 0, #wrap := True, #cssClasses := ["reach"]] >>= #append texts
  GI.new Gtk.CheckButton [#child := texts, #cssClasses := ["option-card"]]

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

-- | The list entry for a synced repository. Commits and the README come
-- from the local mirror, which is not read yet; the mirror state is the
-- source's to fill in.
fromRepository :: Forge.Kind -> R.Repository -> Repository
fromRepository forge r = Repository
  { forge = forge
  , host = unparse r.host
  , name = unparse r.name
  , description = fromMaybe "" r.description
  , pullRequests = unparse r.openPullRequests
  , issues = unparse r.openIssues
  , defaultBranch = r.defaultBranch
  , mirror = (Neutral, "Not mirrored")
  , mirrorDetail = Nothing
  , canMirror = False
  , recentCommits = []
  , readme = ""
  }

sample :: [Repository]
sample =
  [ Repository Forge.GitHub "github.com" "abdivasiyev/usta"
      "A desktop client to connect with your forge" 1 3 "master" (Success, "Up to date") Nothing False
      [ Commit "a41f0c2" "feat: scaffold Nix flake and Hello world app" "3h"
      , Commit "8427b36" "Initial commit" "2 days"
      ]
      "# usta\n\nA desktop client to connect with your forge.\n\n```sh\nnix run\n```\n"
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/homelab"
      "NixOS configuration for the home server" 2 0 "master" (Success, "Up to date") Nothing False
      [Commit "5c2e91d" "feat(cache): serve the binary cache over HTTPS" "1d"]
      "# homelab\n\nNixOS configuration for the home server.\n\n```sh\nnix run .#deploy\n```\n"
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/nix"
      "Nix configurations for my machines" 0 1 "master" (Warning, "2 behind") Nothing False
      [Commit "e03b7aa" "fix: restart nix-daemon after substituter changes" "3d"]
      ""
  , Repository Forge.Forgejo "git.azizovich.uz" "abdivasiyev/cartoons"
      "Cartoons made with Remotion" 0 0 "main" (Success, "Up to date") Nothing False
      [] ""
  , Repository Forge.GitHub "github.com" "abdivasiyev/dotfiles"
      "Shell and editor settings" 0 0 "master" (Neutral, "Not mirrored") Nothing True
      [] ""
  ]

css :: Text
css = mconcat
  -- Underlined tabs, as on a pull request.
  [ "box.repo-tabs { padding: 0 24px; background: var(--surface); border-bottom: 1px solid var(--border); }"
  , "box.repo-tabs button {"
  , "  min-height: 44px; padding: 0 12px; border: none; border-radius: 0; background: none;"
  , "  box-shadow: none; color: var(--text-soft);"
  , "}"
  , "box.repo-tabs button:checked { color: var(--text); font-weight: 600; box-shadow: inset 0 -2px 0 var(--accent); }"
  -- The add-account dialog's look, which this one follows.
  , "window.u-mirror headerbar {"
  , "  background: var(--surface); border-bottom: 1px solid var(--border); box-shadow: none;"
  , "}"
  , "window.u-mirror headerbar label.title { font-size: 15px; font-weight: 600; }"
  , "window.u-mirror headerbar label.subtitle { font-size: 13px; color: var(--text-muted); }"
  , "window.u-mirror box.form { padding: 20px; background: var(--surface); }"
  , "window.u-mirror label.intro { color: var(--text-soft); margin-bottom: 6px; }"
  , "window.u-mirror label.step { font-weight: 600; color: var(--text-soft); margin-top: 8px; }"
  , "window.u-mirror label.muted { font-size: 13px; color: var(--text-muted); }"
  , "window.u-mirror checkbutton.option-card {"
  , "  padding: 14px; border: 1px solid var(--input-border); border-radius: 8px; background: var(--surface);"
  , "}"
  , "window.u-mirror checkbutton.option-card:checked {"
  , "  border: 2px solid var(--accent); padding: 13px; background: var(--selected);"
  , "}"
  , "window.u-mirror checkbutton.option-card radio:checked {"
  , "  background: var(--accent); border-color: var(--accent); color: var(--on-accent);"
  , "}"
  , "window.u-mirror checkbutton.option-card label.name { font-weight: 600; font-size: 15px; }"
  , "window.u-mirror checkbutton.option-card label.reach { font-size: 13px; color: var(--text-muted); }"
  , "window.u-mirror box.origin { color: var(--text-muted); }"
  , "window.u-mirror label.url { color: var(--text); }"
  , "window.u-mirror box.problem {"
  , "  padding: 10px 12px; border-radius: 8px; background: var(--danger-bg); color: var(--danger);"
  , "}"
  , "window.u-mirror box.footer {"
  , "  padding: 14px 20px; background: var(--window); border-top: 1px solid var(--border);"
  , "}"
  , "window.u-mirror button.decline { padding: 2px 4px; }"
  , "window.u-mirror button.decline label.name { font-weight: 500; }"
  , "box.u-repo-pane { background: var(--surface); border-right: 1px solid var(--border); }"
  , "box.u-repo-pane box.search-bar { padding: 12px; }"
  , "list.u-repo-list { background: var(--surface); }"
  , "list.u-repo-list row {"
  , "  padding: 12px 16px; border-bottom: 1px solid var(--border-subtle);"
  , "  background: var(--surface); color: var(--text);"
  , "}"
  , "list.u-repo-list row:hover { background: var(--window); }"
  , "list.u-repo-list row:selected { background: var(--selected); color: var(--text); }"
  , "list.u-repo-list label.name { font-weight: 600; }"
  , "list.u-repo-list label.meta { font-size: 12px; color: var(--text-muted); }"
  , "box.u-repo { padding: 24px 32px; }"
  , "box.u-repo label.repo-name { font-size: 26px; font-weight: 700; }"
  , "box.u-repo label.description { font-size: 15px; color: var(--text-soft); }"
  , "box.u-repo box.u-stat { padding: 14px 12px; }"
  , "box.u-repo box.u-stat label.value { font-size: 18px; }"
  , "box.u-repo box.u-stat.branch label.value { font-family: 'JetBrains Mono', monospace; font-size: 16px; }"
  , "box.u-repo box.commit-row { padding: 10px 12px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-repo box.commit-row:last-child { border-bottom: none; }"
  , "box.u-repo label.hash { color: var(--accent); }"
  , "box.u-repo label.age { font-size: 13px; }"
  , "box.u-repo box.readme { padding: 20px; }"
  ]
