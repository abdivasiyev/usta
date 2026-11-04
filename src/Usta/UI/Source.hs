{-# LANGUAGE OverloadedStrings #-}

-- | Where the window gets what it shows: the lists, as the pages show
-- them, and word of what changed. 'fromDatabase' reads the cache; 'samples'
-- serves the mock lists, for the first-run previews. Neither signs in until
-- 'withAccounts' gives it the accounts.
module Usta.UI.Source
  ( Source (..)
  , Connected (..)
  , fromDatabase
  , samples
  , withAccounts
  , withMirrors
  , withDetails
  , tracking
  ) where

import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist.Sql (ConnectionPool, toSqlKey)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)

import Usta.Accounts (Accounts, Origins (..), describe)
import qualified Usta.Accounts as Accounts
import Usta.Bus (Bus, Change (..), publish, subscribe)
import Usta.DB (run)
import qualified Usta.DB.Cache as Cache
import Usta.DB.Account (listAccounts)
import Usta.DB.Mirror (choiceFor)
import Usta.Details (Details, From)
-- Qualified, since Source has fields of the same names.
import qualified Usta.Details as Details
import Usta.DB.Schema.Account (AccountId)
import qualified Usta.DB.Issue as DB
import qualified Usta.DB.Notification as DB
import qualified Usta.DB.PullRequest as DB
import qualified Usta.DB.Repository as DB
import Usta.Forge (Failure (..), LineNote)
import qualified Usta.Forge as Forge
import Usta.Mirrors (Mirrors (..))
import Usta.Preferences (Preferences (..))
import qualified Usta.Preferences as Preferences
import qualified Usta.Mirrors as Mirrors
import Usta.Sync (Status, Sync (..))
import Usta.Types.Account (Account (..))
import Usta.Types.Forge (Kind)
import Usta.Types.Branch (Branch, Tag)
import qualified Usta.Types.Insights as Insights
import Data.Time (UTCTime)
import Usta.Types.Check (Check, Job, Run)
import Usta.Types.Label (Definition)
import Usta.Types.Login (Login)
import Usta.Types.Milestone (Milestone)
import Usta.Types.Readiness (Readiness, UpdateStyle)
import Usta.Types.Timeline (Event)
import Usta.Types.Reviewer (Candidate, Reviewer, Who)
import Usta.Types.Commit (Commit, Sha)
import Usta.Types.Diff (FileDiff)
import Usta.Types.Tree (Entry)
import Usta.Types.Thread (Thread, Threads)
import Usta.Types.Discussion (Discussion, Feeling, Merge, NewIssue, NewPull, Verdict, Watching)
import Usta.Types.Number (Number)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Host (Host)
import Usta.Types.Mirror (Choice (..))
-- For the repositories' fields.
import qualified Usta.Types.Repository as R
import Usta.Types.Parse (parse, unparse)
-- For the pull requests' fields.
import qualified Usta.Types.PullRequest as P
import qualified Usta.Types.Notification as N
-- For the issues' fields.
import qualified Usta.Types.Issue as I
import Usta.Types.Ref (Ref (..))
import Usta.Types.Resource (Resource)
import Usta.Types.Token (Token)
import Usta.UI.Format (ago, clock)
import qualified Usta.UI.Pages.Inbox as Inbox
import qualified Usta.UI.Pages.Issues as Issues
import qualified Usta.UI.Pages.PullRequests as PullRequests
import qualified Usta.UI.Pages.Repositories as Repositories
import Usta.UI.Sidebar (Model (..))
import Usta.UI.Style (Tone (..))
import qualified Usta.UI.Sidebar as Sidebar

type Source :: Type
data Source = Source
  { inbox :: IO [Inbox.Notification]
  , pullRequests :: IO [PullRequests.PullRequest]
  , issues :: IO [Issues.Issue]
  , repositories :: IO [Repositories.Repository]
  , accounts :: IO [Connected]
  , onChange :: (Resource -> IO ()) -> IO ()
    -- ^ Runs the action on the GTK main thread whenever a list was saved
    -- anew.
  , onStatus :: (Status -> IO ()) -> IO ()
    -- ^ Likewise for each account's sync state, starting with the last
    -- one each account reported before.
  , syncNow :: IO ()
  , clearCache :: IO (Either Text ())
    -- ^ Forgets every synced list, then syncs.
  , mirrorsFolder :: IO FilePath
    -- ^ Where mirrors are kept; made if missing.
  , pruneMirrors :: IO (Either Text Int)
    -- ^ Deletes the clones no chosen mirror uses, and says how many.
  , preferences :: IO Preferences
  , setPreferences :: Preferences -> IO ()
    -- ^ Saves them, and syncs at the new pace.
  , setDone :: Inbox.Notification -> Bool -> IO (Either Text ())
    -- ^ "Mark as done" ('True', which also marks it read on the forge) or
    -- "Move to inbox" ('False', which marks it unread again).
  , signIn :: Kind -> Host -> Token -> IO (Either Text ())
    -- ^ 'Left' says, for the user, why it did not work.
  , remove :: AccountId -> IO (Either Text ())
  , mirror :: Text -> Text -> Choice -> IO (Either Text ())
    -- ^ The user's mirror answer for the repository on a host, by name.
  , onMirrorStatus :: IO () -> IO ()
    -- ^ Runs the action on the GTK main thread whenever a mirror's state
    -- changes.
  , mirrorOrigins :: Text -> Text -> IO (Either Text Repositories.Origins)
    -- ^ Where a mirror of the repository on a host, by name, would clone
    -- from; for the mirror dialog.
  , pullFiles :: Text -> Text -> Int -> IO (Either Text (From, [FileDiff]))
    -- ^ A pull request's changes, by host, repository and number.
  , pullCommits :: Text -> Text -> Int -> IO (Either Text (From, [Commit]))
  , commitDiff :: Text -> Text -> Sha -> IO (Either Text (From, [FileDiff]))
  , recentCommits :: Text -> Text -> Text -> IO (Either Text (From, [Commit]))
    -- ^ A branch's last commits, by host, repository and branch.
  , readme :: Text -> Text -> Text -> IO (Either Text (From, Maybe (Text, Text)))
  , tree :: Text -> Text -> Text -> IO (Either Text (From, [Entry]))
    -- ^ A branch's folders and files, by host, repository and branch.
  , file :: Text -> Text -> Text -> Text -> IO (Either Text (From, ByteString))
    -- ^ A file's contents, by host, repository, branch and path.
  , discussion :: Text -> Text -> Int -> IO (Either Text Discussion)
    -- ^ An issue's or pull request's thread, by host, repository and
    -- number, from the forge.
  , threads :: Text -> Text -> Int -> IO (Either Text Threads)
    -- ^ A pull request's review threads, by host, repository and number.
  , checks :: Text -> Text -> Int -> IO (Either Text [Check])
    -- ^ The checks on a pull request's latest commit.
  , checkLog :: Text -> Text -> Job -> IO (Either Text Text)
    -- ^ A CI job's log, by host and repository.
  , reviewers :: Text -> Text -> Int -> IO (Either Text [Reviewer])
  , requestReview :: Text -> Text -> Int -> [Who] -> IO (Either Text ())
  , unrequestReview :: Text -> Text -> Int -> Who -> IO (Either Text ())
  , readiness :: Text -> Text -> Int -> IO (Either Text Readiness)
  , markReady :: Text -> Text -> Int -> IO (Either Text ())
  , updateBranch :: Text -> Text -> Int -> UpdateStyle -> IO (Either Text ())
  , scheduleMerge :: Text -> Text -> Int -> Merge -> Bool -> IO (Either Text ())
  , cancelScheduledMerge :: Text -> Text -> Int -> IO (Either Text ())
  , reviewCandidates :: Text -> Text -> Int -> IO (Either Text [Candidate])
    -- ^ Who can be asked to review a pull request.
  , reply :: Text -> Text -> Int -> Thread -> Text -> IO (Either Text ())
    -- ^ Adds a note to one of them.
  , comment :: Text -> Text -> Int -> Text -> IO (Either Text ())
  , editComment :: Text -> Text -> Int -> Maybe Int64 -> Text -> IO (Either Text ())
  , deleteComment :: Text -> Text -> Int64 -> IO (Either Text ())
  , editTitle :: Text -> Text -> Int -> Text -> IO (Either Text ())
  , watching :: Text -> Text -> Int -> IO (Either Text Watching)
  , setWatching :: Text -> Text -> Int -> Watching -> IO (Either Text ())
  , timeline :: Text -> Text -> Int -> IO (Either Text [Event])
  , react :: Text -> Text -> Int -> Maybe Int64 -> Feeling -> Bool -> IO (Either Text ())
  , setClosed :: Text -> Text -> Int -> Bool -> IO (Either Text ())
  , merge :: Text -> Text -> Int -> Merge -> Bool -> IO (Either Text ())
    -- ^ 'True' deletes the branch after.
  , review :: Text -> Text -> Int -> Verdict -> Text -> [LineNote] -> IO (Either Text ())
  , createIssue :: Text -> Text -> NewIssue -> IO (Either Text Int)
    -- ^ Files it in the repository on a host, and gives its number.
  , createPull :: Text -> Text -> NewPull -> IO (Either Text Int)
  , branches :: Text -> Text -> IO (Either Text [Text])
  , labels :: Text -> Text -> IO (Either Text [Definition])
  , setLabels :: Text -> Text -> Int -> [Text] -> IO (Either Text ())
  , createLabel :: Text -> Text -> Definition -> IO (Either Text ())
  , assignable :: Text -> Text -> IO (Either Text [Candidate])
  , setAssignees :: Text -> Text -> Int -> [Login] -> IO (Either Text ())
  , milestones :: Text -> Text -> IO (Either Text [Milestone])
  , setMilestone :: Text -> Text -> Int -> Maybe Text -> IO (Either Text ())
  , compare :: Text -> Text -> Text -> Text -> IO (Either Text ([Commit], [Text]))
    -- ^ By host, repository, base branch and branch: the commits and
    -- paths a pull request between them would have.
  , repoPullRequests :: Text -> Text -> IO (Either Text [P.PullRequest])
    -- ^ A repository's latest pull requests in any state, from the forge,
    -- whoever they involve.
  , repoIssues :: Text -> Text -> IO (Either Text [I.Issue])
  , runs :: Text -> Text -> IO (Either Text [Run])
    -- ^ A repository's latest CI runs, from the forge.
  , branchList :: Text -> Text -> Text -> IO (Either Text (From, [Branch]))
    -- ^ By host, repository and default branch.
  , tagList :: Text -> Text -> IO (Either Text (From, [Tag]))
  , history :: Text -> Text -> Text -> Int -> IO (Either Text (From, [Commit]))
    -- ^ By host, repository, branch and page, from 1.
  , changes :: Text -> Text -> Text -> Maybe UTCTime -> IO (Either Text [Insights.Change])
  , graph :: Text -> Text -> [Text] -> Bool -> Int -> IO (Either Text [Insights.Node])
  , discardPending :: Text -> Text -> Int -> IO (Either Text Int)
    -- ^ Deletes the user's review left pending on the forge, by host,
    -- repository and number.
  }

-- | A signed-in account, as the window names it.
type Connected :: Type
data Connected = Connected
  { accountId :: AccountId
  , forge :: Kind
  , host :: Text
  , login :: Text
  }

-- | The lists as the cache has them, with the times formatted against the
-- clock at each read.
fromDatabase :: ConnectionPool -> Bus Change -> Bus Status -> Sync -> Source
fromDatabase pool changes statuses sync = Source
  { inbox = do
      c <- clock
      items <- run pool DB.inbox
      issues <- run pool DB.issues
      let involvementOf i = case (i.pullRequest, i.notification.subject) of
            (Just pr, _) -> pr.involvement
            (Nothing, N.IssueSubject n) ->
              concat [issue.involvement | (key, issue) <- issues, key == i.accountId, issue.ref.repo == i.notification.repo, issue.ref.number == n]
            _ -> []
      pure (mapMaybe (\i -> Inbox.fromNotification c i.forge i.notification i.pullRequest (involvementOf i) i.done) items)
  , pullRequests = do
      c <- clock
      kindOf <- kinds
      rows <- run pool DB.pullRequests
      pure [view | (key, pr) <- rows, Just kind <- [kindOf key], Just view <- [PullRequests.fromPullRequest c kind pr]]
  , issues = do
      c <- clock
      kindOf <- kinds
      rows <- run pool DB.issues
      pure [Issues.fromIssue c kind issue | (key, issue) <- rows, Just kind <- [kindOf key]]
  , repositories = do
      kindOf <- kinds
      rows <- run pool DB.repositories
      pure [Repositories.fromRepository kind r | (key, r) <- rows, Just kind <- [kindOf key]]
  , accounts = do
      rows <- run pool listAccounts
      pure [Connected {accountId = key, forge = a.forge, host = unparse a.host, login = unparse a.login} | (key, a) <- rows]
  , onChange = \act -> subscribe changes (act . (.resource))
  , onStatus = \act -> subscribe statuses act >> (sync.latest >>= mapM_ act)
  , syncNow = sync.syncNow
  , clearCache = do
      cleared <- try (run pool Cache.clear)
      case cleared of
        Left (e :: SomeException) -> pure (Left (T.pack (displayException e)))
        Right () -> do
          -- The pages show the empty lists until sync fills them again.
          accounts <- run pool listAccounts
          sequence_ [publish changes (Change key r) | (key, _) <- accounts, r <- [minBound .. maxBound]]
          sync.syncNow
          pure (Right ())
  , mirrorsFolder = Mirrors.defaultRoot >>= \root -> createDirectoryIfMissing True root >> pure root
  , pruneMirrors = do
      pruned <- try (Mirrors.defaultRoot >>= Mirrors.prune pool)
      pure (either (\(e :: IOException) -> Left (T.pack (displayException e))) Right pruned)
  , preferences = Preferences.file >>= Preferences.load
  , setPreferences = \p -> do
      path <- Preferences.file
      before <- Preferences.load path
      Preferences.save path p
      -- A new interval makes every account sync at once; a theme or
      -- notification change is no reason to.
      when (before.interval /= p.interval) $ sync.setEvery (Preferences.seconds p.interval)
  , setDone = \_ _ -> pure (Left unavailable)
  , signIn = \_ _ _ -> pure (Left unavailable)
  , remove = \_ -> pure (Left unavailable)
  , mirror = \_ _ _ -> pure (Left unavailable)
  , onMirrorStatus = const (pure ())
  , mirrorOrigins = \_ _ -> pure (Left unavailable)
  , pullFiles = \_ _ _ -> pure (Left unavailable)
  , pullCommits = \_ _ _ -> pure (Left unavailable)
  , commitDiff = \_ _ _ -> pure (Left unavailable)
  , recentCommits = \_ _ _ -> pure (Left unavailable)
  , readme = \_ _ _ -> pure (Left unavailable)
  , tree = \_ _ _ -> pure (Left unavailable)
  , file = \_ _ _ _ -> pure (Left unavailable)
  , discussion = \_ _ _ -> pure (Left unavailable)
  , threads = \_ _ _ -> pure (Left unavailable)
  , checks = \_ _ _ -> pure (Left unavailable)
  , checkLog = \_ _ _ -> pure (Left unavailable)
  , reviewers = \_ _ _ -> pure (Left unavailable)
  , requestReview = \_ _ _ _ -> pure (Left unavailable)
  , unrequestReview = \_ _ _ _ -> pure (Left unavailable)
  , readiness = \_ _ _ -> pure (Left unavailable)
  , markReady = \_ _ _ -> pure (Left unavailable)
  , updateBranch = \_ _ _ _ -> pure (Left unavailable)
  , scheduleMerge = \_ _ _ _ _ -> pure (Left unavailable)
  , cancelScheduledMerge = \_ _ _ -> pure (Left unavailable)
  , reviewCandidates = \_ _ _ -> pure (Left unavailable)
  , reply = \_ _ _ _ _ -> pure (Left unavailable)
  , comment = \_ _ _ _ -> pure (Left unavailable)
  , editComment = \_ _ _ _ _ -> pure (Left unavailable)
  , deleteComment = \_ _ _ -> pure (Left unavailable)
  , editTitle = \_ _ _ _ -> pure (Left unavailable)
  , watching = \_ _ _ -> pure (Left unavailable)
  , setWatching = \_ _ _ _ -> pure (Left unavailable)
  , timeline = \_ _ _ -> pure (Left unavailable)
  , react = \_ _ _ _ _ _ -> pure (Left unavailable)
  , setClosed = \_ _ _ _ -> pure (Left unavailable)
  , merge = \_ _ _ _ _ -> pure (Left unavailable)
  , review = \_ _ _ _ _ _ -> pure (Left unavailable)
  , createIssue = \_ _ _ -> pure (Left unavailable)
  , createPull = \_ _ _ -> pure (Left unavailable)
  , branches = \_ _ -> pure (Left unavailable)
  , labels = \_ _ -> pure (Left unavailable)
  , setLabels = \_ _ _ _ -> pure (Left unavailable)
  , createLabel = \_ _ _ -> pure (Left unavailable)
  , assignable = \_ _ -> pure (Left unavailable)
  , setAssignees = \_ _ _ _ -> pure (Left unavailable)
  , milestones = \_ _ -> pure (Left unavailable)
  , setMilestone = \_ _ _ _ -> pure (Left unavailable)
  , compare = \_ _ _ _ -> pure (Left unavailable)
  , repoPullRequests = \_ _ -> pure (Left unavailable)
  , repoIssues = \_ _ -> pure (Left unavailable)
  , runs = \_ _ -> pure (Left unavailable)
  , discardPending = \_ _ _ -> pure (Left unavailable)
  , branchList = \_ _ _ -> pure (Left unavailable)
  , tagList = \_ _ -> pure (Left unavailable)
  , history = \_ _ _ _ -> pure (Left unavailable)
  , changes = \_ _ _ _ -> pure (Left unavailable)
  , graph = \_ _ _ _ _ -> pure (Left unavailable)
  }
  where
    kinds = kindsIn pool

-- | Each account's kind of forge.
kindsIn :: ConnectionPool -> IO (AccountId -> Maybe Kind)
kindsIn pool = do
  accounts <- run pool listAccounts
  pure (\key -> (.forge) <$> lookup key accounts)

-- | The mock lists, which never change.
samples :: Source
samples = Source
  { inbox = pure Inbox.sample
  , pullRequests = pure PullRequests.sample
  , issues = pure Issues.sample
  , repositories = pure Repositories.sample
  , accounts = pure [Connected {accountId = toSqlKey n, forge = f, host = h, login = "abdivasiyev"} | (n, (f, h)) <- zip [1 ..] Sidebar.sample.forges]
  , onChange = const (pure ())
  , onStatus = const (pure ())
  , syncNow = pure ()
  , clearCache = pure (Left unavailable)
  , mirrorsFolder = Mirrors.defaultRoot
  , pruneMirrors = pure (Left unavailable)
  , preferences = pure Preferences.defaults
  , setPreferences = const (pure ())
  , setDone = \_ _ -> pure (Left unavailable)
  , signIn = \_ _ _ -> pure (Left unavailable)
  , remove = \_ -> pure (Left unavailable)
  , mirror = \_ _ _ -> pure (Left unavailable)
  , onMirrorStatus = const (pure ())
  , mirrorOrigins = \_ _ -> pure (Left unavailable)
  , pullFiles = \_ _ _ -> pure (Left unavailable)
  , pullCommits = \_ _ _ -> pure (Left unavailable)
  , commitDiff = \_ _ _ -> pure (Left unavailable)
  , recentCommits = \_ _ _ -> pure (Left unavailable)
  , readme = \_ _ _ -> pure (Left unavailable)
  , tree = \_ _ _ -> pure (Left unavailable)
  , file = \_ _ _ _ -> pure (Left unavailable)
  , discussion = \_ _ _ -> pure (Left unavailable)
  , threads = \_ _ _ -> pure (Left unavailable)
  , checks = \_ _ _ -> pure (Left unavailable)
  , checkLog = \_ _ _ -> pure (Left unavailable)
  , reviewers = \_ _ _ -> pure (Left unavailable)
  , requestReview = \_ _ _ _ -> pure (Left unavailable)
  , unrequestReview = \_ _ _ _ -> pure (Left unavailable)
  , readiness = \_ _ _ -> pure (Left unavailable)
  , markReady = \_ _ _ -> pure (Left unavailable)
  , updateBranch = \_ _ _ _ -> pure (Left unavailable)
  , scheduleMerge = \_ _ _ _ _ -> pure (Left unavailable)
  , cancelScheduledMerge = \_ _ _ -> pure (Left unavailable)
  , reviewCandidates = \_ _ _ -> pure (Left unavailable)
  , reply = \_ _ _ _ _ -> pure (Left unavailable)
  , comment = \_ _ _ _ -> pure (Left unavailable)
  , editComment = \_ _ _ _ _ -> pure (Left unavailable)
  , deleteComment = \_ _ _ -> pure (Left unavailable)
  , editTitle = \_ _ _ _ -> pure (Left unavailable)
  , watching = \_ _ _ -> pure (Left unavailable)
  , setWatching = \_ _ _ _ -> pure (Left unavailable)
  , timeline = \_ _ _ -> pure (Left unavailable)
  , react = \_ _ _ _ _ _ -> pure (Left unavailable)
  , setClosed = \_ _ _ _ -> pure (Left unavailable)
  , merge = \_ _ _ _ _ -> pure (Left unavailable)
  , review = \_ _ _ _ _ _ -> pure (Left unavailable)
  , createIssue = \_ _ _ -> pure (Left unavailable)
  , createPull = \_ _ _ -> pure (Left unavailable)
  , branches = \_ _ -> pure (Left unavailable)
  , labels = \_ _ -> pure (Left unavailable)
  , setLabels = \_ _ _ _ -> pure (Left unavailable)
  , createLabel = \_ _ _ -> pure (Left unavailable)
  , assignable = \_ _ -> pure (Left unavailable)
  , setAssignees = \_ _ _ _ -> pure (Left unavailable)
  , milestones = \_ _ -> pure (Left unavailable)
  , setMilestone = \_ _ _ _ -> pure (Left unavailable)
  , compare = \_ _ _ _ -> pure (Left unavailable)
  , repoPullRequests = \_ _ -> pure (Left unavailable)
  , repoIssues = \_ _ -> pure (Left unavailable)
  , runs = \_ _ -> pure (Left unavailable)
  , discardPending = \_ _ _ -> pure (Left unavailable)
  , branchList = \_ _ _ -> pure (Left unavailable)
  , tagList = \_ _ -> pure (Left unavailable)
  , history = \_ _ _ _ -> pure (Left unavailable)
  , changes = \_ _ _ _ -> pure (Left unavailable)
  , graph = \_ _ _ _ _ -> pure (Left unavailable)
  }

-- | For the previews and @usta --sample@, which never touch the keyring.
unavailable :: Text
unavailable = "Signing in is not available here."

-- | Signs in and out through @accounts@, and tells their forges what the
-- user did: with a notification, or on an issue or pull request. A change
-- on the forge starts a sync, so the lists show it.
withAccounts :: ConnectionPool -> Accounts -> Source -> Source
withAccounts pool accounts source = source
  { signIn = \kind host token -> either (Left . describe) (const (Right ())) <$> accounts.signIn kind host token
  , remove = \key -> either (Left . describe) Right <$> accounts.remove key
  , setDone = \n done -> do
      items <- run pool DB.inbox
      case [i | i <- items, matches n i] of
        [] -> pure (Left "That notification is not in the cache any more.")
        i : _ -> do
          forge <- accounts.forgeOf i.accountId
          let thread = i.notification.threadId
              tell f = if done then f.markRead thread else f.markUnread thread
          told <- maybe (pure (Left SignedOut)) (try . tell) forge
          case told of
            Left problem -> pure (Left (said problem))
            Right () -> do
              run pool $ do
                DB.setUnread i.accountId thread (not done)
                DB.setDone i.accountId thread done
              pure (Right ())
  , discussion = \host repo n -> onForge host repo n (\f name number -> f.discussion name number)
  , threads = \host repo n -> onForge host repo n (\f name number -> f.threads name number)
  , checks = \host repo n -> onForge host repo n (\f name number -> f.checks name number)
  , checkLog = \host repo job -> onRepo host repo (\f name -> f.checkLog name job)
  , reviewers = \host repo n -> onForge host repo n (\f name number -> f.reviewers name number)
  , requestReview = \host repo n whos -> changing (onForge host repo n (\f name number -> f.requestReview name number whos))
  , unrequestReview = \host repo n who -> changing (onForge host repo n (\f name number -> f.unrequestReview name number who))
  , readiness = \host repo n -> onForge host repo n (\f name number -> f.readiness name number)
  , markReady = \host repo n -> changing (onForge host repo n (\f name number -> f.markReady name number))
  , updateBranch = \host repo n style -> changing (onForge host repo n (\f name number -> f.updateBranch name number style))
  , scheduleMerge = \host repo n how delete -> changing (onForge host repo n (\f name number -> f.scheduleMerge name number how delete))
  , cancelScheduledMerge = \host repo n -> changing (onForge host repo n (\f name number -> f.cancelScheduledMerge name number))
  , reviewCandidates = \host repo n -> onForge host repo n (\f name number -> f.reviewCandidates name (Just number))
  , reply = \host repo n t text -> onForge host repo n (\f name number -> f.reply name number t text)
  , comment = \host repo n text -> changing (onForge host repo n (\f name number -> () <$ f.comment name number text))
  , editComment = \host repo n which text -> changing (onForge host repo n (\f name number -> f.editComment name number which text))
  , deleteComment = \host repo i -> changing (onRepo host repo (\f name -> f.deleteComment name i))
  , editTitle = \host repo n title -> changing (onForge host repo n (\f name number -> f.editTitle name number title))
  , watching = \host repo n -> onForge host repo n (\f name number -> f.watching name number)
  , setWatching = \host repo n w -> onForge host repo n (\f name number -> f.setWatching name number w)
  , timeline = \host repo n -> onForge host repo n (\f name number -> f.timeline name number)
  , react = \host repo n which feeling adding -> onForge host repo n (\f name number -> f.react name number which feeling adding)
  , setClosed = \host repo n closed -> changing (onForge host repo n (\f name number -> f.setClosed name number closed))
  , merge = \host repo n how delete -> changing (onForge host repo n (\f name number -> f.merge name number how delete))
  , review = \host repo n verdict text notes -> changing (onForge host repo n (\f name number -> f.review name number verdict text notes))
  , createIssue = \host repo new -> changing (onRepo host repo (\f name -> unparse <$> f.createIssue name new))
  , createPull = \host repo new -> changing (onRepo host repo (\f name -> unparse <$> f.createPull name new))
  , branches = \host repo -> onRepo host repo (\f name -> f.branches name)
  , labels = \host repo -> onRepo host repo (\f name -> f.labels name)
  , setLabels = \host repo n names -> changing (onForge host repo n (\f name number -> f.setLabels name number names))
  , createLabel = \host repo d -> onRepo host repo (\f name -> f.createLabel name d)
  , assignable = \host repo -> onRepo host repo (\f name -> f.assignable name)
  , setAssignees = \host repo n who -> changing (onForge host repo n (\f name number -> f.setAssignees name number who))
  , milestones = \host repo -> onRepo host repo (\f name -> f.milestones name)
  , setMilestone = \host repo n t -> changing (onForge host repo n (\f name number -> f.setMilestone name number t))
  , compare = \host repo into from -> onRepo host repo (\f name -> f.compare name into from)
  , repoPullRequests = \host repo -> onRepo host repo (\f name -> f.repoPullRequests name)
  , repoIssues = \host repo -> onRepo host repo (\f name -> f.repoIssues name)
  , runs = \host repo -> onRepo host repo (\f name -> f.runs name)
  , discardPending = \host repo n -> onForge host repo n (\f name number -> f.discardPending name number)
  }
  where
    onRepo :: Text -> Text -> (Forge.Forge -> RepoName -> IO a) -> IO (Either Text a)
    onRepo host repo act = do
      found <- accountFor pool host repo
      case found of
        Just (key, name) -> do
          forge <- accounts.forgeOf key
          case forge of
            Nothing -> pure (Left (said SignedOut))
            Just f -> either (Left . said) Right <$> try (act f name)
        Nothing -> pure (Left "That repository is not in the cache any more.")
    onForge :: Text -> Text -> Int -> (Forge.Forge -> RepoName -> Number -> IO a) -> IO (Either Text a)
    onForge host repo n act = case parse n of
      Right number -> onRepo host repo (\f name -> act f name number)
      Left _ -> pure (Left "That is not an issue or pull request number.")
    changing :: IO (Either Text a) -> IO (Either Text a)
    changing act = do
      result <- act
      either (const (pure ())) (const source.syncNow) result
      pure result
    matches n i =
      unparse i.notification.host == n.host
        && unparse i.notification.repo == n.repo
        && numberOf i.notification.subject == Just n.number
    numberOf subject = case subject of
      N.PullRequestSubject number -> Just (unparse number)
      N.IssueSubject number -> Just (unparse number)
      N.OtherSubject -> Nothing
    said problem = case problem of
      Offline -> "The forge could not be reached."
      SignedOut -> "The forge refused the account's token; sign in again."
      Refused message -> "The forge said: " <> message

-- | The repositories with their mirrors' state, choosing a mirror through
-- @accounts@, and word of each mirror's progress from @statuses@.
withMirrors :: ConnectionPool -> Accounts -> Mirrors -> Bus Mirrors.Status -> Source -> Source
withMirrors pool accounts mirrors statuses source = source
  { repositories = do
      c <- clock
      kindOf <- kindsIn pool
      rows <- run pool DB.repositories
      latest <- mirrors.latest
      sequence
        [ do
            choice <- run pool (choiceFor key r.name)
            let state = [s.state | s <- latest, s.accountId == key, s.repo == r.name]
            pure (withState c choice (listToMaybe (reverse state)) (Repositories.fromRepository kind r))
        | (key, r) <- rows, Just kind <- [kindOf key]
        ]
  , mirror = \host name choice -> do
      found <- repoOf host name
      case found of
        Just (key, repo) -> either (Left . describe) Right <$> accounts.mirror key repo choice
        Nothing -> pure (Left gone)
  , mirrorOrigins = \host name -> do
      found <- repoOf host name
      case found of
        Nothing -> pure (Left gone)
        Just (key, repo) -> do
          home <- getHomeDirectory
          root <- Mirrors.defaultRoot
          let keptIn = maybe (T.pack root) ("~" <>) (T.stripPrefix (T.pack home) (T.pack root))
          asked <- accounts.origins key repo
          pure $ case asked of
            Left problem -> Left (describe problem)
            Right o -> Right Repositories.Origins
              { https = either (Left . describe) Right o.https
              , ssh = either (Left . describe) Right o.ssh
              , keptIn = keptIn
              }
  , onMirrorStatus = \act -> subscribe statuses (const act)
  }
  where
    repoOf host name = do
      rows <- run pool DB.repositories
      pure $ case [(key, r.name) | (key, r) <- rows, unparse r.host == host, unparse r.name == name] of
        found : _ -> Just found
        [] -> Nothing
    gone = "That repository is not in the cache any more."
    withState c choice state view = case (choice, state) of
      (Nothing, _) -> view {Repositories.canMirror = True}
      (Just Declined, _) -> view
        { Repositories.mirror = (Neutral, "API only")
        , Repositories.mirrorDetail = Just "Not mirrored, as you chose: diffs and history come from the forge's API."
        , Repositories.canMirror = True
        }
      (Just (Mirrored _ _), Just Mirrors.Cloning) -> view {Repositories.mirror = (Accent, "Cloning…")}
      (Just (Mirrored _ _), Just Mirrors.Fetching) -> view {Repositories.mirror = (Accent, "Fetching…")}
      (Just (Mirrored _ _), Just (Mirrors.Ready at)) ->
        view {Repositories.mirror = (Success, "Up to date"), Repositories.mirrorDetail = Just ("Fetched " <> ago c at)}
      -- A failed mirror can be chosen again: another transport, or the API.
      (Just (Mirrored _ _), Just (Mirrors.Failed message)) ->
        view {Repositories.mirror = (Danger, "Failed"), Repositories.mirrorDetail = Just message, Repositories.canMirror = True}
      (Just (Mirrored _ _), Nothing) -> view {Repositories.mirror = (Neutral, "Waiting to fetch")}

-- | Every call that waits on a forge or a mirror run inside @track@, so
-- the window can show that something is on its way.
tracking :: (forall a. IO a -> IO a) -> Source -> Source
tracking track s = s
  { setDone = \n d -> track (s.setDone n d)
  , mirrorOrigins = \h r -> track (s.mirrorOrigins h r)
  , pullFiles = \h r n -> track (s.pullFiles h r n)
  , pullCommits = \h r n -> track (s.pullCommits h r n)
  , commitDiff = \h r x -> track (s.commitDiff h r x)
  , recentCommits = \h r b -> track (s.recentCommits h r b)
  , readme = \h r b -> track (s.readme h r b)
  , tree = \h r b -> track (s.tree h r b)
  , file = \h r b p -> track (s.file h r b p)
  , discussion = \h r n -> track (s.discussion h r n)
  , threads = \h r n -> track (s.threads h r n)
  , checks = \h r n -> track (s.checks h r n)
  , checkLog = \h r j -> track (s.checkLog h r j)
  , reviewers = \h r n -> track (s.reviewers h r n)
  , requestReview = \h r n w -> track (s.requestReview h r n w)
  , unrequestReview = \h r n w -> track (s.unrequestReview h r n w)
  , readiness = \h r n -> track (s.readiness h r n)
  , markReady = \h r n -> track (s.markReady h r n)
  , updateBranch = \h r n st -> track (s.updateBranch h r n st)
  , scheduleMerge = \h r n how d -> track (s.scheduleMerge h r n how d)
  , cancelScheduledMerge = \h r n -> track (s.cancelScheduledMerge h r n)
  , reviewCandidates = \h r n -> track (s.reviewCandidates h r n)
  , reply = \h r n t text -> track (s.reply h r n t text)
  , comment = \h r n t -> track (s.comment h r n t)
  , editComment = \h r n w t -> track (s.editComment h r n w t)
  , deleteComment = \h r i -> track (s.deleteComment h r i)
  , editTitle = \h r n t -> track (s.editTitle h r n t)
  , watching = \h r n -> track (s.watching h r n)
  , setWatching = \h r n w -> track (s.setWatching h r n w)
  , timeline = \h r n -> track (s.timeline h r n)
  , react = \h r n w f a -> track (s.react h r n w f a)
  , setClosed = \h r n c -> track (s.setClosed h r n c)
  , merge = \h r n m d -> track (s.merge h r n m d)
  , review = \h r n v t notes -> track (s.review h r n v t notes)
  , createIssue = \h r x -> track (s.createIssue h r x)
  , createPull = \h r x -> track (s.createPull h r x)
  , branches = \h r -> track (s.branches h r)
  , labels = \h r -> track (s.labels h r)
  , setLabels = \h r n l -> track (s.setLabels h r n l)
  , createLabel = \h r d -> track (s.createLabel h r d)
  , assignable = \h r -> track (s.assignable h r)
  , setAssignees = \h r n w -> track (s.setAssignees h r n w)
  , milestones = \h r -> track (s.milestones h r)
  , setMilestone = \h r n t -> track (s.setMilestone h r n t)
  , compare = \h r a b -> track (s.compare h r a b)
  , repoPullRequests = \h r -> track (s.repoPullRequests h r)
  , repoIssues = \h r -> track (s.repoIssues h r)
  , runs = \h r -> track (s.runs h r)
  , discardPending = \h r n -> track (s.discardPending h r n)
  , branchList = \h r b -> track (s.branchList h r b)
  , tagList = \h r -> track (s.tagList h r)
  , history = \h r b n -> track (s.history h r b n)
  , changes = \h r b t -> track (s.changes h r b t)
  , graph = \h r b f n -> track (s.graph h r b f n)
  , clearCache = track s.clearCache
  , pruneMirrors = track s.pruneMirrors
  }

-- | Pull requests' and branches' contents through @details@, each found by
-- the account that has its repository in the cache.
withDetails :: ConnectionPool -> Details -> Source -> Source
withDetails pool details source = source
  { pullFiles = \host repo n -> pullOf host repo n details.pullFiles
  , pullCommits = \host repo n -> pullOf host repo n details.pullCommits
  , commitDiff = \host repo sha -> repoOf host repo (\key name -> details.commitDiff key name sha)
  , recentCommits = \host repo branch -> repoOf host repo (\key name -> details.recentCommits key name branch)
  , readme = \host repo branch -> repoOf host repo (\key name -> details.readme key name branch)
  , tree = \host repo branch -> repoOf host repo (\key name -> details.tree key name branch)
  , file = \host repo branch path -> repoOf host repo (\key name -> details.file key name branch path)
  , branchList = \host repo base -> repoOf host repo (\key name -> details.branchList key name base)
  , tagList = \host repo -> repoOf host repo (\key name -> details.tagList key name)
  , history = \host repo branch n -> repoOf host repo (\key name -> details.history key name branch n)
  , changes = \host repo branch since -> repoOf host repo (\key name -> details.changes key name branch since)
  , graph = \host repo scope firstParent n -> repoOf host repo (\key name -> details.graph key name scope firstParent n)
  }
  where
    pullOf :: Text -> Text -> Int -> (AccountId -> RepoName -> Number -> IO (Either Text a)) -> IO (Either Text a)
    pullOf host repo n ask = case parse n of
      Left _ -> pure (Left gone)
      Right number -> repoOf host repo (\key name -> ask key name number)
    -- The repository may be the user's own or one they only have pull
    -- requests in.
    repoOf :: Text -> Text -> (AccountId -> RepoName -> IO (Either Text a)) -> IO (Either Text a)
    repoOf host repo ask = accountFor pool host repo >>= maybe (pure (Left gone)) (uncurry ask)
    gone = "That repository is not in the cache any more."

-- | The account that has the repository on a host in the cache: as its
-- own, or through a pull request or issue in it.
accountFor :: ConnectionPool -> Text -> Text -> IO (Maybe (AccountId, RepoName))
accountFor pool host repo = do
  repos <- run pool DB.repositories
  prs <- run pool DB.pullRequests
  issues <- run pool DB.issues
  let keys = [key | (key, r) <- repos, unparse r.host == host, unparse r.name == repo]
        <> [key | (key, pr) <- prs, unparse pr.ref.host == host, unparse pr.ref.repo == repo]
        <> [key | (key, i) <- issues, unparse i.ref.host == host, unparse i.ref.repo == repo]
  pure $ case (keys, parse repo) of
    (key : _, Right name) -> Just (key, name)
    _ -> Nothing
