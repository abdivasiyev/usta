{-# LANGUAGE OverloadedStrings #-}

-- | A forge with no network: the pages' mock content, served as a forge
-- would serve it. For @usta --sample@ and tests. It changes only when told
-- to ('advance'), and fails only when told to ('failWith'), so a test knows
-- what every call answers.
module Usta.Forge.Sample
  ( Sample (..)
  , new
  ) where

import Control.Exception (throwIO)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime)

import Usta.Forge (Failure, Fetched (..))
import qualified Usta.Forge as F
import Usta.Types.Account (Account (..))
import Usta.Types.ETag (ETag)
import Usta.Types.Host (Host)
import Usta.Types.Involvement (Involvement (..))
import qualified Usta.Types.Discussion as D
import qualified Usta.Types.Issue as I
import qualified Usta.Types.Label as L
import qualified Usta.Types.Notification as N
import Usta.Types.Parse (parseM, unparse)
import qualified Usta.Types.PullRequest as P
import Usta.Types.Ref (Ref (..))
import qualified Usta.Types.Repository as R
import Usta.Types.Resource (Resource (..))

type Sample :: Type
data Sample = Sample
  { forge :: F.Forge
    -- ^ Serves only the items on the account's host.
  , advance :: UTCTime -> IO Bool
    -- ^ Makes the next scripted change, dated at the given time; 'False'
    -- once the script is done. A change on another host leaves this
    -- account's lists, and their ETags, as they were.
  , failWith :: Maybe Failure -> IO ()
    -- ^ Every call throws the failure until it is cleared with 'Nothing'.
  }

type State :: Type
data State = State
  { notifications :: [N.Notification]
  , pullRequests :: [P.PullRequest]
  , issues :: [I.Issue]
  , repositories :: [R.Repository]
  , versions :: [(Resource, Int)]
    -- ^ Bumped when a resource's list changes; its ETag is made from it.
  , script :: [Step]
  , failure :: Maybe Failure
  }

-- | A scripted change, dated at the time it is made.
type Step :: Type
type Step = UTCTime -> State -> State

new :: UTCTime -> Account -> IO Sample
new now account = do
  everything <- content now
  steps <- changes
  ref <- newIORef (mine account.host everything) {script = map (\step time -> mine account.host . step time) steps}
  pure Sample
    { forge = F.Forge
        { F.notifications = fetch ref Notifications (.notifications)
        , F.pullRequests = fetch ref PullRequests (.pullRequests)
        , F.issues = fetch ref Issues (.issues)
        , F.repositories = fetch ref Repositories (.repositories)
        , F.cloneUrls = \repo -> pure F.CloneUrls
            { F.https = "https://" <> unparse account.host <> "/" <> unparse repo <> ".git"
            , F.ssh = Just ("git@" <> unparse account.host <> ":" <> unparse repo <> ".git")
            }
        -- The sample has no code: its pull requests change nothing.
        , F.pullDiff = \_ _ -> pure []
        , F.pullCommits = \_ _ -> pure []
        , F.recentCommits = \_ _ -> pure []
        , F.readme = \_ _ -> pure Nothing
        , F.commitDiff = \_ _ -> pure []
        , F.markRead = \thread -> do
            failIfTold ref
            atomicModifyIORef' ref $ \s ->
              (bumped s s {notifications = [n {N.unread = n.unread && n.threadId /= thread} | n <- s.notifications]}, ())
        -- Discussions are the items' own: no description, no comments,
        -- and nothing can be posted.
        , F.discussion = \repo n -> do
            failIfTold ref
            s <- readIORef ref
            let opened = [(i.author, i.createdAt, i.closedAt /= Nothing, False) | i <- s.issues, i.ref.repo == repo, i.ref.number == n]
                  <> [(p.author, p.createdAt, p.state `elem` [P.Merged, P.Closed], p.state == P.Merged) | p <- s.pullRequests, p.ref.repo == repo, p.ref.number == n]
            case opened of
              (author, at, closed, merged) : _ -> pure D.Discussion
                { D.opening = D.Comment {D.author = author, D.body = "", D.createdAt = at}
                , D.comments = []
                , D.closed = closed
                , D.merged = merged
                , D.assignees = []
                , D.labels = []
                , D.milestone = Nothing
                }
              [] -> throwIO (F.Refused "No such issue or pull request.")
        , F.comment = \_ _ _ -> refuse
        , F.setClosed = \_ _ _ -> refuse
        , F.merge = \_ _ _ _ -> refuse
        , F.review = \_ _ _ _ -> refuse
        , F.markUnread = \thread -> do
            failIfTold ref
            atomicModifyIORef' ref $ \s ->
              (bumped s s {notifications = [n {N.unread = n.unread || n.threadId == thread} | n <- s.notifications]}, ())
        }
    , advance = \time -> atomicModifyIORef' ref $ \s -> case s.script of
        [] -> (s, False)
        step : rest -> (bumped s (step time s) {script = rest}, True)
    , failWith = \f -> atomicModifyIORef' ref $ \s -> (s {failure = f}, ())
    }

fetch :: IORef State -> Resource -> (State -> [a]) -> Maybe ETag -> IO (Fetched [a])
fetch ref resource items known = do
  failIfTold ref
  s <- readIORef ref
  tag <- parseM ("\"sample-" <> T.pack (show (version resource s)) <> "\"")
  pure $ if known == Just tag then NotModified else Fetched {etag = Just tag, items = items s}

-- | The sample takes no changes.
refuse :: IO a
refuse = throwIO (F.Refused "The sample forge takes no changes.")

failIfTold :: IORef State -> IO ()
failIfTold ref = readIORef ref >>= \s -> for_ s.failure throwIO

version :: Resource -> State -> Int
version resource s = fromMaybe 0 (lookup resource s.versions)

-- | @after@ with the versions of the lists that differ from @before@ bumped.
bumped :: State -> State -> State
bumped before after = after {versions = [(r, version r before + if differs r then 1 else 0) | r <- [minBound ..]]}
  where
    differs r = case r of
      Notifications -> before.notifications /= after.notifications
      PullRequests -> before.pullRequests /= after.pullRequests
      Issues -> before.issues /= after.issues
      Repositories -> before.repositories /= after.repositories

-- | Only the items on one host.
mine :: Host -> State -> State
mine host s = s
  { notifications = filter ((== host) . (.host)) s.notifications
  , pullRequests = filter ((== host) . (.ref.host)) s.pullRequests
  , issues = filter ((== host) . (.ref.host)) s.issues
  , repositories = filter ((== host) . (.host)) s.repositories
  }

-- | The mock content the pages show, dated back from @now@.
content :: UTCTime -> IO State
content now = do
  usta <- repository "github.com" "abdivasiyev/usta" (Just "A desktop client to connect with your forge") 2 3 False
  homelab <- repository "git.azizovich.uz" "abdivasiyev/homelab" (Just "NixOS homelab") 2 1 True
  nix <- repository "git.azizovich.uz" "abdivasiyev/nix" (Just "Shell and editor settings") 2 1 False

  pr2 <- pullRequest "github.com" "abdivasiyev/usta" 2 "feat: add gi-gtk4 dependency" "feat/gtk4-dep"
           P.Open (Just P.ChecksPassed) (Just P.AwaitingReview) 1 [AsReviewer] 5 5
  pr6 <- pullRequest "github.com" "abdivasiyev/usta" 6 "feat: render main window with GTK4" "feat/main-window"
           P.Draft Nothing Nothing 0 [AsAuthor] (3 * day) (2 * day)
  pr1 <- pullRequest "github.com" "abdivasiyev/usta" 1 "feat: scaffold Nix flake and Hello world app" "feat/scaffold"
           P.Merged (Just P.ChecksPassed) (Just P.Approved) 2 [AsAuthor] (5 * day) (3 * 60)
  pr48 <- pullRequest "git.azizovich.uz" "abdivasiyev/homelab" 48 "chore: move cache priorities into nix.settings" "cache-priorities"
            P.Open (Just P.ChecksFailed) (Just P.AwaitingReview) 0 [AsReviewer] 22 22
  pr47 <- pullRequest "git.azizovich.uz" "abdivasiyev/homelab" 47 "feat: add binary cache health check" "cache-health"
            P.Open (Just P.ChecksRunning) Nothing 2 [AsAuthor] (2 * day) day
  pr13 <- pullRequest "git.azizovich.uz" "abdivasiyev/nix" 13 "fix: restart nix-daemon after config change" "daemon-restart"
            P.Open (Just P.ChecksPassed) (Just P.Approved) 3 [AsAuthor] (4 * day) (2 * day)
  pr21 <- pullRequest "git.azizovich.uz" "abdivasiyev/nix" 21 "refactor: move every host to den aspects" "refactor/den-aspects"
            P.Open (Just P.ChecksRunning) (Just P.AwaitingReview) 0 [AsReviewer] (2 * 60) 60

  gtk <- label "gtk" "1d76db"
  bug <- label "bug" "d73a4a"
  forgeApi <- label "forge api" "7057ff"
  nixLabel <- label "nix" "cfd3d7"
  i5 <- issue "github.com" "abdivasiyev/usta" 5 "Render the main window with GTK4" [gtk] 2 [AsAssignee] day 60 Nothing
  i3 <- issue "github.com" "abdivasiyev/usta" 3 "Diff view: keep scroll position when switching files" [bug] 1 [AsAssignee, AsMentioned] (2 * day) 60 Nothing
  i4 <- issue "github.com" "abdivasiyev/usta" 4 "Support GitLab merge request discussions" [forgeApi] 0 [AsAssignee] (2 * day) (2 * day) Nothing
  i1 <- issue "github.com" "abdivasiyev/usta" 1 "Pick a name and a logo" [] 6 [AsAuthor] (8 * day) (5 * day) (Just (5 * day))
  i12 <- issue "git.azizovich.uz" "abdivasiyev/nix" 12 "Restart nix-daemon after substituter changes" [nixLabel] 3 [AsAuthor] (3 * day) day Nothing
  i30 <- issue "git.azizovich.uz" "abdivasiyev/homelab" 30 "Binary cache answers as not a cache" [bug] 4 [AsMentioned] (8 * day) (6 * day) Nothing

  n1 <- notification "1" "github.com" "abdivasiyev/usta" (N.PullRequestSubject <$> parseM 2) "feat: add gi-gtk4 dependency" (Just N.ReviewRequested) True 5
  n2 <- notification "2" "git.azizovich.uz" "abdivasiyev/homelab" (N.PullRequestSubject <$> parseM 48) "chore: move cache priorities into nix.settings" Nothing True 22
  n3 <- notification "3" "github.com" "abdivasiyev/usta" (N.IssueSubject <$> parseM 3) "Diff view: keep scroll position when switching files" (Just N.Mentioned) True 60
  n4 <- notification "4" "github.com" "abdivasiyev/usta" (N.PullRequestSubject <$> parseM 1) "feat: scaffold Nix flake and Hello world app" (Just N.StateChanged) False (3 * 60)
  n5 <- notification "5" "git.azizovich.uz" "abdivasiyev/nix" (N.IssueSubject <$> parseM 12) "Restart nix-daemon after substituter changes" Nothing False day
  n6 <- notification "6" "git.azizovich.uz" "abdivasiyev/homelab" (N.PullRequestSubject <$> parseM 47) "feat: add binary cache health check" Nothing False day
  n7 <- notification "7" "github.com" "abdivasiyev/usta" (N.IssueSubject <$> parseM 4) "Support GitLab merge request discussions" (Just N.Assigned) False (2 * day)

  pure State
    { notifications = [n1, n2, n3, n4, n5, n6, n7]
    , pullRequests = [pr2, pr6, pr1, pr48, pr47, pr13, pr21]
    , issues = [i5, i3, i4, i1, i12, i30]
    , repositories = [usta, homelab, nix]
    , versions = []
    , script = []
    , failure = Nothing
    }
  where
    day = 24 * 60
    ago :: NominalDiffTime -> UTCTime
    ago minutes = addUTCTime (negate (60 * minutes)) now

    repository host name description prs open private = do
      h <- parseM host
      n <- parseM name
      prCount <- parseM prs
      issueCount <- parseM open
      pure R.Repository
        { host = h, name = n, description = description, defaultBranch = "master", private = private
        , openPullRequests = prCount, openIssues = issueCount, updatedAt = ago 60 }

    pullRequest host repo number title branch state checks review comments involvement created updated = do
      r <- refOf host repo number
      author <- parseM "abdivasiyev"
      c <- parseM comments
      pure P.PullRequest
        { ref = r, title = title, author = author, headBranch = branch, baseBranch = "master"
        , state = state, checks = checks, review = review, comments = c
        , changedFiles = Nothing, commits = Nothing, involvement = involvement
        , createdAt = ago created, updatedAt = ago updated }

    label name color = do
      c <- parseM color
      pure L.Label {name = name, color = c}

    issue host repo number title labels comments involvement created updated closed = do
      r <- refOf host repo number
      author <- parseM "abdivasiyev"
      c <- parseM comments
      pure I.Issue
        { ref = r, title = title, author = author, labels = labels, comments = c
        , involvement = involvement, createdAt = ago created, updatedAt = ago updated
        , closedAt = fmap ago closed }

    notification thread host repo subject title reason unread updated = do
      t <- parseM thread
      h <- parseM host
      r <- parseM repo
      s <- subject
      pure N.Notification
        { threadId = t, host = h, repo = r, subject = s, title = title
        , reason = reason, unread = unread, updatedAt = ago updated }

refOf :: Text -> Text -> Int -> IO Ref
refOf host repo number = Ref <$> parseM host <*> parseM repo <*> parseM number

-- | What 'advance' does, in order: checks finish, a review is requested,
-- an issue is closed.
changes :: IO [Step]
changes = do
  pr8 <- refOf "github.com" "abdivasiyev/usta" 8
  thread <- parseM "8"
  author <- parseM "abdivasiyev"
  none <- parseM 0
  closed <- refOf "github.com" "abdivasiyev/usta" 3
  health <- refOf "git.azizovich.uz" "abdivasiyev/homelab" 47
  pure
    [ \time s -> s
        { pullRequests =
            [ if pr.ref == health then pr {P.checks = Just P.ChecksPassed, P.updatedAt = time} else pr
            | pr <- s.pullRequests ] }
    , \time s -> s
        { notifications = N.Notification
            { threadId = thread, host = pr8.host, repo = pr8.repo, subject = N.PullRequestSubject pr8.number
            , title = "feat: sync engine", reason = Just N.ReviewRequested, unread = True, updatedAt = time }
            : s.notifications
        , pullRequests = P.PullRequest
            { ref = pr8, title = "feat: sync engine", author = author, headBranch = "feat/sync", baseBranch = "master"
            , state = P.Open, checks = Just P.ChecksRunning, review = Just P.AwaitingReview, comments = none
            , changedFiles = Nothing, commits = Nothing, involvement = [AsReviewer]
            , createdAt = time, updatedAt = time }
            : s.pullRequests }
    , \time s -> s
        { issues =
            [ if i.ref == closed then i {I.closedAt = Just time, I.updatedAt = time} else i
            | i <- s.issues ] }
    ]
