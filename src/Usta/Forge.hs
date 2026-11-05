-- | What sync asks of a forge, the same for every kind: one 'Forge' per
-- signed-in account. Adapters (GitHub, Forgejo, the sample) build it.
module Usta.Forge
  ( Forge (..)
  , Known (..)
  , Fetched (..)
  , CloneUrls (..)
  , Fork (..)
  , Failure (..)
  , LineNote (..)
  ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (UTCTime)

import Usta.Types.Branch (Branch, Tag)
import Usta.Types.Release (Release)
import Usta.Types.Search (Found)
import Usta.Types.Check (Check, Job, Run, State)
import Usta.Types.Commit (Commit, Sha)
import Usta.Types.Diff (FileDiff)
import Usta.Types.Tree (Entry)
import Usta.Types.Workflow (Workflow)
import Usta.Types.Discussion (Comment, Discussion, Feeling, Merge, NewIssue, NewPull, Verdict, Watching)
import Usta.Types.ETag (ETag)
import Usta.Types.Issue (Issue)
import Usta.Types.Label (Definition)
import Usta.Types.Listing (Counts, Listing, Paged)
import Usta.Types.Login (Login)
import Usta.Types.Milestone (Milestone)
import Usta.Types.Notification (Notification)
import Usta.Types.Number (Number)
import Usta.Types.PullRequest (PullRequest)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Readiness (Readiness, UpdateStyle)
import Usta.Types.Repository (Repository)
import Usta.Types.Reviewer (Candidate, Reviewer, Who)
import Usta.Types.Thread (Thread, Threads)
import Usta.Types.Timeline (Event)
import Usta.Types.ThreadId (ThreadId)

-- | A note on one line of a pull request's new version, sent with a
-- review; it starts a thread there.
type LineNote :: Type
data LineNote = LineNote
  { path :: Text
  , line :: Int
    -- ^ The line in the file as the pull request leaves it.
  , body :: Text
  }
  deriving stock (Eq, Show)

-- | Each list takes what sync already has of it, and answers
-- 'NotModified' when that is still current, or what changed since.
-- Calls throw 'Failure'.
type Forge :: Type
data Forge = Forge
  { notifications :: Known -> IO (Fetched [Notification])
  , pullRequests :: Known -> IO (Fetched [PullRequest])
    -- ^ The ones the account is involved in.
  , issues :: Known -> IO (Fetched [Issue])
  , repositories :: Known -> IO (Fetched [Repository])
  , repository :: RepoName -> IO Repository
    -- ^ Any one you can see, synced or not, as a link or a search finds it.
  , forkOwners :: IO [Text]
    -- ^ Where you can fork to: you first, then your organizations.
  , forks :: RepoName -> IO [RepoName]
  , parentOf :: RepoName -> IO (Maybe RepoName)
    -- ^ The repository a fork was made from.
  , fork :: RepoName -> Fork -> IO RepoName
    -- ^ The new fork's name; the forge may take a few seconds to fill it.
  , markRead :: ThreadId -> IO ()
  , markUnread :: ThreadId -> IO ()
    -- ^ Puts a notification back among the unread, for "Move to inbox".
  , cloneUrls :: RepoName -> IO CloneUrls
    -- ^ Where a mirror of the repository fetches from, as the forge says.
  , pullDiff :: RepoName -> Number -> IO [FileDiff]
    -- ^ What a pull request changes, for when the repository has no mirror.
  , pullCommits :: RepoName -> Number -> IO [Commit]
    -- ^ Its commits, oldest first.
  , threads :: RepoName -> Number -> IO Threads
  , checks :: RepoName -> Number -> IO [Check]
    -- ^ The checks on a pull request's latest commit.
  , checkLog :: RepoName -> Job -> IO Text
    -- ^ A CI job's log, as it printed it.
  , jobSteps :: RepoName -> Job -> IO (State, [(Text, State)])
    -- ^ A job's state and each of its steps', while it runs and its log
    -- is not out yet.
  , reviewers :: RepoName -> Number -> IO [Reviewer]
    -- ^ Who reviewed a pull request or is asked to, and where each stands.
  , requestReview :: RepoName -> Number -> [Who] -> IO ()
  , unrequestReview :: RepoName -> Number -> Who -> IO ()
  , readiness :: RepoName -> Number -> IO Readiness
    -- ^ What stands between a pull request and its merge, besides checks.
  , markReady :: RepoName -> Number -> IO ()
    -- ^ A draft becomes ready for review.
  , updateBranch :: RepoName -> Number -> UpdateStyle -> IO ()
    -- ^ Brings in the base branch's newer commits.
  , scheduleMerge :: RepoName -> Number -> Merge -> Bool -> IO ()
    -- ^ Merges once the checks pass; 'True' deletes the branch after.
  , cancelScheduledMerge :: RepoName -> Number -> IO ()
  , reviewCandidates :: RepoName -> Maybe Number -> IO [Candidate]
    -- ^ Who can be asked to review: with a pull request, first those who
    -- changed its files before, then collaborators, then teams.
    -- ^ Its review conversations, and when the user last reviewed it.
  , reply :: RepoName -> Number -> Thread -> Text -> IO ()
    -- ^ Adds a note to a thread.
  , recentCommits :: RepoName -> Text -> IO [Commit]
    -- ^ The last commits on a branch, newest first.
  , readme :: RepoName -> Text -> IO (Maybe (Text, Text))
    -- ^ The README at the top of a branch, with its file name.
  , commitDiff :: RepoName -> Sha -> IO [FileDiff]
    -- ^ What one commit changes.
  , discussion :: RepoName -> Number -> IO Discussion
    -- ^ An issue's or pull request's description and comments.
  , comment :: RepoName -> Number -> Text -> IO Comment
    -- ^ Posts a comment, and gives it back as the forge saved it.
  , editComment :: RepoName -> Number -> Maybe Int64 -> Text -> IO ()
    -- ^ A comment's new text, by its id; 'Nothing' for the description.
  , deleteComment :: RepoName -> Int64 -> IO ()
  , editTitle :: RepoName -> Number -> Text -> IO ()
  , watching :: RepoName -> Number -> IO Watching
  , setWatching :: RepoName -> Number -> Watching -> IO ()
    -- ^ A forge may refuse one it cannot set, saying why.
  , timeline :: RepoName -> Number -> IO [Event]
  , repoPullRequests :: RepoName -> Listing -> Maybe Text -> IO (Paged PullRequest)
    -- ^ A page of a repository's pull requests, whoever they involve; the
    -- first, or the one the last page's 'next' points at.
  , repoIssues :: RepoName -> Listing -> Maybe Text -> IO (Paged Issue)
  , repoCounts :: RepoName -> Bool -> Text -> IO Counts
    -- ^ How many pull requests ('True') or issues of each state the
    -- repository has with these words.
  , runs :: RepoName -> IO [Run]
    -- ^ A repository's latest CI runs, newest first.
  , startable :: RepoName -> Text -> IO [Workflow]
    -- ^ The workflows on a branch that can be started by hand, with their
    -- inputs.
  , dispatch :: RepoName -> Workflow -> Text -> [(Text, Text)] -> IO ()
    -- ^ Starts one on a branch, with its inputs' values.
  , rerun :: RepoName -> Int64 -> Bool -> IO ()
    -- ^ A run again, all of it or ('True') only its failed jobs.
  , cancelRun :: RepoName -> Int64 -> IO ()
  , rerunJob :: RepoName -> Job -> IO ()
  , workflows :: RepoName -> IO [Text]
    -- ^ The workflows the repository has now, by the names its runs give
    -- them: not those whose file was deleted, whose runs stay.
    -- ^ What happened besides comments, oldest first.
  , react :: RepoName -> Number -> Maybe Int64 -> Feeling -> Bool -> IO ()
    -- ^ Adds a reaction ('True') or takes it back, on a comment by its id
    -- or on the description.
  , setClosed :: RepoName -> Number -> Bool -> IO ()
    -- ^ Closes ('True') or reopens an issue or pull request.
  , merge :: RepoName -> Number -> Merge -> Bool -> IO ()
    -- ^ Merges a pull request; 'True' deletes its branch after.
  , review :: RepoName -> Number -> Verdict -> Text -> [LineNote] -> IO ()
    -- ^ Submits a review with its comment and its notes on lines.
  , discardPending :: RepoName -> Number -> IO Int
    -- ^ Deletes the user's reviews still pending on the forge, as one left
    -- on its website; how many there were.
  , createIssue :: RepoName -> NewIssue -> IO Number
  , createPull :: RepoName -> NewPull -> IO Number
  , branches :: RepoName -> IO [Text]
  , branchList :: RepoName -> IO [Branch]
    -- ^ Every branch with when it was last pushed; no counts.
  , tags :: RepoName -> IO [Tag]
  , upload :: RepoName -> Number -> Text -> ByteString -> IO Text
    -- ^ Attaches a file, by name, to an issue or pull request, and says
    -- where it is served, for a comment to link or show it.
  , search :: Text -> IO [Found]
    -- ^ Pull requests, issues and repositories the words are in, closed
    -- ones too; the forge's own search.
  , starred :: RepoName -> IO Bool
  , setStarred :: RepoName -> Bool -> IO ()
  , releases :: RepoName -> IO [Release]
    -- ^ Newest first.
  , fileHistory :: RepoName -> Text -> Text -> IO [Commit]
    -- ^ The last 50 commits on a branch that changed a file, newest first.
  , history :: RepoName -> Text -> Int -> IO [Commit]
    -- ^ A branch's history, newest first, a page of 50 at a time from 1.
  , labels :: RepoName -> IO [Definition]
    -- ^ The labels the repository defines.
  , setLabels :: RepoName -> Number -> [Text] -> IO ()
    -- ^ An issue's or pull request's labels, by name, all at once.
  , createLabel :: RepoName -> Definition -> IO ()
  , assignable :: RepoName -> IO [Candidate]
    -- ^ Who can be assigned, with their full names.
  , setAssignees :: RepoName -> Number -> [Login] -> IO ()
    -- ^ All of them at once.
  , milestones :: RepoName -> IO [Milestone]
    -- ^ Open and closed, by due date.
  , setMilestone :: RepoName -> Number -> Maybe Text -> IO ()
    -- ^ By title; 'Nothing' takes it out of any.
  , compare :: RepoName -> Text -> Text -> IO ([Commit], [Text])
    -- ^ What a branch has that the base branch does not: its commits,
    -- oldest first, and the paths they change.
  , compareDiff :: RepoName -> Text -> Text -> IO [FileDiff]
    -- ^ The same as a diff, base then branch.
  , tree :: RepoName -> Text -> IO [Entry]
    -- ^ Every folder and file on a branch, for when there is no mirror.
  , file :: RepoName -> Text -> Text -> IO ByteString
    -- ^ A file's contents on a branch, by path.
  }

-- | A repository's clone URLs. The forge gives them because only it knows
-- its address (it may sit under a path) and its ssh port.
-- | A fork to make: in an organization, or yours when 'Nothing'.
type Fork :: Type
data Fork = Fork
  { owner :: Maybe Text
  , name :: Text
  , defaultBranchOnly :: Bool
    -- ^ GitHub's; Forgejo copies every branch.
  }
  deriving stock (Eq, Show)

type CloneUrls :: Type
data CloneUrls = CloneUrls
  { https :: Text
    -- ^ The forge's own URL; plain http on a forge without TLS.
  , ssh :: Maybe Text
    -- ^ 'Nothing' when the forge has ssh turned off.
  }
  deriving stock (Eq, Show)

-- | What sync has of a list: the ETag of its copy, and when it was
-- fetched. With 'since', a forge may send only what changed after it,
-- which sync adds to its copy; without, it sends the whole list.
type Known :: Type
data Known = Known
  { etag :: Maybe ETag
  , since :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

type Fetched :: Type -> Type
data Fetched a
  = NotModified
  | Fetched
      { etag :: Maybe ETag
        -- ^ 'Nothing' when the forge sends none; the next call fetches
        -- the whole list again.
      , items :: a
      }
  deriving stock (Eq, Show)

-- | Why a call failed, as sync tells the user.
type Failure :: Type
data Failure
  = Offline
    -- ^ The forge could not be reached.
  | SignedOut
    -- ^ The forge refused the token: sign in again.
  | Refused Text
    -- ^ Anything else the forge answered with, described for the user.
  deriving stock (Eq, Show)

instance Exception Failure
