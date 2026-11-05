{-# LANGUAGE OverloadedStrings #-}

-- | GitHub's API as usta uses it: REST routes for the user and the
-- notifications, which carry ETags, and GraphQL for the lists, which
-- bring a pull request's checks, review and counts in the same answer.
-- Each GraphQL query is a fixed text in this module and each answer a
-- record of its own; the variables are the only part that changes.
module Usta.Forge.GitHub.Api
  ( Routes (..)
  , api
  , Graph (..)
  , graph
  , NotificationQuery (..)
  , User (..)
  , Thread (..)
  , Subject (..)
  , RepositoryRef (..)
  , GraphQuery (..)
  , Answer (..)
  , Page (..)
  , PullNode (..)
  , IssueNode (..)
  , RepoNode (..)
  , Repos (..)
  , LabelNode (..)
  , ListQuery (..)
  , RepoInfo (..)
  , Body (..)
  , RestComment (..)
  , RestReaction (..)
  , ReactionBody (..)
  , RestLabel (..)
  , LabelNames (..)
  , RestMilestone (..)
  , Created (..)
  , Ignored (..)
  , DiscussionNode (..)
  , CommentNode (..)
  , ReviewText (..)
  , ReactionGroup (..)
  , Item (..)
  , TimelineNode (..)
  , GitHubDiff
  , GitHubRaw
  , LogText
  , CommitsQuery (..)
  , RestCommit (..)
  , BaseHead (..)
  , Comparison (..)
  , Named' (..)
  , Tree (..)
  , TreeEntry (..)
  , RestRelease (..)
  , RestAsset (..)
  , WorkflowRuns (..)
  , WorkflowRun (..)
  , Jobs (..)
  , Workflows (..)
  , RestJob (..)
  , JobSteps (..)
  , RefPage (..)
  , RefNode (..)
  , Checks (..)
  , CheckNode (..)
  , FoundNode (..)
  , Team (..)
  , PullState (..)
  , ReviewNode (..)
  , ThreadNode (..)
  , NoteNode (..)
  , Counted (..)
  , countsQuery
  , pullStateQuery
  , pullMutation
  , refsQuery
  , checksQuery
  , foundQuery
  , feelingName
  , feelingOf
  , discussionQuery
  , timelineQuery
  , subscriptionMutation
  , searchQuery
  , pullFields
  , issueFields
  , reposQuery
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client (AsClientT, ClientM, client)

import Usta.Types.Count (Count)
import Usta.Types.Discussion (Feeling (..))
import Usta.Types.ETag (ETag)
import Usta.Types.Label (Color)
import Usta.Types.Parse (unparse)
import Usta.Types.Login (Login)
import Usta.Types.Number (Number)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Servant.NamedQueryParam (Named (..), RecordQueryParam, ToQuery)
import Usta.Types.ThreadId (ThreadId)

-- | The REST API, at @https://api.github.com@ (or a GitHub Enterprise
-- server's @/api/v3@).
type Routes :: Type -> Type
data Routes mode = Routes
  { me :: mode :- "user" :> Get '[JSON] User
  , notifications
      :: mode :- "notifications" :> RecordQueryParam NotificationQuery :> Header "If-None-Match" ETag
           :> Get '[JSON] (Headers '[Header "ETag" ETag] [Thread])
    -- ^ With the last ETag, GitHub answers 304 when nothing changed, and
    -- that answer does not count against the rate limit.
  , markThread :: mode :- "notifications" :> "threads" :> Capture "id" ThreadId :> Verb 'PATCH 205 '[JSON] NoContent
  , repository :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> Get '[JSON] RepoInfo
  , postComment
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "number" Number
           :> "comments" :> ReqBody '[JSON] Body :> PostCreated '[JSON] RestComment
  , editComment
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> "comments" :> Capture "id" Int64
           :> ReqBody '[JSON] Body :> Patch '[JSON] RestComment
  , deleteComment
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> "comments" :> Capture "id" Int64
           :> DeleteNoContent
  , editIssue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "number" Number
           :> ReqBody '[JSON] Value :> Patch '[JSON] Ignored
    -- ^ Only the fields given change: title, body, state, assignees,
    -- milestone (null takes it out).
  , issueReactions
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "number" Number
           :> "reactions" :> QueryParam "content" Text :> QueryParam "per_page" Int :> Get '[JSON] [RestReaction]
  , reactToIssue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "number" Number
           :> "reactions" :> ReqBody '[JSON] ReactionBody :> PostCreated '[JSON] Ignored
    -- ^ 201 for a new reaction, 200 when it was there: run it with
    -- 'Usta.Forge.Http.runDone'.
  , unreactToIssue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "number" Number
           :> "reactions" :> Capture "reaction" Int64 :> DeleteNoContent
  , commentReactions
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> "comments" :> Capture "id" Int64
           :> "reactions" :> QueryParam "content" Text :> QueryParam "per_page" Int :> Get '[JSON] [RestReaction]
  , reactToComment
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> "comments" :> Capture "id" Int64
           :> "reactions" :> ReqBody '[JSON] ReactionBody :> PostCreated '[JSON] Ignored
    -- ^ 201 for a new reaction, 200 when it was there: run it with
    -- 'Usta.Forge.Http.runDone'.
  , unreactToComment
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> "comments" :> Capture "id" Int64
           :> "reactions" :> Capture "reaction" Int64 :> DeleteNoContent
  , labels
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "labels" :> RecordQueryParam ListQuery
           :> Get '[JSON] [RestLabel]
  , setLabels
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "number" Number
           :> "labels" :> ReqBody '[JSON] LabelNames :> Put '[JSON] Ignored
  , createLabel
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "labels" :> ReqBody '[JSON] RestLabel
           :> PostCreated '[JSON] Ignored
  , assignees
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "assignees" :> RecordQueryParam ListQuery
           :> Get '[JSON] [User]
  , milestones
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "milestones" :> QueryParam "state" Text
           :> RecordQueryParam ListQuery :> Get '[JSON] [RestMilestone]
  , createIssue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> ReqBody '[JSON] Value
           :> PostCreated '[JSON] Created
  , createPull
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> ReqBody '[JSON] Value
           :> PostCreated '[JSON] Created
  , requestReviewers
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> "requested_reviewers" :> ReqBody '[JSON] Value :> PostCreated '[JSON] Ignored
  , unrequestReviewers
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> "requested_reviewers" :> ReqBody '[JSON] Value :> Delete '[JSON] Ignored
  , pullDiff
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> Get '[GitHubDiff] Text
  , pullCommits
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> "commits" :> RecordQueryParam ListQuery :> Get '[JSON] [RestCommit]
  , commitDiff
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "commits" :> Capture "sha" Text
           :> Get '[GitHubDiff] Text
  , commits
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "commits" :> RecordQueryParam CommitsQuery
           :> Get '[JSON] [RestCommit]
  , compare
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "compare" :> Capture "basehead" BaseHead
           :> Get '[JSON] Comparison
  , compareDiff
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "compare" :> Capture "basehead" BaseHead
           :> Get '[GitHubDiff] Text
  , readme
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "readme" :> QueryParam "ref" Text
           :> Get '[JSON] Named'
  , tree
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "git" :> "trees" :> Capture "ref" Text
           :> QueryParam "recursive" Int :> Get '[JSON] Tree
  , file
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "contents" :> CaptureAll "path" Text
           :> QueryParam "ref" Text :> Get '[GitHubRaw] ByteString
  , releases
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "releases" :> RecordQueryParam ListQuery
           :> Get '[JSON] [RestRelease]
  , runs
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "runs" :> RecordQueryParam ListQuery
           :> Get '[JSON] WorkflowRuns
  , workflows
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "workflows"
           :> RecordQueryParam ListQuery :> Get '[JSON] Workflows
  , dispatch
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "workflows" :> Capture "workflow" Text
           :> "dispatches" :> ReqBody '[JSON] Value :> PostNoContent
  , rerun
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "runs" :> Capture "run" Int64
           :> "rerun" :> PostNoContent
  , rerunFailed
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "runs" :> Capture "run" Int64
           :> "rerun-failed-jobs" :> PostNoContent
  , cancelRun
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "runs" :> Capture "run" Int64
           :> "cancel" :> PostNoContent
  , rerunJob
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "jobs" :> Capture "job" Int64
           :> "rerun" :> PostNoContent
    -- ^ These four answer 201 or 202: run them with 'Usta.Forge.Http.runDone'.
  , runJobs
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "runs" :> Capture "run" Int64
           :> "jobs" :> Get '[JSON] Jobs
  , job
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "jobs" :> Capture "job" Int64
           :> Get '[JSON] JobSteps
  , jobLog
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "actions" :> "jobs" :> Capture "job" Int64
           :> "logs" :> Get '[LogText] ByteString
    -- ^ GitHub answers with a redirect to the log, which the client follows.
  , review
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> "reviews" :> ReqBody '[JSON] Value :> Post '[JSON] Ignored
  , deleteReview
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> "reviews" :> Capture "review" Int64 :> Delete '[JSON] Ignored
  , replyToComment
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> "comments" :> Capture "comment" Int64 :> "replies" :> ReqBody '[JSON] Body :> PostCreated '[JSON] Ignored
  , merge
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> "merge" :> ReqBody '[JSON] Value :> Put '[JSON] Ignored
  , updateBranch
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "number" Number
           :> "update-branch" :> ReqBody '[JSON] Value :> Verb 'PUT 202 '[JSON] Ignored
  , deleteBranch
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "git" :> "refs" :> "heads" :> CaptureAll "branch" Text
           :> DeleteNoContent
  , teams :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "teams" :> Get '[JSON] [Team]
  , starred :: mode :- "user" :> "starred" :> Capture "owner" Text :> Capture "repo" Text :> GetNoContent
    -- ^ 204 when starred, 404 when not.
  , star :: mode :- "user" :> "starred" :> Capture "owner" Text :> Capture "repo" Text :> PutNoContent
  , unstar :: mode :- "user" :> "starred" :> Capture "owner" Text :> Capture "repo" Text :> DeleteNoContent
  }
  deriving stock (Generic)

api :: Routes (AsClientT ClientM)
api = client (Proxy @(NamedRoutes Routes))

-- | The GraphQL API, at @https://api.github.com/graphql@ (or a server's
-- @/api/graphql@): one route, a field per kind of answer.
type Graph :: Type -> Type
data Graph mode = Graph
  { searchPulls :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer (Page PullNode))
  , searchIssues :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer (Page IssueNode))
  , repositories :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer Repos)
  , discussion :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer DiscussionNode)
  , timeline :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer (Item [TimelineNode]))
  , mutate :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer Ignored)
  , refs :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer RefPage)
  , checks :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer Checks)
  , found :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer (Page FoundNode))
  , pullState :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer PullState)
  , counts :: mode :- ReqBody '[JSON] GraphQuery :> Post '[JSON] (Answer Counted)
  }
  deriving stock (Generic)

graph :: Graph (AsClientT ClientM)
graph = client (Proxy @(NamedRoutes Graph))

-- | @all@ includes the ones already read.
type NotificationQuery :: Type
data NotificationQuery = NotificationQuery
  { all :: Bool
  , since :: Maybe UTCTime
  , page :: Maybe Int
  , perPage :: Named "per_page" (Maybe Int)
  }
  deriving stock (Generic)

instance ToQuery NotificationQuery

type User :: Type
newtype User = User {login :: Login}

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "login"

-- | @reason@ is GitHub's: "review_requested", "mention", "ci_activity" …
type Thread :: Type
data Thread = Thread
  { threadId :: ThreadId
  , repository :: RepositoryRef
  , subject :: Subject
  , reason :: Text
  , unread :: Bool
  , updatedAt :: UTCTime
  }

instance FromJSON Thread where
  parseJSON = withObject "Thread" $ \o -> Thread
    <$> o .: "id"
    <*> o .: "repository"
    <*> o .: "subject"
    <*> o .: "reason"
    <*> o .: "unread"
    <*> o .: "updated_at"

-- | @kind@ is "PullRequest", "Issue", "Release", "Commit" …; @url@, the
-- subject's API URL, ends in the number of an issue or pull request.
type Subject :: Type
data Subject = Subject
  { title :: Text
  , kind :: Text
  , url :: Text
  }

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \o -> Subject
    <$> o .: "title"
    <*> o .: "type"
    <*> o .:? "url" .!= ""

type RepositoryRef :: Type
newtype RepositoryRef = RepositoryRef {fullName :: RepoName}

instance FromJSON RepositoryRef where
  parseJSON = withObject "RepositoryRef" $ \o -> RepositoryRef <$> o .: "full_name"

-- | A query and its variables, as GraphQL takes them.
type GraphQuery :: Type
data GraphQuery = GraphQuery
  { query :: Text
  , variables :: Value
  }

instance ToJSON GraphQuery where
  toJSON q = object ["query" .= q.query, "variables" .= q.variables]

-- | The @data@ of an answer, or what GitHub said went wrong: GraphQL
-- answers 200 with @errors@ when a query fails.
type Answer :: Type -> Type
data Answer a = Answer a | Problem Text

instance FromJSON a => FromJSON (Answer a) where
  parseJSON = withObject "Answer" $ \o -> do
    errors <- o .:? "errors" .!= []
    case errors of
      GraphError message : _ -> pure (Problem message)
      [] -> Answer <$> o .: "data"

type GraphError :: Type
newtype GraphError = GraphError Text

instance FromJSON GraphError where
  parseJSON = withObject "Error" $ \e -> GraphError <$> e .: "message"

-- | One page of a search, under the alias @found@.
type Page :: Type -> Type
data Page a = Page
  { nodes :: [a]
  , total :: Int
  , next :: Maybe Text
    -- ^ The cursor of the next page, when there is one.
  }

instance FromJSON a => FromJSON (Page a) where
  parseJSON = withObject "Data" $ \d -> d .: "found" >>= connection

connection :: FromJSON a => Value -> Parser (Page a)
connection = withObject "Connection" $ \c -> do
  info <- c .: "pageInfo"
  more <- info .: "hasNextPage"
  cursor <- info .:? "endCursor"
  -- A search lists other kinds as empty objects; they are left out.
  nodes <- c .: "nodes" >>= traverse (\v -> (Just <$> parseJSON v) <|> pure Nothing)
  total <- c .:? "issueCount" >>= maybe (c .:? "totalCount" .!= 0) pure
  pure Page {nodes = [n | Just n <- nodes], total = total, next = if more then cursor else Nothing}

-- | The viewer's repositories, a page of them.
type Repos :: Type
newtype Repos = Repos (Page RepoNode)

instance FromJSON Repos where
  parseJSON = withObject "Data" $ \d -> do
    viewer <- d .: "viewer"
    Repos <$> (viewer .: "found" >>= connection)

-- | A pull request as the search brings it.
type PullNode :: Type
data PullNode = PullNode
  { number :: Number
  , title :: Text
  , author :: Maybe Login
    -- ^ 'Nothing' for a deleted user ("ghost").
  , repo :: RepoName
  , headBranch :: Text
  , baseBranch :: Text
  , state :: Text
    -- ^ "OPEN", "CLOSED" or "MERGED".
  , draft :: Bool
  , reviewDecision :: Maybe Text
    -- ^ "APPROVED", "CHANGES_REQUESTED", "REVIEW_REQUIRED", or none.
  , reviewRequests :: Int
  , comments :: Count
  , changedFiles :: Count
  , commits :: Count
  , checks :: Maybe Text
    -- ^ The latest commit's rollup: "SUCCESS", "FAILURE", "ERROR",
    -- "PENDING" or "EXPECTED"; 'Nothing' when nothing runs.
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }

instance FromJSON PullNode where
  parseJSON = withObject "PullRequest" $ \o -> do
    commits <- o .: "commits"
    lastCommit <- commits .: "nodes"
    rollup <- case lastCommit of
      c : _ -> c .: "commit" >>= (.:? "statusCheckRollup") >>= traverse (.: "state")
      [] -> pure Nothing
    PullNode
      <$> o .: "number"
      <*> o .: "title"
      <*> authorOf o
      <*> (o .: "repository" >>= (.: "nameWithOwner"))
      <*> o .: "headRefName"
      <*> o .: "baseRefName"
      <*> o .: "state"
      <*> o .: "isDraft"
      <*> o .:? "reviewDecision"
      <*> (o .: "reviewRequests" >>= (.: "totalCount"))
      <*> (o .: "comments" >>= (.: "totalCount"))
      <*> o .: "changedFiles"
      <*> commits .: "totalCount"
      <*> pure rollup
      <*> o .: "createdAt"
      <*> o .: "updatedAt"

-- | An issue as the search brings it.
type IssueNode :: Type
data IssueNode = IssueNode
  { number :: Number
  , title :: Text
  , author :: Maybe Login
  , repo :: RepoName
  , labels :: [LabelNode]
  , comments :: Count
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , closedAt :: Maybe UTCTime
  }

instance FromJSON IssueNode where
  parseJSON = withObject "Issue" $ \o -> IssueNode
    <$> o .: "number"
    <*> o .: "title"
    <*> authorOf o
    <*> (o .: "repository" >>= (.: "nameWithOwner"))
    <*> (o .: "labels" >>= (.: "nodes"))
    <*> (o .: "comments" >>= (.: "totalCount"))
    <*> o .: "createdAt"
    <*> o .: "updatedAt"
    <*> o .:? "closedAt"

type LabelNode :: Type
data LabelNode = LabelNode {name :: Text, color :: Color}

instance FromJSON LabelNode where
  parseJSON = withObject "Label" $ \o -> LabelNode <$> o .: "name" <*> o .: "color"

type RepoNode :: Type
data RepoNode = RepoNode
  { name :: RepoName
  , description :: Maybe Text
  , defaultBranch :: Maybe Text
    -- ^ 'Nothing' for an empty repository.
  , private :: Bool
  , archived :: Bool
  , openPullRequests :: Count
  , openIssues :: Count
  , updatedAt :: UTCTime
  }

instance FromJSON RepoNode where
  parseJSON = withObject "Repository" $ \o -> RepoNode
    <$> o .: "nameWithOwner"
    <*> o .:? "description"
    <*> (o .:? "defaultBranchRef" >>= traverse (.: "name"))
    <*> o .: "isPrivate"
    <*> o .: "isArchived"
    <*> (o .: "pullRequests" >>= (.: "totalCount"))
    <*> (o .: "issues" >>= (.: "totalCount"))
    <*> o .: "updatedAt"

authorOf :: Object -> Parser (Maybe Login)
authorOf o = o .:? "author" >>= maybe (pure Nothing) (.:? "login")

-- | A search for @$q@, a page of 40 after @$after@ (100, with the checks,
-- is more than GitHub answers in time), with @fields@ for
-- each node.
searchQuery :: Text -> Text
searchQuery fields =
  "query($q: String!, $after: String) { found: search(query: $q, type: ISSUE, first: 40, after: $after) {"
    <> " issueCount pageInfo { hasNextPage endCursor } nodes { " <> fields <> " } } }"

pullFields :: Text
pullFields =
  "... on PullRequest { number title author { login } repository { nameWithOwner } headRefName baseRefName"
    <> " state isDraft reviewDecision reviewRequests { totalCount } comments { totalCount } changedFiles"
    <> " commits(last: 1) { totalCount nodes { commit { statusCheckRollup { state } } } } createdAt updatedAt }"

issueFields :: Text
issueFields =
  "... on Issue { number title author { login } repository { nameWithOwner }"
    <> " labels(first: 20) { nodes { name color } } comments { totalCount } createdAt updatedAt closedAt }"

-- | The repositories the viewer owns, works on or is in an organization
-- of, recently changed first. Both filters are needed: ownerAffiliations
-- leaves out an organization's repositories unless it is given too.
reposQuery :: Text
reposQuery =
  "query($after: String) { viewer { found: repositories(first: 100, after: $after,"
    <> " affiliations: [OWNER, COLLABORATOR, ORGANIZATION_MEMBER], ownerAffiliations: [OWNER, COLLABORATOR, ORGANIZATION_MEMBER],"
    <> " orderBy: {field: UPDATED_AT, direction: DESC}) {"
    <> " totalCount pageInfo { hasNextPage endCursor } nodes { nameWithOwner description defaultBranchRef { name }"
    <> " isPrivate isArchived pullRequests(states: OPEN) { totalCount } issues(states: OPEN) { totalCount } updatedAt } } } }"

-- | A page of a REST list; GitHub gives at most 100 a page.
type ListQuery :: Type
data ListQuery = ListQuery
  { page :: Maybe Int
  , perPage :: Named "per_page" (Maybe Int)
  }
  deriving stock (Generic)

instance ToQuery ListQuery

type RepoInfo :: Type
data RepoInfo = RepoInfo {cloneUrl :: Text, sshUrl :: Maybe Text}

instance FromJSON RepoInfo where
  parseJSON = withObject "Repository" $ \o -> RepoInfo <$> o .: "clone_url" <*> o .:? "ssh_url"

type Body :: Type
newtype Body = Body Text

instance ToJSON Body where
  toJSON (Body b) = object ["body" .= b]

type RestComment :: Type
data RestComment = RestComment
  { id :: Int64
  , author :: Maybe Login
  , body :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , url :: Text
  }

instance FromJSON RestComment where
  parseJSON = withObject "Comment" $ \o -> RestComment
    <$> o .: "id"
    <*> (o .:? "user" >>= maybe (pure Nothing) (.:? "login"))
    <*> o .:? "body" .!= ""
    <*> o .: "created_at"
    <*> o .: "updated_at"
    <*> o .: "html_url"

type RestReaction :: Type
data RestReaction = RestReaction {id :: Int64, user :: Maybe Login}

instance FromJSON RestReaction where
  parseJSON = withObject "Reaction" $ \o -> RestReaction
    <$> o .: "id"
    <*> (o .:? "user" >>= maybe (pure Nothing) (.:? "login"))

type ReactionBody :: Type
newtype ReactionBody = ReactionBody Feeling

instance ToJSON ReactionBody where
  toJSON (ReactionBody f) = object ["content" .= feelingName f]

-- | GitHub's REST name of a reaction.
feelingName :: Feeling -> Text
feelingName f = case f of
  ThumbsUp -> "+1"
  ThumbsDown -> "-1"
  Laugh -> "laugh"
  Hooray -> "hooray"
  Confused -> "confused"
  Heart -> "heart"
  Rocket -> "rocket"
  Eyes -> "eyes"

-- | A reaction by its GraphQL name, "THUMBS_UP" and the like.
feelingOf :: Text -> Maybe Feeling
feelingOf t = lookup t
  [ ("THUMBS_UP", ThumbsUp), ("THUMBS_DOWN", ThumbsDown), ("LAUGH", Laugh), ("HOORAY", Hooray)
  , ("CONFUSED", Confused), ("HEART", Heart), ("ROCKET", Rocket), ("EYES", Eyes) ]

type RestLabel :: Type
data RestLabel = RestLabel {name :: Text, color :: Color, description :: Text}

instance FromJSON RestLabel where
  parseJSON = withObject "Label" $ \o -> RestLabel <$> o .: "name" <*> o .: "color" <*> o .:? "description" .!= ""

instance ToJSON RestLabel where
  toJSON l = object ["name" .= l.name, "color" .= colorText l.color, "description" .= l.description]
    where
      colorText = unparse

type LabelNames :: Type
newtype LabelNames = LabelNames [Text]

instance ToJSON LabelNames where
  toJSON (LabelNames ns) = object ["labels" .= ns]

type RestMilestone :: Type
data RestMilestone = RestMilestone
  { number :: Int
  , title :: Text
  , open :: Bool
  , due :: Maybe UTCTime
  , closedAt :: Maybe UTCTime
  , openItems :: Int
  , closedItems :: Int
  }

instance FromJSON RestMilestone where
  parseJSON = withObject "Milestone" $ \o -> RestMilestone
    <$> o .: "number"
    <*> o .: "title"
    <*> ((== ("open" :: Text)) <$> o .: "state")
    <*> o .:? "due_on"
    <*> o .:? "closed_at"
    <*> o .: "open_issues"
    <*> o .: "closed_issues"

-- | What was created, by its number.
type Created :: Type
newtype Created = Created {number :: Number}

instance FromJSON Created where
  parseJSON = withObject "Created" $ \o -> Created <$> o .: "number"

-- | An answer whose content usta does not need.
type Ignored :: Type
data Ignored = Ignored

instance FromJSON Ignored where
  parseJSON _ = pure Ignored

-- | An issue or a pull request with its conversation, a page of comments
-- at a time.
type DiscussionNode :: Type
data DiscussionNode = DiscussionNode
  { nodeId :: Text
  , opening :: CommentNode
  , closed :: Bool
  , merged :: Bool
  , subscription :: Maybe Text
    -- ^ "SUBSCRIBED", "UNSUBSCRIBED" or "IGNORED".
  , assignees :: [Login]
  , labels :: [LabelNode]
  , milestone :: Maybe Text
  , comments :: Page CommentNode
  , reviews :: [ReviewText]
    -- ^ A pull request's reviews; an issue has none.
  }

-- | A review as a comment, with its state ("APPROVED" …).
type ReviewText :: Type
data ReviewText = ReviewText {comment :: CommentNode, state :: Text}

instance FromJSON ReviewText where
  parseJSON v = withObject "Review" (\o -> ReviewText <$> parseJSON v <*> o .: "state") v

instance FromJSON DiscussionNode where
  parseJSON = withObject "Data" $ \d -> do
    item <- d .: "repository" >>= (.: "found")
    withObject "Item" (\o -> DiscussionNode
      <$> o .: "id"
      <*> parseJSON item
      <*> o .: "closed"
      <*> o .:? "merged" .!= False
      <*> o .:? "viewerSubscription"
      <*> (o .: "assignees" >>= (.: "nodes") >>= traverse (.: "login"))
      <*> (o .: "labels" >>= (.: "nodes"))
      <*> (o .:? "milestone" >>= traverse (.: "title"))
      <*> (o .: "comments" >>= connection)
      <*> (o .:? "reviews" >>= maybe (pure []) (.: "nodes"))) item

-- | A comment, or the description (which has no database id here).
type CommentNode :: Type
data CommentNode = CommentNode
  { databaseId :: Maybe Int64
  , author :: Maybe Login
  , body :: Text
  , createdAt :: UTCTime
  , edited :: Bool
  , url :: Text
  , reactions :: [ReactionGroup]
  }

instance FromJSON CommentNode where
  parseJSON = withObject "Comment" $ \o -> CommentNode
    <$> o .:? "databaseId"
    <*> authorOf o
    <*> o .: "body"
    <*> (o .:? "createdAt" >>= maybe (o .: "submittedAt") pure)
    <*> ((/= (Nothing :: Maybe Value)) <$> o .:? "lastEditedAt")
    <*> o .: "url"
    <*> o .: "reactionGroups"

type ReactionGroup :: Type
data ReactionGroup = ReactionGroup {content :: Text, mine :: Bool, count :: Int}

instance FromJSON ReactionGroup where
  parseJSON = withObject "ReactionGroup" $ \o -> ReactionGroup
    <$> o .: "content"
    <*> o .: "viewerHasReacted"
    <*> (o .: "reactors" >>= (.: "totalCount"))

-- | What the issue or pull request found has, under @repository.found@.
type Item :: Type -> Type
newtype Item a = Item a

instance FromJSON (Item [TimelineNode]) where
  parseJSON = withObject "Data" $ \d -> do
    found <- d .: "repository" >>= (.: "found")
    Item <$> (found .: "timelineItems" >>= (.: "nodes"))

-- | One event of the timeline, as GitHub names it; usta reads the fields
-- each kind has.
type TimelineNode :: Type
data TimelineNode = TimelineNode
  { kind :: Text
  , actor :: Maybe Login
  , at :: Maybe UTCTime
  , label :: Maybe LabelNode
  , assignee :: Maybe Login
  , reviewer :: Maybe Login
  , milestone :: Maybe Text
  , previous :: Maybe Text
  , current :: Maybe Text
  , source :: Maybe (Number, Text)
  , commitAuthor :: Maybe Login
  , committedAt :: Maybe UTCTime
  , reviewState :: Maybe Text
  , reviewBody :: Maybe Text
  }

instance FromJSON TimelineNode where
  parseJSON = withObject "TimelineItem" $ \o -> do
    source <- o .:? "source" >>= \case
      Just s -> (\n t -> (,) <$> n <*> t) <$> s .:? "number" <*> s .:? "title"
      Nothing -> pure Nothing
    commit <- o .:? "commit"
    TimelineNode
      <$> o .: "__typename"
      <*> ((<|>) <$> authorOfKey "actor" o <*> authorOfKey "author" o)
      <*> ((<|>) <$> o .:? "createdAt" <*> o .:? "submittedAt")
      <*> o .:? "label"
      <*> (o .:? "assignee" >>= maybe (pure Nothing) (.:? "login"))
      <*> (o .:? "requestedReviewer" >>= maybe (pure Nothing) (.:? "login"))
      <*> o .:? "milestoneTitle"
      <*> o .:? "previousTitle"
      <*> o .:? "currentTitle"
      <*> pure source
      <*> maybe (pure Nothing) (\c -> c .:? "author" >>= maybe (pure Nothing) (\a -> a .:? "user" >>= maybe (pure Nothing) (.:? "login"))) commit
      <*> maybe (pure Nothing) (.:? "committedDate") commit
      <*> o .:? "state"
      <*> o .:? "body"

authorOfKey :: Text -> Object -> Parser (Maybe Login)
authorOfKey key o = o .:? Data.Aeson.Key.fromText key >>= maybe (pure Nothing) (.:? "login")

reactionFields :: Text
reactionFields = "reactionGroups { content viewerHasReacted reactors { totalCount } }"

-- | An issue's or pull request's conversation, by @$owner@, @$name@ and
-- @$number@, its comments a page after @$after@.
discussionQuery :: Text
discussionQuery =
  "query($owner: String!, $name: String!, $number: Int!, $after: String) { repository(owner: $owner, name: $name) {"
    <> " found: issueOrPullRequest(number: $number) { ... on Issue { " <> common <> " } ... on PullRequest { " <> common <> " merged"
    <> " reviews(first: 100) { nodes { author { login } body state createdAt lastEditedAt url " <> reactionFields <> " } } } } } }"
  where
    common =
      "id author { login } body createdAt lastEditedAt url " <> reactionFields
        <> " closed viewerSubscription assignees(first: 20) { nodes { login } } labels(first: 30) { nodes { name color } }"
        <> " milestone { title } comments(first: 100, after: $after) { totalCount pageInfo { hasNextPage endCursor }"
        <> " nodes { databaseId author { login } body createdAt lastEditedAt url " <> reactionFields <> " } }"

-- | The events usta shows, oldest first, at most 250.
timelineQuery :: Text
timelineQuery =
  "query($owner: String!, $name: String!, $number: Int!) { repository(owner: $owner, name: $name) {"
    <> " found: issueOrPullRequest(number: $number) { ... on Issue { " <> items "" <> " } ... on PullRequest { " <> items pullKinds <> " } } } }"
  where
    items extra =
      "timelineItems(first: 250, itemTypes: [LABELED_EVENT, UNLABELED_EVENT, ASSIGNED_EVENT, UNASSIGNED_EVENT,"
        <> " MILESTONED_EVENT, DEMILESTONED_EVENT, RENAMED_TITLE_EVENT, CLOSED_EVENT, REOPENED_EVENT, CROSS_REFERENCED_EVENT"
        <> extra <> "]) { nodes { __typename " <> fields <> (if T.null extra then "" else pullFields') <> " } }"
    pullKinds = ", PULL_REQUEST_REVIEW, MERGED_EVENT, REVIEW_REQUESTED_EVENT, REVIEW_REQUEST_REMOVED_EVENT, PULL_REQUEST_COMMIT, AUTO_MERGE_ENABLED_EVENT, AUTO_MERGE_DISABLED_EVENT, HEAD_REF_DELETED_EVENT"
    -- Only a pull request's timeline has these.
    pullFields' =
      " ... on MergedEvent { actor { login } createdAt }"
        <> " ... on PullRequestReview { author { login } submittedAt state body }"
        <> " ... on ReviewRequestedEvent { actor { login } createdAt requestedReviewer { ... on User { login } } }"
        <> " ... on ReviewRequestRemovedEvent { actor { login } createdAt requestedReviewer { ... on User { login } } }"
        <> " ... on PullRequestCommit { commit { committedDate author { user { login } } } }"
        <> " ... on AutoMergeEnabledEvent { actor { login } createdAt }"
        <> " ... on AutoMergeDisabledEvent { actor { login } createdAt }"
        <> " ... on HeadRefDeletedEvent { actor { login } createdAt }"
    fields =
      "... on LabeledEvent { actor { login } createdAt label { name color } }"
        <> " ... on UnlabeledEvent { actor { login } createdAt label { name color } }"
        <> " ... on AssignedEvent { actor { login } createdAt assignee { ... on User { login } } }"
        <> " ... on UnassignedEvent { actor { login } createdAt assignee { ... on User { login } } }"
        <> " ... on MilestonedEvent { actor { login } createdAt milestoneTitle }"
        <> " ... on DemilestonedEvent { actor { login } createdAt milestoneTitle }"
        <> " ... on RenamedTitleEvent { actor { login } createdAt previousTitle currentTitle }"
        <> " ... on ClosedEvent { actor { login } createdAt }"
        <> " ... on ReopenedEvent { actor { login } createdAt }"
        <> " ... on CrossReferencedEvent { actor { login } createdAt source { ... on Issue { number title } ... on PullRequest { number title } } }"

-- | Sets the viewer's subscription to @$id@: "SUBSCRIBED",
-- "UNSUBSCRIBED" or "IGNORED".
subscriptionMutation :: Text
subscriptionMutation =
  "mutation($id: ID!, $state: SubscriptionState!) { updateSubscription(input: {subscribableId: $id, state: $state}) { clientMutationId } }"

-- | A diff, as GitHub sends it for this media type.
type GitHubDiff :: Type
data GitHubDiff

instance Accept GitHubDiff where
  contentTypes _ = "application/vnd.github.diff" :| []

instance MimeUnrender GitHubDiff Text where
  mimeUnrender _ = Right . decodeUtf8Lenient . BL.toStrict

-- | A file's bytes as they are, or a job's log.
type GitHubRaw :: Type
data GitHubRaw

instance Accept GitHubRaw where
  contentTypes _ = "application/vnd.github.raw" :| []

instance MimeUnrender GitHubRaw ByteString where
  mimeUnrender _ = Right . BL.toStrict

-- | A job's log. GitHub's API takes only its own media type, then
-- redirects to storage that sends the log as text.
type LogText :: Type
data LogText

instance Accept LogText where
  contentTypes _ = "application/vnd.github+json" :| ["text/plain", "application/octet-stream"]

instance MimeUnrender LogText ByteString where
  mimeUnrender _ = Right . BL.toStrict

type CommitsQuery :: Type
data CommitsQuery = CommitsQuery
  { sha :: Maybe Text
  , path :: Maybe Text
  , page :: Maybe Int
  , perPage :: Named "per_page" (Maybe Int)
  }
  deriving stock (Generic)

instance ToQuery CommitsQuery

type RestCommit :: Type
data RestCommit = RestCommit
  { sha :: Text
  , author :: Text
  , authoredAt :: UTCTime
  , message :: Text
  }

instance FromJSON RestCommit where
  parseJSON = withObject "Commit" $ \o -> do
    c <- o .: "commit"
    a <- c .: "author"
    RestCommit <$> o .: "sha" <*> a .: "name" <*> a .: "date" <*> c .: "message"

-- | The path segment "base...head".
type BaseHead :: Type
data BaseHead = BaseHead Text Text

instance ToHttpApiData BaseHead where
  toUrlPiece (BaseHead base tip) = base <> "..." <> tip

type Comparison :: Type
data Comparison = Comparison {commits :: [RestCommit], files :: [Text], behindBy :: Int}

instance FromJSON Comparison where
  parseJSON = withObject "Comparison" $ \o -> Comparison
    <$> o .: "commits"
    <*> (o .:? "files" .!= [] >>= traverse (withObject "File" (.: "filename")))
    <*> o .:? "behind_by" .!= 0

-- | Something with a name and a path, as the README answer.
type Named' :: Type
data Named' = Named' {name :: Text, path :: Text}

instance FromJSON Named' where
  parseJSON = withObject "Content" $ \o -> Named' <$> o .: "name" <*> o .: "path"

type Tree :: Type
data Tree = Tree {entries :: [TreeEntry], truncated :: Bool}

instance FromJSON Tree where
  parseJSON = withObject "Tree" $ \o -> Tree <$> o .: "tree" <*> o .:? "truncated" .!= False

type TreeEntry :: Type
data TreeEntry = TreeEntry {path :: Text, folder :: Bool, size :: Maybe Int}

instance FromJSON TreeEntry where
  parseJSON = withObject "Entry" $ \o -> TreeEntry
    <$> o .: "path"
    <*> ((== ("tree" :: Text)) <$> o .: "type")
    <*> o .:? "size"

type RestRelease :: Type
data RestRelease = RestRelease
  { name :: Maybe Text
  , tag :: Text
  , notes :: Maybe Text
  , author :: Maybe Login
  , published :: Maybe UTCTime
  , draft :: Bool
  , prerelease :: Bool
  , url :: Text
  , assets :: [RestAsset]
  }

instance FromJSON RestRelease where
  parseJSON = withObject "Release" $ \o -> RestRelease
    <$> o .:? "name"
    <*> o .: "tag_name"
    <*> o .:? "body"
    <*> (o .:? "author" >>= maybe (pure Nothing) (.:? "login"))
    <*> o .:? "published_at"
    <*> o .: "draft"
    <*> o .: "prerelease"
    <*> o .: "html_url"
    <*> o .: "assets"

type RestAsset :: Type
data RestAsset = RestAsset {name :: Text, size :: Int64, downloads :: Int, url :: Text}

instance FromJSON RestAsset where
  parseJSON = withObject "Asset" $ \o -> RestAsset
    <$> o .: "name" <*> o .: "size" <*> o .: "download_count" <*> o .: "browser_download_url"

type WorkflowRuns :: Type
newtype WorkflowRuns = WorkflowRuns [WorkflowRun]

instance FromJSON WorkflowRuns where
  parseJSON = withObject "Runs" $ \o -> WorkflowRuns <$> o .: "workflow_runs"

type WorkflowRun :: Type
data WorkflowRun = WorkflowRun
  { id :: Int64
  , number :: Int
  , workflow :: Text
  , title :: Text
  , branch :: Text
  , event :: Text
  , sha :: Text
  , status :: Text
  , conclusion :: Maybe Text
  , started :: UTCTime
  , updated :: UTCTime
  , url :: Text
  }

instance FromJSON WorkflowRun where
  parseJSON = withObject "Run" $ \o -> WorkflowRun
    <$> o .: "id"
    <*> o .: "run_number"
    <*> o .:? "name" .!= ""
    <*> o .:? "display_title" .!= ""
    <*> o .:? "head_branch" .!= ""
    <*> o .: "event"
    <*> o .: "head_sha"
    <*> o .: "status"
    <*> o .:? "conclusion"
    <*> (o .:? "run_started_at" >>= maybe (o .: "created_at") pure)
    <*> o .: "updated_at"
    <*> o .: "html_url"

type Jobs :: Type
newtype Jobs = Jobs [RestJob]

instance FromJSON Jobs where
  parseJSON = withObject "Jobs" $ \o -> Jobs <$> o .: "jobs"

type RestJob :: Type
data RestJob = RestJob
  { id :: Int64
  , name :: Text
  , status :: Text
  , conclusion :: Maybe Text
  , startedAt :: Maybe UTCTime
  , completedAt :: Maybe UTCTime
  , url :: Text
  }

instance FromJSON RestJob where
  parseJSON = withObject "Job" $ \o -> RestJob
    <$> o .: "id" <*> o .: "name" <*> o .: "status" <*> o .:? "conclusion"
    <*> o .:? "started_at" <*> o .:? "completed_at" <*> o .: "html_url"

-- | A page of a repository's branches or tags.
type RefPage :: Type
newtype RefPage = RefPage (Page RefNode)

instance FromJSON RefPage where
  parseJSON = withObject "Data" $ \d -> d .: "repository" >>= (.: "found") >>= fmap RefPage . connection

type RefNode :: Type
data RefNode = RefNode {name :: Text, at :: Maybe UTCTime}

instance FromJSON RefNode where
  parseJSON = withObject "Ref" $ \o -> do
    target <- o .:? "target"
    -- A tag may point at an annotated tag, which points at the commit.
    at <- case target of
      Just t -> do
        direct <- t .:? "committedDate"
        inner <- t .:? "target" >>= maybe (pure Nothing) (.:? "committedDate")
        pure (direct <|> inner)
      Nothing -> pure Nothing
    RefNode <$> o .: "name" <*> pure at

-- | One check on a pull request's latest commit: a check run (most are
-- GitHub Actions jobs) or a commit status.
type CheckNode :: Type
data CheckNode = CheckNode
  { kind :: Text
  , name :: Text
  , status :: Maybe Text
  , conclusion :: Maybe Text
  , state :: Maybe Text
  , description :: Maybe Text
  , url :: Maybe Text
  , jobId :: Maybe Int64
  }

-- | The checks of the pull request found.
type Checks :: Type
newtype Checks = Checks [CheckNode]

instance FromJSON Checks where
  parseJSON = withObject "Data" $ \d -> fmap Checks $ do
    found <- d .: "repository" >>= (.: "found")
    commits <- found .: "commits" >>= (.: "nodes")
    case commits of
      c : _ -> c .: "commit" >>= (.:? "statusCheckRollup") >>= \case
        Just r -> r .: "contexts" >>= (.: "nodes")
        Nothing -> pure []
      [] -> pure []

instance FromJSON CheckNode where
  parseJSON = withObject "Check" $ \o -> CheckNode
    <$> o .: "__typename"
    <*> ((<|>) <$> o .:? "name" <*> o .:? "context" >>= maybe (pure "") pure)
    <*> o .:? "status"
    <*> o .:? "conclusion"
    <*> o .:? "state"
    <*> ((<|>) <$> o .:? "title" <*> o .:? "description")
    <*> ((<|>) <$> o .:? "detailsUrl" <*> o .:? "targetUrl")
    <*> o .:? "databaseId"

-- | A pull request, issue or repository the forge's search found.
type FoundNode :: Type
data FoundNode = FoundNode
  { kind :: Text
  , repo :: Text
  , number :: Maybe Int
  , title :: Text
  , closed :: Bool
  , updated :: UTCTime
  }

instance FromJSON FoundNode where
  parseJSON = withObject "Found" $ \o -> do
    kind <- o .: "__typename"
    case kind :: Text of
      "Repository" -> FoundNode kind <$> o .: "nameWithOwner" <*> pure Nothing <*> (o .:? "description" .!= "") <*> pure False <*> o .: "updatedAt"
      _ -> FoundNode kind
        <$> (o .: "repository" >>= (.: "nameWithOwner"))
        <*> (Just <$> o .: "number")
        <*> o .: "title"
        <*> o .: "closed"
        <*> o .: "updatedAt"

-- | Branches (@$prefix@ "refs/heads/") or tags ("refs/tags/"), latest
-- commit first, a page of 100.
refsQuery :: Text
refsQuery =
  "query($owner: String!, $name: String!, $prefix: String!, $after: String) { repository(owner: $owner, name: $name) {"
    <> " found: refs(refPrefix: $prefix, first: 100, after: $after, orderBy: {field: TAG_COMMIT_DATE, direction: DESC}) {"
    <> " totalCount pageInfo { hasNextPage endCursor } nodes { name target { ... on Commit { committedDate }"
    <> " ... on Tag { target { ... on Commit { committedDate } } } } } } } }"

-- | The checks on a pull request's latest commit.
checksQuery :: Text
checksQuery =
  "query($owner: String!, $name: String!, $number: Int!) { repository(owner: $owner, name: $name) {"
    <> " found: pullRequest(number: $number) { commits(last: 1) { nodes { commit { statusCheckRollup { contexts(first: 100) {"
    <> " nodes { __typename ... on CheckRun { name status conclusion title detailsUrl databaseId }"
    <> " ... on StatusContext { context state description targetUrl } } } } } } } } } }"

-- | The forge's search for @$q@ (an ISSUE or REPOSITORY search, @$type@),
-- ten of them.
foundQuery :: Text
foundQuery =
  "query($q: String!, $type: SearchType!) { found: search(query: $q, type: $type, first: 10) {"
    <> " issueCount pageInfo { hasNextPage endCursor } nodes { __typename"
    <> " ... on PullRequest { number title closed updatedAt repository { nameWithOwner } }"
    <> " ... on Issue { number title closed updatedAt repository { nameWithOwner } }"
    <> " ... on Repository { nameWithOwner description updatedAt } } } }"

type Team :: Type
data Team = Team {slug :: Text, name :: Text}

instance FromJSON Team where
  parseJSON = withObject "Team" $ \o -> Team <$> o .: "slug" <*> o .: "name"

-- | Where a pull request stands for review and merging, and who looks.
type PullState :: Type
data PullState = PullState
  { viewer :: Login
  , nodeId :: Text
  , draft :: Bool
  , mergeable :: Text
    -- ^ "MERGEABLE", "CONFLICTING" or "UNKNOWN".
  , autoMerge :: Bool
  , headBranch :: Text
  , baseBranch :: Text
  , headRepo :: Maybe RepoName
  , headSha :: Text
  , requiredApprovals :: Int
  , requested :: [Either Text Login]
    -- ^ A team by its slug, or a person.
  , latest :: [ReviewNode]
    -- ^ Each reviewer's latest review.
  , reviews :: [ReviewNode]
    -- ^ The last 100, pending ones too.
  , threads :: [ThreadNode]
  }

instance FromJSON PullState where
  parseJSON = withObject "Data" $ \d -> do
    viewer <- d .: "viewer" >>= (.: "login")
    p <- d .: "repository" >>= (.: "found")
    rule <- p .:? "baseRef" >>= maybe (pure Nothing) (.:? "branchProtectionRule")
    requested <- p .: "reviewRequests" >>= (.: "nodes") >>= traverse (\r -> r .: "requestedReviewer" >>= \who -> do
      slug <- who .:? "slug"
      login <- who .:? "login"
      pure (maybe (Left <$> slug) (Just . Right) login))
    PullState viewer
      <$> p .: "id"
      <*> p .: "isDraft"
      <*> p .: "mergeable"
      <*> ((/= (Nothing :: Maybe Value)) <$> p .:? "autoMergeRequest")
      <*> p .: "headRefName"
      <*> p .: "baseRefName"
      <*> (p .:? "headRepository" >>= maybe (pure Nothing) (.:? "nameWithOwner"))
      <*> p .: "headRefOid"
      <*> maybe (pure 0) (\r -> r .:? "requiredApprovingReviewCount" .!= 0) rule
      <*> pure [w | Just w <- requested]
      <*> (p .: "latestReviews" >>= (.: "nodes"))
      <*> (p .: "reviews" >>= (.: "nodes"))
      <*> (p .: "reviewThreads" >>= (.: "nodes"))

type ReviewNode :: Type
data ReviewNode = ReviewNode
  { id :: Maybe Int64
  , author :: Maybe Login
  , state :: Text
    -- ^ "APPROVED", "CHANGES_REQUESTED", "COMMENTED", "DISMISSED" or "PENDING".
  , submittedAt :: Maybe UTCTime
  , commit :: Maybe Text
  }

instance FromJSON ReviewNode where
  parseJSON = withObject "Review" $ \o -> ReviewNode
    <$> o .:? "databaseId"
    <*> authorOf o
    <*> o .: "state"
    <*> o .:? "submittedAt"
    <*> (o .:? "commit" >>= maybe (pure Nothing) (.:? "oid"))

type ThreadNode :: Type
data ThreadNode = ThreadNode
  { path :: Text
  , line :: Maybe Int
  , originalLine :: Maybe Int
  , resolvedBy :: Maybe Login
  , notes :: [NoteNode]
  }

instance FromJSON ThreadNode where
  parseJSON = withObject "Thread" $ \o -> ThreadNode
    <$> o .: "path"
    <*> o .:? "line"
    <*> o .:? "originalLine"
    <*> (o .:? "resolvedBy" >>= maybe (pure Nothing) (.:? "login"))
    <*> (o .: "comments" >>= (.: "nodes"))

type NoteNode :: Type
data NoteNode = NoteNode
  { id :: Int64
  , author :: Maybe Login
  , body :: Text
  , createdAt :: UTCTime
  , hunk :: Text
  , url :: Text
  , commit :: Maybe Text
  }

instance FromJSON NoteNode where
  parseJSON = withObject "Comment" $ \o -> NoteNode
    <$> o .: "databaseId"
    <*> authorOf o
    <*> o .: "body"
    <*> o .: "createdAt"
    <*> o .:? "diffHunk" .!= ""
    <*> o .: "url"
    <*> (o .:? "commit" >>= maybe (pure Nothing) (.:? "oid"))

-- | A pull request's review and merge state, by @$owner@, @$name@ and
-- @$number@.
pullStateQuery :: Text
pullStateQuery =
  "query($owner: String!, $name: String!, $number: Int!) { viewer { login } repository(owner: $owner, name: $name) {"
    <> " found: pullRequest(number: $number) { id isDraft mergeable autoMergeRequest { enabledAt } headRefName baseRefName"
    <> " headRepository { nameWithOwner } headRefOid baseRef { branchProtectionRule { requiredApprovingReviewCount } }"
    <> " reviewRequests(first: 50) { nodes { requestedReviewer { ... on User { login } ... on Team { slug } } } }"
    <> " latestReviews(first: 50) { nodes { databaseId author { login } state submittedAt commit { oid } } }"
    <> " reviews(last: 100) { nodes { databaseId author { login } state submittedAt commit { oid } } }"
    <> " reviewThreads(first: 100) { nodes { path line originalLine resolvedBy { login }"
    <> " comments(first: 100) { nodes { databaseId author { login } body createdAt diffHunk url commit { oid } } } } } } } }"

-- | One of the mutations on a pull request by its @$id@: "markPullRequestReadyForReview",
-- "disablePullRequestAutoMerge", or with @extra@ input fields
-- "enablePullRequestAutoMerge" and "updatePullRequestBranch".
pullMutation :: Text -> Text -> Text
pullMutation name extra =
  "mutation($id: ID!) { " <> name <> "(input: {pullRequestId: $id" <> extra <> "}) { clientMutationId } }"

-- | How many each of five searches finds, by their names in 'countsQuery'.
type Counted :: Type
data Counted = Counted {opened :: Int, drafts :: Int, merged :: Int, shut :: Int, every :: Int}

instance FromJSON Counted where
  parseJSON = withObject "Data" $ \d -> Counted
    <$> count d "opened" <*> count d "drafts" <*> count d "merged" <*> count d "shut" <*> count d "every"
    where
      count d k = d .: Data.Aeson.Key.fromText k >>= (.: "issueCount")

-- | Five searches' counts at once, @$opened@ to @$every@.
countsQuery :: Text
countsQuery =
  "query($opened: String!, $drafts: String!, $merged: String!, $shut: String!, $every: String!) {"
    <> T.concat [" " <> k <> ": search(query: $" <> k <> ", type: ISSUE, first: 1) { issueCount }" | k <- ["opened", "drafts", "merged", "shut", "every"]]
    <> " }"

-- | A job with its steps, each by name, status and conclusion.
type JobSteps :: Type
data JobSteps = JobSteps {status :: Text, conclusion :: Maybe Text, steps :: [(Text, Text, Maybe Text)]}

instance FromJSON JobSteps where
  parseJSON = withObject "Job" $ \o -> JobSteps
    <$> o .: "status"
    <*> o .:? "conclusion"
    <*> (o .:? "steps" .!= [] >>= traverse (withObject "Step" (\x -> (,,) <$> x .: "name" <*> x .: "status" <*> x .:? "conclusion")))

-- | Each workflow's name and state: "active", "deleted" (its file is
-- gone), or one of the "disabled_…" states.
type Workflows :: Type
newtype Workflows = Workflows [(Text, Text, Text)]
  -- ^ Each one's name, file and state.

instance FromJSON Workflows where
  parseJSON = withObject "Workflows" $ \o -> Workflows
    <$> (o .: "workflows" >>= traverse (withObject "Workflow" (\w -> (,,) <$> w .: "name" <*> w .: "path" <*> w .: "state")))
