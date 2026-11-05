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
  , ReactionGroup (..)
  , Item (..)
  , TimelineNode (..)
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
import Data.Int (Int64)
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
           :> "comments" :> ReqBody '[JSON] Body :> Post '[JSON] RestComment
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
           :> "reactions" :> ReqBody '[JSON] ReactionBody :> Post '[JSON] Ignored
  , unreactToIssue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "number" Number
           :> "reactions" :> Capture "reaction" Int64 :> DeleteNoContent
  , commentReactions
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> "comments" :> Capture "id" Int64
           :> "reactions" :> QueryParam "content" Text :> QueryParam "per_page" Int :> Get '[JSON] [RestReaction]
  , reactToComment
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> "comments" :> Capture "id" Int64
           :> "reactions" :> ReqBody '[JSON] ReactionBody :> Post '[JSON] Ignored
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
           :> Post '[JSON] Ignored
  , assignees
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "assignees" :> RecordQueryParam ListQuery
           :> Get '[JSON] [User]
  , milestones
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "milestones" :> QueryParam "state" Text
           :> RecordQueryParam ListQuery :> Get '[JSON] [RestMilestone]
  , createIssue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> ReqBody '[JSON] Value
           :> Post '[JSON] Created
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
-- of, recently changed first.
reposQuery :: Text
reposQuery =
  "query($after: String) { viewer { found: repositories(first: 100, after: $after,"
    <> " affiliations: [OWNER, COLLABORATOR, ORGANIZATION_MEMBER], orderBy: {field: UPDATED_AT, direction: DESC}) {"
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
  }

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
      <*> (o .: "comments" >>= connection)) item

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
    <*> o .: "createdAt"
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
  }

instance FromJSON TimelineNode where
  parseJSON = withObject "TimelineItem" $ \o -> do
    source <- o .:? "source" >>= \case
      Just s -> (\n t -> (,) <$> n <*> t) <$> s .:? "number" <*> s .:? "title"
      Nothing -> pure Nothing
    commit <- o .:? "commit"
    TimelineNode
      <$> o .: "__typename"
      <*> authorOfKey "actor" o
      <*> o .:? "createdAt"
      <*> o .:? "label"
      <*> (o .:? "assignee" >>= maybe (pure Nothing) (.:? "login"))
      <*> (o .:? "requestedReviewer" >>= maybe (pure Nothing) (.:? "login"))
      <*> o .:? "milestoneTitle"
      <*> o .:? "previousTitle"
      <*> o .:? "currentTitle"
      <*> pure source
      <*> maybe (pure Nothing) (\c -> c .:? "author" >>= maybe (pure Nothing) (\a -> a .:? "user" >>= maybe (pure Nothing) (.:? "login"))) commit
      <*> maybe (pure Nothing) (.:? "committedDate") commit

authorOfKey :: Text -> Object -> Parser (Maybe Login)
authorOfKey key o = o .:? Data.Aeson.Key.fromText key >>= maybe (pure Nothing) (.:? "login")

reactionFields :: Text
reactionFields = "reactionGroups { content viewerHasReacted reactors { totalCount } }"

-- | An issue's or pull request's conversation, by @$owner@, @$name@ and
-- @$number@, its comments a page after @$after@.
discussionQuery :: Text
discussionQuery =
  "query($owner: String!, $name: String!, $number: Int!, $after: String) { repository(owner: $owner, name: $name) {"
    <> " found: issueOrPullRequest(number: $number) { ... on Issue { " <> common <> " } ... on PullRequest { " <> common <> " merged } } } }"
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
    pullKinds = ", MERGED_EVENT, REVIEW_REQUESTED_EVENT, REVIEW_REQUEST_REMOVED_EVENT, PULL_REQUEST_COMMIT, AUTO_MERGE_ENABLED_EVENT, AUTO_MERGE_DISABLED_EVENT, HEAD_REF_DELETED_EVENT"
    -- Only a pull request's timeline has these.
    pullFields' =
      " ... on MergedEvent { actor { login } createdAt }"
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
