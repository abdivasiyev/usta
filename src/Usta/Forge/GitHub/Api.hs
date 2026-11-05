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
  , searchQuery
  , pullFields
  , issueFields
  , reposQuery
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client (AsClientT, ClientM, client)

import Usta.Types.Count (Count)
import Usta.Types.ETag (ETag)
import Usta.Types.Label (Color)
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

-- | The @data@ of an answer. GraphQL answers 200 with @errors@ when a query
-- fails, so those are a decode failure with their message.
type Answer :: Type -> Type
newtype Answer a = Answer a

instance FromJSON a => FromJSON (Answer a) where
  parseJSON = withObject "Answer" $ \o -> do
    errors <- o .:? "errors" .!= []
    case errors of
      GraphError message : _ -> fail ("GitHub said: " <> T.unpack message)
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
