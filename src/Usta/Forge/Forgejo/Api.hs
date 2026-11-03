{-# LANGUAGE OverloadedStrings #-}

-- | The part of Forgejo's REST API (@/api/v1@) usta uses, as a servant API,
-- with the answers decoded into usta's value types as they arrive. Only the
-- fields usta reads are decoded; Forgejo sends many more.
module Usta.Forge.Forgejo.Api
  ( Routes (..)
  , api
  , ListQuery (..)
  , CommitsQuery (..)
  , RefQuery (..)
  , RawText
  , DiffOf (..)
  , CommitDiffOf (..)
  , Entry (..)
  , ApiCommit (..)
  , IssueDetail (..)
  , ApiComment (..)
  , NewComment (..)
  , IssueEdit (..)
  , MergeOption (..)
  , NewReview (..)
  , CreateIssue (..)
  , CreatePull (..)
  , Created (..)
  , BranchName (..)
  , RepoLabel (..)
  , Reviewers (..)
  , NotificationQuery (..)
  , ThreadQuery (..)
  , ThreadState (..)
  , SearchQuery (..)
  , IssueType (..)
  , IssueState (..)
  , User (..)
  , Repository (..)
  , RepositoryRef (..)
  , Thread (..)
  , Subject (..)
  , Issue (..)
  , Label (..)
  , PullRequest (..)
  , Branch (..)
  , Review (..)
  , CombinedStatus (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BL
import Data.List.NonEmpty (NonEmpty (..))
import Data.Aeson.Types (Parser)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client (AsClientT, ClientM, client)

import Usta.Types.Commit (Commit (..), Sha)
import Usta.Types.Discussion (Comment (..), Merge (..), Verdict (..))
import Usta.Types.Count (Count)
import Usta.Types.Label (Color)
import qualified Usta.Types.Label as L
import Usta.Types.Login (Login)
import Usta.Types.Number (Number)
import Usta.Types.Parse (parseM, unparse)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Servant.NamedQueryParam (Named (..), RecordQueryParam, ToQuery)
import Usta.Types.ThreadId (ThreadId)

-- | Paths under @/api/v1@.
type Routes :: Type -> Type
data Routes mode = Routes
  { me :: mode :- "user" :> Get '[JSON] User
  , repositories :: mode :- "user" :> "repos" :> RecordQueryParam ListQuery :> Get '[JSON] [Repository]
  , repository :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> Get '[JSON] Repository
  , notifications :: mode :- "notifications" :> RecordQueryParam NotificationQuery :> Get '[JSON] [Thread]
  , markThread
      :: mode :- "notifications" :> "threads" :> Capture "id" ThreadId :> RecordQueryParam ThreadQuery
           :> Verb 'PATCH 205 '[JSON] NoContent
  , search :: mode :- "repos" :> "issues" :> "search" :> RecordQueryParam SearchQuery :> Get '[JSON] [Issue]
  , pullRequest
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> Get '[JSON] PullRequest
  , reviews
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "reviews" :> RecordQueryParam ListQuery :> Get '[JSON] [Review]
  , status
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "commits" :> Capture "ref" Text
           :> "status" :> Get '[JSON] CombinedStatus
  , pullDiff
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" DiffOf
           :> Get '[RawText] Text
  , pullCommits
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "commits" :> RecordQueryParam ListQuery :> Get '[JSON] [ApiCommit]
  , commits
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "commits"
           :> RecordQueryParam CommitsQuery :> Get '[JSON] [ApiCommit]
  , contents
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "contents"
           :> RecordQueryParam RefQuery :> Get '[JSON] [Entry]
  , raw
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "raw" :> Capture "path" Text
           :> RecordQueryParam RefQuery :> Get '[RawText] Text
  , commitDiff
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "git" :> "commits"
           :> Capture "sha" CommitDiffOf :> Get '[RawText] Text
  , issue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "index" Number
           :> Get '[JSON] IssueDetail
    -- ^ Pull requests are issues here too.
  , issueComments
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "index" Number
           :> "comments" :> Get '[JSON] [ApiComment]
    -- ^ Every comment at once; Forgejo does not page them.
  , postComment
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "index" Number
           :> "comments" :> ReqBody '[JSON] NewComment :> Verb 'POST 201 '[JSON] ApiComment
  , editIssue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "index" Number
           :> ReqBody '[JSON] IssueEdit :> Verb 'PATCH 201 '[JSON] NoContent
  , merge
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "merge" :> ReqBody '[JSON] MergeOption :> Post '[JSON] NoContent
  , review
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "reviews" :> ReqBody '[JSON] NewReview :> Post '[JSON] NoContent
  , createIssue
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues"
           :> ReqBody '[JSON] CreateIssue :> Verb 'POST 201 '[JSON] Created
  , createPull
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls"
           :> ReqBody '[JSON] CreatePull :> Verb 'POST 201 '[JSON] Created
  , requestReviewers
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "requested_reviewers" :> ReqBody '[JSON] Reviewers :> Verb 'POST 201 '[JSON] NoContent
  , branches
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "branches"
           :> RecordQueryParam ListQuery :> Get '[JSON] [BranchName]
  , labels
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "labels"
           :> RecordQueryParam ListQuery :> Get '[JSON] [RepoLabel]
  }
  deriving stock (Generic)

api :: Routes (AsClientT ClientM)
api = client (Proxy @(NamedRoutes Routes))

-- | One page of a list. Forgejo pages from 1; a short page is the last.
type ListQuery :: Type
data ListQuery = ListQuery
  { page :: Maybe Int
  , limit :: Maybe Int
  }
  deriving stock (Generic)

instance ToQuery ListQuery

-- | What a notification thread becomes.
type ThreadQuery :: Type
newtype ThreadQuery = ThreadQuery
  { toStatus :: Named "to-status" ThreadState
  }
  deriving stock (Generic)

instance ToQuery ThreadQuery

type ThreadState :: Type
data ThreadState = Read | Unread
  deriving stock (Eq, Show)

instance ToHttpApiData ThreadState where
  toQueryParam state = case state of
    Read -> "read"
    Unread -> "unread"

-- | @all@ includes notifications already read.
type NotificationQuery :: Type
data NotificationQuery = NotificationQuery
  { all :: Bool
  , since :: Maybe UTCTime
  , page :: Maybe Int
  , limit :: Maybe Int
  }
  deriving stock (Generic)

instance ToQuery NotificationQuery

-- | Issues or pull requests across repositories. Each flag set to 'True'
-- keeps those involving the user that way.
type SearchQuery :: Type
data SearchQuery = SearchQuery
  { kind :: Named "type" IssueType
  , state :: IssueState
  , since :: Maybe UTCTime
  , assigned :: Maybe Bool
  , created :: Maybe Bool
  , mentioned :: Maybe Bool
  , reviewRequested :: Named "review_requested" (Maybe Bool)
  , page :: Maybe Int
  , limit :: Maybe Int
  }
  deriving stock (Generic)

instance ToQuery SearchQuery

-- | A branch's commits; the details Forgejo can add are turned off.
type CommitsQuery :: Type
data CommitsQuery = CommitsQuery
  { sha :: Text
    -- ^ The branch or commit to start from.
  , limit :: Int
  , stat :: Bool
  , verification :: Bool
  , files :: Bool
  }
  deriving stock (Generic)

instance ToQuery CommitsQuery

type RefQuery :: Type
newtype RefQuery = RefQuery {ref :: Text}
  deriving stock (Generic)

instance ToQuery RefQuery

-- | A file sent as is: Forgejo labels raw files and diffs as plain text, a
-- diff or a download, all read here as UTF-8.
type RawText :: Type
data RawText

instance Accept RawText where
  contentTypes _ = "text/plain" :| ["text/x-diff", "text/x-patch", "text/markdown", "application/octet-stream"]

instance MimeUnrender RawText Text where
  mimeUnrender _ = Right . decodeUtf8Lenient . BL.toStrict

-- | The path segment "3.diff": pull request 3 as a diff.
type DiffOf :: Type
newtype DiffOf = DiffOf Number

instance ToHttpApiData DiffOf where
  toUrlPiece (DiffOf n) = toUrlPiece n <> ".diff"

-- | The path segment "<sha>.diff": a commit as a diff.
type CommitDiffOf :: Type
newtype CommitDiffOf = CommitDiffOf Sha

instance ToHttpApiData CommitDiffOf where
  toUrlPiece (CommitDiffOf sha) = toUrlPiece sha <> ".diff"

-- | A file or folder in a repository's tree.
type Entry :: Type
data Entry = Entry
  { name :: Text
  , kind :: Text
    -- ^ "file", "dir", "symlink" or "submodule".
  }

instance FromJSON Entry where
  parseJSON = withObject "Entry" $ \o -> Entry <$> o .: "name" <*> o .: "type"

-- | A commit as Forgejo lists it, read straight into usta's.
type ApiCommit :: Type
newtype ApiCommit = ApiCommit {commit :: Commit}

instance FromJSON ApiCommit where
  parseJSON = withObject "Commit" $ \o -> do
    sha <- o .: "sha" >>= parseM
    details <- o .: "commit"
    message <- details .: "message"
    author <- details .: "author"
    name <- author .: "name"
    date <- author .: "date"
    let (subject, rest) = T.breakOn "\n" message
    pure (ApiCommit Commit {sha = sha, author = name, authoredAt = date, subject = T.strip subject, body = T.strip rest})

-- | One issue or pull request, with its description and side details.
type IssueDetail :: Type
data IssueDetail = IssueDetail
  { opening :: Comment
  , closed :: Bool
  , merged :: Bool
  , assignees :: [Login]
  , labels :: [L.Label]
  , milestone :: Maybe Text
  }

instance FromJSON IssueDetail where
  parseJSON = withObject "Issue" $ \o -> do
    user <- o .: "user"
    body <- o .:? "body" .!= ""
    createdAt <- o .: "created_at"
    state <- o .: "state" :: Parser Text
    assignees <- o .:? "assignees" .!= []
    labels <- o .:? "labels" .!= []
    milestone <- o .:? "milestone"
    pull <- o .:? "pull_request"
    merged <- maybe (pure False) (\p -> p .:? "merged" .!= False) pull
    title <- traverse (.: "title") milestone
    pure IssueDetail
      { opening = Comment {author = (user :: User).login, body = body, createdAt = createdAt}
      , closed = state == "closed"
      , merged = merged
      , assignees = map (\(User login) -> login) assignees
      , labels = [L.Label {name = l.name, color = l.color} | l <- labels :: [Label]]
      , milestone = title
      }

type ApiComment :: Type
newtype ApiComment = ApiComment {comment :: Comment}

instance FromJSON ApiComment where
  parseJSON = withObject "Comment" $ \o -> do
    user <- o .: "user"
    body <- o .: "body"
    createdAt <- o .: "created_at"
    pure (ApiComment Comment {author = (user :: User).login, body = body, createdAt = createdAt})

type NewComment :: Type
newtype NewComment = NewComment {body :: Text}

instance ToJSON NewComment where
  toJSON c = object ["body" .= c.body]

-- | Closes or reopens an issue or pull request.
type IssueEdit :: Type
newtype IssueEdit = IssueEdit {closed :: Bool}

instance ToJSON IssueEdit where
  toJSON e = object ["state" .= (if e.closed then "closed" else "open" :: Text)]

type MergeOption :: Type
data MergeOption = MergeOption
  { how :: Merge
  , deleteBranch :: Bool
  }

instance ToJSON MergeOption where
  toJSON m = object
    [ "Do" .= case m.how of
        Squash -> "squash" :: Text
        MergeCommit -> "merge"
        Rebase -> "rebase"
    , "delete_branch_after_merge" .= m.deleteBranch
    ]

type NewReview :: Type
data NewReview = NewReview
  { verdict :: Verdict
  , body :: Text
  }

instance ToJSON NewReview where
  toJSON r = object
    [ "event" .= case r.verdict of
        Approve -> "APPROVED" :: Text
        RequestChanges -> "REQUEST_CHANGES"
        CommentOnly -> "COMMENT"
    , "body" .= r.body
    ]

-- | Labels go by id; the adapter finds them by name first.
type CreateIssue :: Type
data CreateIssue = CreateIssue
  { title :: Text
  , body :: Text
  , labels :: [Int64]
  , assignees :: [Login]
  }

instance ToJSON CreateIssue where
  toJSON c = object
    [ "title" .= c.title
    , "body" .= c.body
    , "labels" .= c.labels
    , "assignees" .= map unparse c.assignees
    ]

type CreatePull :: Type
data CreatePull = CreatePull
  { title :: Text
  , body :: Text
  , head :: Text
  , base :: Text
  , labels :: [Int64]
  }

instance ToJSON CreatePull where
  toJSON c = object
    [ "title" .= c.title
    , "body" .= c.body
    , "head" .= c.head
    , "base" .= c.base
    , "labels" .= c.labels
    ]

-- | What a create answers with that usta reads: the new number.
type Created :: Type
newtype Created = Created {number :: Number}

instance FromJSON Created where
  parseJSON = withObject "Created" $ \o -> Created <$> o .: "number"

type Reviewers :: Type
newtype Reviewers = Reviewers {reviewers :: [Login]}

instance ToJSON Reviewers where
  toJSON r = object ["reviewers" .= map unparse r.reviewers]

type BranchName :: Type
newtype BranchName = BranchName {name :: Text}

instance FromJSON BranchName where
  parseJSON = withObject "Branch" $ \o -> BranchName <$> o .: "name"

type RepoLabel :: Type
data RepoLabel = RepoLabel
  { id :: Int64
  , name :: Text
  }

instance FromJSON RepoLabel where
  parseJSON = withObject "Label" $ \o -> RepoLabel <$> o .: "id" <*> o .: "name"

type IssueType :: Type
data IssueType = Issues | Pulls

instance ToHttpApiData IssueType where
  toQueryParam t = case t of
    Issues -> "issues"
    Pulls -> "pulls"

type IssueState :: Type
data IssueState = Open | Closed

instance ToHttpApiData IssueState where
  toQueryParam s = case s of
    Open -> "open"
    Closed -> "closed"

type User :: Type
newtype User = User {login :: Login}

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "login"

type Repository :: Type
data Repository = Repository
  { fullName :: RepoName
  , description :: Text
  , defaultBranch :: Text
  , private :: Bool
  , openIssues :: Count
  , openPullRequests :: Count
  , updatedAt :: UTCTime
  , cloneUrl :: Text
  , sshUrl :: Maybe Text
    -- ^ 'Nothing' when ssh is turned off; Forgejo then sends "".
  }

instance FromJSON Repository where
  parseJSON = withObject "Repository" $ \o -> Repository
    <$> o .: "full_name"
    <*> o .:? "description" .!= ""
    <*> o .: "default_branch"
    <*> o .: "private"
    <*> o .: "open_issues_count"
    <*> o .: "open_pr_counter"
    <*> o .: "updated_at"
    <*> o .: "clone_url"
    <*> (nonEmpty <$> o .:? "ssh_url" .!= "")
    where
      nonEmpty t = if T.null t then Nothing else Just t

-- | The repository a thread or an issue is in.
type RepositoryRef :: Type
newtype RepositoryRef = RepositoryRef {fullName :: RepoName}

instance FromJSON RepositoryRef where
  parseJSON = withObject "RepositoryRef" $ \o -> RepositoryRef <$> o .: "full_name"

type Thread :: Type
data Thread = Thread
  { threadId :: ThreadId
  , repository :: RepositoryRef
  , subject :: Subject
  , unread :: Bool
  , updatedAt :: UTCTime
  }

instance FromJSON Thread where
  parseJSON = withObject "Thread" $ \o -> Thread
    <$> (o .: "id" >>= \n -> parseM (T.pack (show (n :: Int64))))
    <*> o .: "repository"
    <*> o .: "subject"
    <*> o .: "unread"
    <*> o .: "updated_at"

-- | @kind@ is Forgejo's subject type: "Issue", "Pull", "Commit" or
-- "Repository". @url@ is the API URL of the subject, which ends in its
-- number for issues and pull requests.
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

-- | An issue or, with @pullRequest@ set, a pull request, as search lists
-- them.
type Issue :: Type
data Issue = Issue
  { number :: Number
  , title :: Text
  , user :: User
  , labels :: [Label]
  , comments :: Count
  , repository :: RepositoryRef
  , isPullRequest :: Bool
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , closedAt :: Maybe UTCTime
  }

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \o -> do
    pull <- o .:? "pull_request" :: Parser (Maybe Value)
    Issue
      <$> o .: "number"
      <*> o .: "title"
      <*> o .: "user"
      <*> o .:? "labels" .!= []
      <*> o .: "comments"
      <*> o .: "repository"
      <*> pure (isJust pull)
      <*> o .: "created_at"
      <*> o .: "updated_at"
      <*> o .:? "closed_at"

type Label :: Type
data Label = Label
  { name :: Text
  , color :: Color
  }

instance FromJSON Label where
  parseJSON = withObject "Label" $ \o -> Label <$> o .: "name" <*> o .: "color"

type PullRequest :: Type
data PullRequest = PullRequest
  { head :: Branch
  , base :: Branch
  , draft :: Bool
  , merged :: Bool
  , changedFiles :: Maybe Count
  , requestedReviewers :: [User]
  }

instance FromJSON PullRequest where
  parseJSON = withObject "PullRequest" $ \o -> PullRequest
    <$> o .: "head"
    <*> o .: "base"
    <*> o .:? "draft" .!= False
    <*> o .:? "merged" .!= False
    <*> o .:? "changed_files"
    <*> o .:? "requested_reviewers" .!= []

type Branch :: Type
data Branch = Branch
  { ref :: Text
  , sha :: Text
  }

instance FromJSON Branch where
  parseJSON = withObject "Branch" $ \o -> Branch <$> o .: "ref" <*> o .: "sha"

-- | @state@ is "APPROVED", "REQUEST_CHANGES", "COMMENT", "PENDING" or
-- "REQUEST_REVIEW"; @stale@ when commits came after it.
type Review :: Type
data Review = Review
  { state :: Text
  , user :: Maybe User
  , stale :: Bool
  , dismissed :: Bool
  }

instance FromJSON Review where
  parseJSON = withObject "Review" $ \o -> Review
    <$> o .: "state"
    <*> o .:? "user"
    <*> o .:? "stale" .!= False
    <*> o .:? "dismissed" .!= False

-- | @state@ is "pending", "success", "error", "failure", "warning" or
-- "skipped"; no statuses at all when @total@ is 0.
type CombinedStatus :: Type
data CombinedStatus = CombinedStatus
  { state :: Text
  , total :: Int
  }

instance FromJSON CombinedStatus where
  parseJSON = withObject "CombinedStatus" $ \o -> CombinedStatus
    <$> o .:? "state" .!= ""
    <*> o .:? "total_count" .!= 0
