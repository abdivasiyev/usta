{-# LANGUAGE OverloadedStrings #-}

-- | The part of Forgejo's REST API (@/api/v1@) usta uses, as a servant API,
-- with the answers decoded into usta's value types as they arrive. Only the
-- fields usta reads are decoded; Forgejo sends many more.
module Usta.Forge.Forgejo.Api
  ( Routes (..)
  , api
  , Web (..)
  , web
  , ListQuery (..)
  , CommitsQuery (..)
  , RefQuery (..)
  , RawText
  , RawBytes
  , TreeQuery (..)
  , ApiTree (..)
  , ApiTreeEntry (..)
  , DiffOf (..)
  , CommitDiffOf (..)
  , Entry (..)
  , ApiCommit (..)
  , IssueDetail (..)
  , ApiComment (..)
  , NewComment (..)
  , IssueEdit (..)
  , MergeOption (..)
  , PullEdit (..)
  , TimelineEvent (..)
  , Style (..)
  , BranchProtection (..)
  , NewReview (..)
  , CreateIssue (..)
  , CreatePull (..)
  , Created (..)
  , BranchName (..)
  , RepoLabel (..)
  , Reviewers (..)
  , BaseHead (..)
  , Comparison (..)
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
  , Team (..)
  , Person (..)
  , Authored (..)
  , FileCommitsQuery (..)
  , ReviewComment (..)
  , LineComment (..)
  , CombinedStatus (..)
  , CommitStatus (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.ByteString (ByteString)
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
import Usta.Types.Readiness (UpdateStyle (..))
import Usta.Types.RepoName (RepoName)
import Usta.Types.Servant.NamedQueryParam (Named (..), RecordQueryParam, ToQuery)
import Usta.Types.ThreadId (ThreadId)
import qualified Usta.Types.Tree as Tree

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
  , reviewComments
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "reviews" :> Capture "id" Int64 :> "comments" :> Get '[JSON] [ReviewComment]
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
  , scheduleMerge
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "merge" :> ReqBody '[JSON] MergeOption :> Verb 'POST 201 '[JSON] NoContent
    -- ^ The same as 'merge' with 'whenChecksPass': Forgejo answers 201.
  , timeline
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "issues" :> Capture "index" Number
           :> "timeline" :> RecordQueryParam ListQuery :> Get '[JSON] [TimelineEvent]
  , cancelAutoMerge
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "merge" :> Verb 'DELETE 204 '[JSON] NoContent
  , editPull
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> ReqBody '[JSON] PullEdit :> Verb 'PATCH 201 '[JSON] NoContent
  , updatePull
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "update" :> QueryParam' '[Required, Strict] "style" Style :> Verb 'POST 200 '[JSON] NoContent
  , branchProtections
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "branch_protections"
           :> Get '[JSON] [BranchProtection]
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
  , unrequestReviewers
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "pulls" :> Capture "index" Number
           :> "requested_reviewers" :> ReqBody '[JSON] Reviewers :> Verb 'DELETE 204 '[JSON] NoContent
  , collaborators
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "collaborators"
           :> RecordQueryParam ListQuery :> Get '[JSON] [Person]
  , teams
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "teams" :> Get '[JSON] [Team]
  , teamMembers :: mode :- "teams" :> Capture "id" Int64 :> "members" :> Get '[JSON] [User]
  , fileCommits
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "commits"
           :> RecordQueryParam FileCommitsQuery :> Get '[JSON] [Authored]
  , branches
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "branches"
           :> RecordQueryParam ListQuery :> Get '[JSON] [BranchName]
  , labels
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "labels"
           :> RecordQueryParam ListQuery :> Get '[JSON] [RepoLabel]
  , compare
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "compare"
           :> Capture "basehead" BaseHead :> Get '[JSON] Comparison
  , tree
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "git" :> "trees"
           :> Capture "ref" Text :> RecordQueryParam TreeQuery :> Get '[JSON] ApiTree
  , file
      :: mode :- "repos" :> Capture "owner" Text :> Capture "repo" Text :> "raw" :> CaptureAll "path" Text
           :> RecordQueryParam RefQuery :> Get '[RawBytes] ByteString
    -- ^ The path goes one folder per segment, so its slashes stay slashes.
  }
  deriving stock (Generic)

api :: Routes (AsClientT ClientM)
api = client (Proxy @(NamedRoutes Routes))

-- | Paths of Forgejo's web pages, from the site's root, for what the API
-- does not give: an Actions job's log, as plain text, one line per line
-- the job printed, each after its time.
type Web :: Type -> Type
data Web mode = Web
  { jobLog
      :: mode :- Capture "owner" Text :> Capture "repo" Text :> "actions" :> "runs" :> Capture "run" Int
           :> "jobs" :> Capture "job" Int :> "attempt" :> Capture "attempt" Int :> "logs" :> Get '[RawText] Text
  }
  deriving stock (Generic)

web :: Web (AsClientT ClientM)
web = client (Proxy @(NamedRoutes Web))

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

-- | The latest commits on a branch that changed one file.
type FileCommitsQuery :: Type
data FileCommitsQuery = FileCommitsQuery
  { sha :: Text
  , path :: Text
  , limit :: Int
  , stat :: Bool
  , verification :: Bool
  , files :: Bool
  }
  deriving stock (Generic)

instance ToQuery FileCommitsQuery

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

-- | A file as it is, whatever type the forge says it has.
type RawBytes :: Type
data RawBytes

instance Accept RawBytes where
  contentTypes _ = "*/*" :| []

instance MimeUnrender RawBytes ByteString where
  mimeUnrender _ = Right . BL.toStrict

-- | One page of a tree, with every folder below it.
type TreeQuery :: Type
data TreeQuery = TreeQuery
  { recursive :: Bool
  , page :: Maybe Int
  , perPage :: Named "per_page" (Maybe Int)
  }
  deriving stock (Generic)

instance ToQuery TreeQuery

type ApiTree :: Type
data ApiTree = ApiTree
  { entries :: [ApiTreeEntry]
  , totalCount :: Int
  }

instance FromJSON ApiTree where
  parseJSON = withObject "Tree" $ \o -> ApiTree <$> o .:? "tree" .!= [] <*> o .:? "total_count" .!= 0

type ApiTreeEntry :: Type
newtype ApiTreeEntry = ApiTreeEntry {entry :: Tree.Entry}

instance FromJSON ApiTreeEntry where
  parseJSON = withObject "TreeEntry" $ \o -> do
    path <- o .: "path"
    kind <- o .: "type" :: Parser Text
    size <- o .:? "size"
    pure $ ApiTreeEntry $ if kind == "blob"
      then Tree.Entry {path = path, kind = Tree.File, size = size}
      else Tree.Entry {path = path, kind = Tree.Folder, size = Nothing}

-- | The path segment "3.diff": pull request 3 as a diff.
type DiffOf :: Type
newtype DiffOf = DiffOf Number

instance ToHttpApiData DiffOf where
  toUrlPiece (DiffOf n) = toUrlPiece n <> ".diff"

-- | The path segment "base...head": what head has that base does not.
type BaseHead :: Type
data BaseHead = BaseHead Text Text

instance ToHttpApiData BaseHead where
  toUrlPiece (BaseHead base tip) = toUrlPiece (base <> "..." <> tip)

-- | The commits, oldest first, and the files they change.
type Comparison :: Type
data Comparison = Comparison
  { commits :: [ApiCommit]
  , files :: [Text]
  }

instance FromJSON Comparison where
  parseJSON = withObject "Compare" $ \o -> do
    commits <- o .:? "commits" .!= []
    files <- o .:? "files" .!= []
    names <- traverse (withObject "File" (.: "filename")) files
    pure Comparison {commits = commits, files = names}

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
  , whenChecksPass :: Bool
    -- ^ Merge once the checks pass instead of now.
  }

instance ToJSON MergeOption where
  toJSON m = object
    [ "Do" .= case m.how of
        Squash -> "squash" :: Text
        MergeCommit -> "merge"
        Rebase -> "rebase"
    , "delete_branch_after_merge" .= m.deleteBranch
    , "merge_when_checks_succeed" .= m.whenChecksPass
    ]

-- | An event in an issue's or pull request's timeline, by its kind, e.g.
-- "pull_scheduled_merge" or "pull_cancel_scheduled_merge".
type TimelineEvent :: Type
newtype TimelineEvent = TimelineEvent {kind :: Text}

instance FromJSON TimelineEvent where
  parseJSON = withObject "TimelineEvent" $ \o -> TimelineEvent <$> o .:? "type" .!= ""

-- | A new title, which says whether it is a draft ("WIP: …").
type PullEdit :: Type
newtype PullEdit = PullEdit {title :: Text}

instance ToJSON PullEdit where
  toJSON e = object ["title" .= e.title]

-- | How Forgejo brings the base branch's commits into a pull request.
type Style :: Type
newtype Style = Style UpdateStyle

instance ToHttpApiData Style where
  toQueryParam (Style s) = case s of
    ByMerge -> "merge"
    ByRebase -> "rebase"

-- | A branch protection rule: the branch it names, or a pattern, and the
-- approvals it asks for.
type BranchProtection :: Type
data BranchProtection = BranchProtection {rule :: Text, requiredApprovals :: Int}

instance FromJSON BranchProtection where
  parseJSON = withObject "BranchProtection" $ \o -> do
    named <- o .:? "rule_name" .!= ""
    branch <- o .:? "branch_name" .!= ""
    BranchProtection (if T.null named then branch else named) <$> o .:? "required_approvals" .!= 0

type NewReview :: Type
data NewReview = NewReview
  { verdict :: Verdict
  , body :: Text
  , lines :: [LineComment]
    -- ^ Comments on lines of the diff, with the review.
  }

instance ToJSON NewReview where
  toJSON r = object
    [ "event" .= case r.verdict of
        Approve -> "APPROVED" :: Text
        RequestChanges -> "REQUEST_CHANGES"
        CommentOnly -> "COMMENT"
    , "body" .= r.body
    , "comments" .= r.lines
    ]

-- | A comment on one line of a file in the diff, by its line in the new
-- file.
type LineComment :: Type
data LineComment = LineComment
  { path :: Text
  , line :: Int
  , body :: Text
  }

instance ToJSON LineComment where
  toJSON c = object ["path" .= c.path, "new_position" .= c.line, "body" .= c.body]

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

-- | People by login and teams by name, to ask for a review or stop asking.
type Reviewers :: Type
data Reviewers = Reviewers {reviewers :: [Login], teams :: [Text]}

instance ToJSON Reviewers where
  toJSON r = object ["reviewers" .= map unparse r.reviewers, "team_reviewers" .= r.teams]

-- | A team of the repository's organization, which can be asked to review.
type Team :: Type
data Team = Team {id :: Int64, name :: Text, description :: Text}

instance FromJSON Team where
  parseJSON = withObject "Team" $ \o -> Team
    <$> o .:? "id" .!= 0
    <*> o .: "name"
    <*> o .:? "description" .!= ""

-- | Someone with their full name, which may be empty.
type Person :: Type
data Person = Person {login :: Login, fullName :: Text}

instance FromJSON Person where
  parseJSON = withObject "Person" $ \o -> Person <$> o .: "login" <*> o .:? "full_name" .!= ""

-- | Who made a commit, when the forge knows them as a user, and when.
type Authored :: Type
data Authored = Authored {author :: Maybe User, date :: UTCTime}

instance FromJSON Authored where
  parseJSON = withObject "Authored" $ \o -> do
    details <- o .: "commit"
    Authored <$> o .:? "author" <*> (details .: "author" >>= (.: "date"))

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
data IssueState = Open | Closed | AnyState

instance ToHttpApiData IssueState where
  toQueryParam s = case s of
    Open -> "open"
    Closed -> "closed"
    AnyState -> "all"

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
  , requestedTeams :: [Team]
  , user :: User
    -- ^ Who opened it.
  , title :: Text
  , mergeable :: Bool
    -- ^ Forgejo can merge it as it is: no conflicts.
  }

instance FromJSON PullRequest where
  parseJSON = withObject "PullRequest" $ \o -> PullRequest
    <$> o .: "head"
    <*> o .: "base"
    <*> o .:? "draft" .!= False
    <*> o .:? "merged" .!= False
    <*> o .:? "changed_files"
    <*> o .:? "requested_reviewers" .!= []
    <*> o .:? "requested_reviewers_teams" .!= []
    <*> o .: "user"
    <*> o .:? "title" .!= ""
    <*> o .:? "mergeable" .!= True

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
  { id :: Int64
  , state :: Text
  , user :: Maybe User
  , stale :: Bool
  , dismissed :: Bool
  , submittedAt :: Maybe UTCTime
  , comments :: Int
    -- ^ How many line comments it holds.
  }

instance FromJSON Review where
  parseJSON = withObject "Review" $ \o -> Review
    <$> o .: "id"
    <*> o .: "state"
    <*> o .:? "user"
    <*> o .:? "stale" .!= False
    <*> o .:? "dismissed" .!= False
    <*> o .:? "submitted_at"
    <*> o .:? "comments_count" .!= 0

-- | A comment on a line of a pull request's diff. @position@ is its line
-- in the new file, @originalPosition@ in the old one; whichever side it
-- was not on is 0.
type ReviewComment :: Type
data ReviewComment = ReviewComment
  { body :: Text
  , user :: Maybe User
  , path :: Text
  , position :: Int
  , originalPosition :: Int
  , commitId :: Text
  , diffHunk :: Text
  , resolver :: Maybe User
  , htmlUrl :: Text
  , createdAt :: UTCTime
  }

instance FromJSON ReviewComment where
  parseJSON = withObject "ReviewComment" $ \o -> ReviewComment
    <$> o .:? "body" .!= ""
    <*> o .:? "user"
    <*> o .: "path"
    <*> o .:? "position" .!= 0
    <*> o .:? "original_position" .!= 0
    <*> o .:? "commit_id" .!= ""
    <*> o .:? "diff_hunk" .!= ""
    <*> o .:? "resolver"
    <*> o .:? "html_url" .!= ""
    <*> o .: "created_at"

-- | @state@ is "pending", "success", "error", "failure", "warning" or
-- "skipped"; no statuses at all when @total@ is 0.
type CombinedStatus :: Type
data CombinedStatus = CombinedStatus
  { state :: Text
  , total :: Int
  , statuses :: [CommitStatus]
    -- ^ The latest of each context.
  }

instance FromJSON CombinedStatus where
  parseJSON = withObject "CombinedStatus" $ \o -> CombinedStatus
    <$> o .:? "state" .!= ""
    <*> o .:? "total_count" .!= 0
    <*> o .:? "statuses" .!= []

-- | One check's latest status. Forgejo Actions names it
-- "workflow / job (event)" and links it to the job's page, e.g.
-- "/owner/repo/actions/runs/1/jobs/0"; a skipped job is a "success" that
-- says "Has been skipped".
type CommitStatus :: Type
data CommitStatus = CommitStatus
  { status :: Text
  , context :: Text
  , description :: Text
  , targetUrl :: Text
  }

instance FromJSON CommitStatus where
  parseJSON = withObject "CommitStatus" $ \o -> CommitStatus
    <$> o .:? "status" .!= ""
    <*> o .:? "context" .!= ""
    <*> o .:? "description" .!= ""
    <*> o .:? "target_url" .!= ""
