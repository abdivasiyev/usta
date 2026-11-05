{-# LANGUAGE OverloadedStrings #-}

-- | A 'Forge' for one GitHub account, on github.com or a GitHub
-- Enterprise server.
--
-- The notifications come from the REST API with their ETag, so an inbox
-- that did not change costs nothing against the rate limit. Pull
-- requests, issues and repositories come from GraphQL searches, one for
-- each way the user is involved: a pull request arrives with its checks,
-- review decision and counts in the same answer. A full sync asks for the
-- open ones and those closed in the last two weeks; the others for what
-- was updated since the last one, in any state. Repositories come only
-- with a full sync.
module Usta.Forge.GitHub
  ( github
  , whoAmI
  , restUrl
  , graphUrl
  ) where

import Control.Exception (throwIO)
import Data.Aeson (Value (Null, String), object, (.=))
import Data.ByteString (ByteString)
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Network.HTTP.Client as HTTP
import Servant.API (getResponse, lookupResponseHeader, ResponseHeader (..))
import Servant.Client (BaseUrl (..), ClientM, Scheme (Https))
import Text.Read (readMaybe)

import Usta.Forge (Failure (..), Fetched (..), Forge (..), Known (..))
import qualified Usta.Forge.GitHub.Api as Api
import Usta.Forge.Http (Client, connect, run, runChanged)
import Usta.Types.Host (Host)
import Usta.Types.Involvement (Involvement (..))
import qualified Usta.Types.Issue as I
import qualified Usta.Types.Label as L
import Usta.Types.Login (Login)
import qualified Usta.Types.Notification as N
import Usta.Types.Parse (parseM, unparse)
import qualified Usta.Types.PullRequest as P
import Usta.Types.Ref (Ref (..))
import qualified Usta.Types.Repository as R
import Usta.Types.Servant.NamedQueryParam (Named (..))
import Usta.Types.Token (Token)

-- | Where the REST API is: @api.github.com@, or a server's @/api/v3@.
restUrl :: Host -> BaseUrl
restUrl host
  | unparse host == "github.com" = BaseUrl Https "api.github.com" 443 ""
  | otherwise = (onHost host) {baseUrlPath = "/api/v3"}

-- | Where GraphQL is: @api.github.com/graphql@, or a server's
-- @/api/graphql@.
graphUrl :: Host -> BaseUrl
graphUrl host
  | unparse host == "github.com" = BaseUrl Https "api.github.com" 443 "/graphql"
  | otherwise = (onHost host) {baseUrlPath = "/api/graphql"}

onHost :: Host -> BaseUrl
onHost host = BaseUrl Https (T.unpack name) (fromMaybe 443 (readMaybe (T.unpack (T.drop 1 port)))) ""
  where
    (name, port) = T.breakOn ":" (unparse host)

-- | The forge of the account @token@ signs in as, on @host@; @token@ is
-- read for each call.
github :: HTTP.Manager -> Host -> IO Token -> IO Forge
github manager host token = do
  let rest = connect manager (restUrl host) (authorization <$> token)
      graph = connect manager (graphUrl host) (authorization <$> token)
  pure Forge
    { notifications = notificationsOf rest host
    , pullRequests = \known -> Fetched Nothing <$> pullRequestsOf graph host known.since
    , issues = \known -> Fetched Nothing <$> issuesOf graph host known.since
    , repositories = \known -> case known.since of
        Just _ -> pure NotModified
        Nothing -> Fetched Nothing <$> repositoriesOf graph host
    , markRead = \thread -> () <$ run rest (Api.api.markThread thread)
    , markUnread = \_ -> throwIO (Refused "GitHub cannot mark a notification unread again.")
    , cloneUrls = soon
    , pullDiff = soon
    , pullCommits = soon
    , threads = soon
    , checks = soon
    , checkLog = soon
    , reviewers = soon
    , requestReview = soon
    , unrequestReview = soon
    , readiness = soon
    , markReady = soon
    , updateBranch = soon
    , scheduleMerge = soon
    , cancelScheduledMerge = soon
    , reviewCandidates = soon
    , reply = soon
    , recentCommits = soon
    , readme = soon
    , commitDiff = soon
    , discussion = soon
    , comment = soon
    , editComment = soon
    , deleteComment = soon
    , editTitle = soon
    , watching = soon
    , setWatching = soon
    , timeline = soon
    , repoPullRequests = soon
    , repoIssues = soon
    , runs = soon
    , react = soon
    , setClosed = soon
    , merge = soon
    , review = soon
    , discardPending = soon
    , createIssue = soon
    , createPull = soon
    , branches = soon
    , branchList = soon
    , tags = soon
    , upload = soon
    , search = soon
    , starred = soon
    , setStarred = soon
    , releases = soon
    , fileHistory = soon
    , history = soon
    , labels = soon
    , setLabels = soon
    , createLabel = soon
    , assignable = soon
    , setAssignees = soon
    , milestones = soon
    , setMilestone = soon
    , compare = soon
    , compareDiff = soon
    , tree = soon
    , file = soon
    }

-- | What the GitHub client does not do yet; each of its parts replaces
-- one of these.
soon :: Soon a => a
soon = later

type Soon :: Type -> Constraint
class Soon a where
  later :: a

instance Soon (IO a) where
  later = throwIO (Refused "This is not on GitHub in usta yet.")

instance Soon b => Soon (a -> b) where
  later = const later

-- | Who @token@ signs in as.
whoAmI :: HTTP.Manager -> Host -> Token -> IO Login
whoAmI manager host token = (.login) <$> run (connect manager (restUrl host) (pure (authorization token))) Api.api.me

authorization :: Token -> ByteString
authorization token = "Bearer " <> encodeUtf8 (unparse token)

-- | The last month's notifications, read ones too. The first page is
-- asked with the ETag of the last answer; GitHub lists the latest first,
-- so an unchanged first page means nothing changed.
notificationsOf :: Client -> Host -> Known -> IO (Fetched [N.Notification])
notificationsOf client host known = do
  month <- addUTCTime (-30 * nominalDay) <$> getCurrentTime
  let page n tag = Api.api.notifications Api.NotificationQuery {all = True, since = Nothing, page = Just n, perPage = Named (Just 50)} tag
  first <- runChanged client (page 1 known.etag)
  case first of
    Nothing -> pure NotModified
    Just answer -> do
      let tag = case lookupResponseHeader @"ETag" answer of
            Header t -> Just t
            _ -> Nothing
          threads0 = getResponse answer
          -- Older pages until one reaches past the month, at most ten.
          more n got
            | n > 10 || length got < 50 * (n - 1) || any (\t -> t.updatedAt < month) got = pure got
            | otherwise = do
                next <- getResponse <$> run client (page n Nothing)
                if null next then pure got else more (n + 1) (got <> next)
      threads <- more 2 threads0
      Fetched tag <$> traverse (fromThread host) [t | t <- threads, t.updatedAt >= month]

fromThread :: Host -> Api.Thread -> IO N.Notification
fromThread host t = do
  subject <- case (t.subject.kind, numberAtEnd t.subject.url) of
    ("PullRequest", Just n) -> N.PullRequestSubject <$> parseM n
    ("Issue", Just n) -> N.IssueSubject <$> parseM n
    _ -> pure N.OtherSubject
  pure N.Notification
    { threadId = t.threadId
    , host = host
    , repo = t.repository.fullName
    , subject = subject
    , title = t.subject.title
    , reason = lookup t.reason reasons
    , unread = t.unread
    , updatedAt = t.updatedAt
    }
  where
    numberAtEnd url = readMaybe (T.unpack (T.takeWhileEnd (/= '/') url)) :: Maybe Int
    reasons =
      [ ("review_requested", N.ReviewRequested), ("mention", N.Mentioned), ("team_mention", N.TeamMentioned)
      , ("assign", N.Assigned), ("author", N.Authored), ("comment", N.Commented), ("ci_activity", N.CiActivity)
      , ("state_change", N.StateChanged), ("subscribed", N.Subscribed), ("manual", N.Subscribed) ]

-- | Each way a pull request or issue involves the user, as a search
-- qualifier.
ways :: [(Involvement, Text)]
ways = [(AsReviewer, "review-requested:@me"), (AsAuthor, "author:@me"), (AsAssignee, "assignee:@me"), (AsMentioned, "mentions:@me")]

-- | What one search asks, for a full sync or since a time.
windows :: Maybe UTCTime -> IO [Text]
windows since = case since of
  Just t -> pure ["updated:>=" <> T.pack (iso8601Show t)]
  Nothing -> do
    fortnight <- addUTCTime (-14 * nominalDay) <$> getCurrentTime
    pure ["is:open", "is:closed updated:>=" <> T.pack (iso8601Show fortnight)]

-- | Every page of a search, at most 25 (GitHub stops at 1000 results).
searched :: Client -> (Api.GraphQuery -> ClientM (Api.Answer (Api.Page a))) -> Text -> Text -> IO [a]
searched client route fields q = go (1 :: Int) Nothing
  where
    go n after = do
      Api.Answer found <- run client (route Api.GraphQuery
        { query = Api.searchQuery fields
        , variables = object ["q" .= q, "after" .= maybe Null String after] })
      case found.next of
        Just cursor | n < 25 -> (found.nodes <>) <$> go (n + 1) (Just cursor)
        _ -> pure found.nodes

-- | Found by each way, kept once with every way it was found by.
gathered :: Ord k => (a -> k) -> [(Involvement, [a])] -> [(a, [Involvement])]
gathered key found = Map.elems (Map.fromListWith (\(_, i) (a, j) -> (a, j <> i)) [(key a, (a, [way])) | (way, as) <- found, a <- as])

pullRequestsOf :: Client -> Host -> Maybe UTCTime -> IO [P.PullRequest]
pullRequestsOf client host since = do
  qs <- windows since
  found <- traverse (\(way, q) -> (,) way . concat <$> traverse (\w -> searched client Api.graph.searchPulls Api.pullFields (T.unwords ["is:pr archived:false", q, w])) qs) ways
  traverse (pullOf host) (gathered (\p -> (p.repo, p.number)) found)

pullOf :: Host -> (Api.PullNode, [Involvement]) -> IO P.PullRequest
pullOf host (p, involvement) = do
  author <- maybe (parseM "ghost") pure p.author
  pure P.PullRequest
    { ref = Ref {host = host, repo = p.repo, number = p.number}
    , title = p.title
    , author = author
    , headBranch = p.headBranch
    , baseBranch = p.baseBranch
    , state = case p.state of
        "MERGED" -> P.Merged
        "CLOSED" -> P.Closed
        _ | p.draft -> P.Draft
        _ -> P.Open
    , checks = p.checks >>= \c -> lookup c [("SUCCESS", P.ChecksPassed), ("FAILURE", P.ChecksFailed), ("ERROR", P.ChecksFailed), ("PENDING", P.ChecksRunning), ("EXPECTED", P.ChecksRunning)]
    , review = case p.reviewDecision of
        Just "APPROVED" -> Just P.Approved
        Just "CHANGES_REQUESTED" -> Just P.ChangesRequested
        Just "REVIEW_REQUIRED" -> Just P.AwaitingReview
        _ | p.reviewRequests > 0 -> Just P.AwaitingReview
        _ -> Nothing
    , comments = p.comments
    , changedFiles = Just p.changedFiles
    , commits = Just p.commits
    , involvement = involvement
    , createdAt = p.createdAt
    , updatedAt = p.updatedAt
    }

issuesOf :: Client -> Host -> Maybe UTCTime -> IO [I.Issue]
issuesOf client host since = do
  qs <- windows since
  let mine = [w | w@(way, _) <- ways, way /= AsReviewer]
  found <- traverse (\(way, q) -> (,) way . concat <$> traverse (\w -> searched client Api.graph.searchIssues Api.issueFields (T.unwords ["is:issue archived:false", q, w])) qs) mine
  traverse (issueOf host) (gathered (\i -> (i.repo, i.number)) found)

issueOf :: Host -> (Api.IssueNode, [Involvement]) -> IO I.Issue
issueOf host (i, involvement) = do
  author <- maybe (parseM "ghost") pure i.author
  pure I.Issue
    { ref = Ref {host = host, repo = i.repo, number = i.number}
    , title = i.title
    , author = author
    , labels = [L.Label {name = l.name, color = l.color} | l <- i.labels]
    , comments = i.comments
    , involvement = involvement
    , createdAt = i.createdAt
    , updatedAt = i.updatedAt
    , closedAt = i.closedAt
    }

-- | The repositories, recently changed first, at most 1000.
repositoriesOf :: Client -> Host -> IO [R.Repository]
repositoriesOf client host = go (1 :: Int) Nothing
  where
    go n after = do
      Api.Answer (Api.Repos found) <- run client (Api.graph.repositories Api.GraphQuery
        { query = Api.reposQuery
        , variables = object ["after" .= maybe Null String after] })
      let here = map repositoryOf found.nodes
      case found.next of
        Just cursor | n < 10 -> (here <>) <$> go (n + 1) (Just cursor)
        _ -> pure here
    repositoryOf r = R.Repository
      { host = host
      , name = r.name
      , description = r.description >>= \d -> if T.null d then Nothing else Just d
      , defaultBranch = fromMaybe "main" r.defaultBranch
      , private = r.private
      , openPullRequests = r.openPullRequests
      , openIssues = r.openIssues
      , updatedAt = r.updatedAt
      }
