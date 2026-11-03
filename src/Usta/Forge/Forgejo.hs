{-# LANGUAGE OverloadedStrings #-}

-- | A 'Forge' for one Forgejo account: what sync asks, answered from
-- Forgejo's API in usta's types.
--
-- Forgejo sends no ETags on these lists, so every sync fetches them whole.
-- Each pull request costs extra calls (its branches, reviews and checks);
-- branches and reviews are kept until the pull request changes, checks
-- are asked every time, since a CI run does not change the pull request.
module Usta.Forge.Forgejo
  ( forgejo
  , whoAmI
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import qualified Network.HTTP.Client as HTTP
import Servant.Client (BaseUrl, ClientM)
import Text.Read (readMaybe)

import Usta.Forge (CloneUrls (..), Failure (..), Fetched (..), Forge (..))
import qualified Usta.Forge.Forgejo.Api as Api
import Usta.Forge.Http (Client, connect, run)
import Usta.Types.Diff (parseDiff)
import Usta.Types.Host (Host)
import Usta.Types.Involvement (Involvement (..))
import qualified Usta.Types.Issue as I
import qualified Usta.Types.Label as L
import Usta.Types.Login (Login)
import qualified Usta.Types.Notification as N
import Usta.Types.Number (Number)
import Usta.Types.Parse (parseM, unparse)
import qualified Usta.Types.PullRequest as P
import Usta.Types.Ref (Ref (..))
import Usta.Types.RepoName (RepoName, owner, shortName)
import qualified Usta.Types.Repository as R
import Usta.Types.Servant.NamedQueryParam (Named (..))
import Usta.Types.Token (Token)

-- | The forge of the account @token@ signs in as, on @host@, whose API is at
-- @base@ (normally @https://host/api/v1@).
forgejo :: HTTP.Manager -> BaseUrl -> Host -> Token -> IO Forge
forgejo manager base host token = do
  let client = connect manager base (authorization token)
  details <- newIORef Map.empty
  pure Forge
    { notifications = const (Fetched Nothing <$> notificationsOf client host)
    , pullRequests = const (Fetched Nothing <$> pullRequestsOf client details host)
    , issues = const (Fetched Nothing <$> issuesOf client host)
    , repositories = const (Fetched Nothing <$> repositoriesOf client host)
    , markRead = \thread -> () <$ run client (Api.api.markRead thread)
    , cloneUrls = \repo -> do
        r <- run client (Api.api.repository (owner repo) (shortName repo))
        pure CloneUrls {https = r.cloneUrl, ssh = r.sshUrl}
    , pullDiff = \repo n -> do
        text <- run client (Api.api.pullDiff (owner repo) (shortName repo) (Api.DiffOf n))
        either (throwIO . Refused) pure (parseDiff text)
    , pullCommits = \repo n -> map (.commit) <$> run client
        (everyPage (\page limit -> Api.api.pullCommits (owner repo) (shortName repo) n Api.ListQuery {page = page, limit = limit}))
    , recentCommits = \repo branch -> map (.commit) <$> run client
        (Api.api.commits (owner repo) (shortName repo)
          Api.CommitsQuery {sha = branch, limit = 10, stat = False, verification = False, files = False})
    , commitDiff = \repo sha -> do
        text <- run client (Api.api.commitDiff (owner repo) (shortName repo) (Api.CommitDiffOf sha))
        either (throwIO . Refused) pure (parseDiff text)
    , readme = \repo branch -> do
        entries <- run client (Api.api.contents (owner repo) (shortName repo) Api.RefQuery {ref = branch})
        case [e.name | e <- entries, e.kind == "file", T.toLower (T.takeWhile (/= '.') e.name) == "readme"] of
          name : _ -> Just . (,) name <$> run client (Api.api.raw (owner repo) (shortName repo) name Api.RefQuery {ref = branch})
          [] -> pure Nothing
    }

-- | Who @token@ signs in as; throws 'Usta.Forge.SignedOut' when Forgejo
-- refuses it.
whoAmI :: HTTP.Manager -> BaseUrl -> Token -> IO Login
whoAmI manager base token = (.login) <$> run (connect manager base (authorization token)) Api.api.me

authorization :: Token -> ByteString
authorization token = "token " <> encodeUtf8 (unparse token)

-- | Every page of a list, 50 at a time.
everyPage :: (Maybe Int -> Maybe Int -> ClientM [a]) -> ClientM [a]
everyPage fetch = go 1
  where
    go n = do
      items <- fetch (Just n) (Just perPage)
      if length items < perPage then pure items else (items <>) <$> go (n + 1)
    perPage = 50

-- | The last 30 days of notifications, read ones too.
notificationsOf :: Client -> Host -> IO [N.Notification]
notificationsOf client host = do
  month <- addUTCTime (-30 * nominalDay) <$> getCurrentTime
  threads <- run client $ everyPage $ \page limit ->
    Api.api.notifications Api.NotificationQuery {all = True, since = Just month, page = page, limit = limit}
  traverse (fromThread host) threads

fromThread :: Host -> Api.Thread -> IO N.Notification
fromThread host t = do
  subject <- case (t.subject.kind, numberAtEnd t.subject.url) of
    ("Pull", Just n) -> N.PullRequestSubject <$> parseM n
    ("Issue", Just n) -> N.IssueSubject <$> parseM n
    _ -> pure N.OtherSubject
  pure N.Notification
    { threadId = t.threadId
    , host = host
    , repo = t.repository.fullName
    , subject = subject
    , title = t.subject.title
    , reason = Nothing
    , unread = t.unread
    , updatedAt = t.updatedAt
    }
  where
    -- ".../repos/owner/repo/issues/3": pull requests end the same way.
    numberAtEnd url = readMaybe (T.unpack (T.takeWhileEnd (/= '/') url)) :: Maybe Int

-- | How each search finds what involves the user.
type Way :: Type
data Way = Way
  { involvement :: Involvement
  , query :: Api.SearchQuery -> Api.SearchQuery
  }

ways :: [Way]
ways =
  [ Way AsReviewer (\q -> q {Api.reviewRequested = Named (Just True)})
  , Way AsAuthor (\q -> q {Api.created = Just True})
  , Way AsAssignee (\q -> q {Api.assigned = Just True})
  , Way AsMentioned (\q -> q {Api.mentioned = Just True})
  ]

-- | Every issue or pull request each way finds, once, with the ways that
-- found it.
searchAll :: Client -> Api.IssueType -> Api.IssueState -> Maybe UTCTime -> [Way] -> IO [(Api.Issue, [Involvement])]
searchAll client kind state since chosen = do
  found <- traverse (\w -> map (\i -> (i, [w.involvement])) <$> run client (everyPage (search w))) chosen
  let merged = Map.fromListWith (\new old -> (fst old, snd old <> snd new)) [(keyOf i, entry) | entry@(i, _) <- concat found]
  pure (sortOn (Down . (.updatedAt) . fst) [(i, nub ws) | (i, ws) <- Map.elems merged])
  where
    keyOf i = (i.repository.fullName, i.number)
    search w page limit = Api.api.search $ w.query Api.SearchQuery
      { kind = Named kind
      , state = state
      , since = since
      , assigned = Nothing
      , created = Nothing
      , mentioned = Nothing
      , reviewRequested = Named Nothing
      , page = page
      , limit = limit
      }

-- | What a pull request's own calls add to its search entry, as of the
-- search's @updatedAt@.
type Details :: Type
data Details = Details
  { asOf :: UTCTime
  , pull :: Api.PullRequest
  , review :: Maybe P.Review
  }

pullRequestsOf :: Client -> IORef (Map.Map (RepoName, Number) Details) -> Host -> IO [P.PullRequest]
pullRequestsOf client cache host = do
  found <- searchAll client Api.Pulls Api.Open Nothing ways
  traverse one found
  where
    one (i, involvement) = do
      let key = (i.repository.fullName, i.number)
          (o, r) = (owner i.repository.fullName, shortName i.repository.fullName)
      cached <- Map.lookup key <$> readIORef cache
      d <- case cached of
        Just d | d.asOf == i.updatedAt -> pure d
        _ -> do
          pull <- run client (Api.api.pullRequest o r i.number)
          given <- run client (everyPage (\page limit -> Api.api.reviews o r i.number Api.ListQuery {page = page, limit = limit}))
          let d = Details {asOf = i.updatedAt, pull = pull, review = reviewOf pull given}
          atomicModifyIORef' cache (\m -> (Map.insert key d m, ()))
          pure d
      combined <- run client (Api.api.status o r d.pull.head.sha)
      pure P.PullRequest
        { ref = Ref {host = host, repo = i.repository.fullName, number = i.number}
        , title = i.title
        , author = i.user.login
        , headBranch = d.pull.head.ref
        , baseBranch = d.pull.base.ref
        , state = stateOf d.pull
        , checks = checksOf combined
        , review = d.review
        , comments = i.comments
        , changedFiles = d.pull.changedFiles
        , commits = Nothing
        , involvement = involvement
        , createdAt = i.createdAt
        , updatedAt = i.updatedAt
        }

stateOf :: Api.PullRequest -> P.State
stateOf pull
  | pull.merged = P.Merged
  | pull.draft = P.Draft
  | otherwise = P.Open

-- | Each reviewer's last review counts, unless it is stale or dismissed.
reviewOf :: Api.PullRequest -> [Api.Review] -> Maybe P.Review
reviewOf pull given
  | "REQUEST_CHANGES" `elem` verdicts = Just P.ChangesRequested
  | "APPROVED" `elem` verdicts = Just P.Approved
  | not (null pull.requestedReviewers) = Just P.AwaitingReview
  | otherwise = Nothing
  where
    counted = [r | r <- given, not r.stale, not r.dismissed, r.state `elem` ["APPROVED", "REQUEST_CHANGES"]]
    lastBy = Map.fromList [(fmap (unparse . (.login)) r.user, r.state) | r <- counted]
    verdicts = Map.elems lastBy

checksOf :: Api.CombinedStatus -> Maybe P.Checks
checksOf combined
  | combined.total == 0 = Nothing
  | otherwise = case combined.state of
      "pending" -> Just P.ChecksRunning
      "failure" -> Just P.ChecksFailed
      "error" -> Just P.ChecksFailed
      _ -> Just P.ChecksPassed

-- | Open issues involving the user, and those closed in the last 14 days.
issuesOf :: Client -> Host -> IO [I.Issue]
issuesOf client host = do
  fortnight <- addUTCTime (-14 * nominalDay) <$> getCurrentTime
  let mine = [w | w <- ways, w.involvement /= AsReviewer]
  open <- searchAll client Api.Issues Api.Open Nothing mine
  closed <- searchAll client Api.Issues Api.Closed (Just fortnight) mine
  pure (map fromIssue (open <> closed))
  where
    fromIssue (i, involvement) = I.Issue
      { ref = Ref {host = host, repo = i.repository.fullName, number = i.number}
      , title = i.title
      , author = i.user.login
      , labels = [L.Label {name = l.name, color = l.color} | l <- i.labels]
      , comments = i.comments
      , involvement = involvement
      , createdAt = i.createdAt
      , updatedAt = i.updatedAt
      , closedAt = i.closedAt
      }

repositoriesOf :: Client -> Host -> IO [R.Repository]
repositoriesOf client host = do
  found <- run client $ everyPage $ \page limit -> Api.api.repositories Api.ListQuery {page = page, limit = limit}
  pure (map fromRepository found)
  where
    fromRepository r = R.Repository
      { host = host
      , name = r.fullName
      , description = if T.null r.description then Nothing else Just r.description
      , defaultBranch = r.defaultBranch
      , private = r.private
      , openPullRequests = r.openPullRequests
      , openIssues = r.openIssues
      , updatedAt = r.updatedAt
      }
