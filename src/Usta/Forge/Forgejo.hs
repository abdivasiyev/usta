{-# LANGUAGE OverloadedStrings #-}

-- | A 'Forge' for one Forgejo account: what sync asks, answered from
-- Forgejo's API in usta's types.
--
-- Forgejo sends no ETags on these lists. A full sync fetches them whole;
-- the others ask only for what was updated since the last one, in any
-- state, so a pull request merged or an issue closed meanwhile comes too.
-- Each pull request costs extra calls (its branches, reviews and checks);
-- branches and reviews are kept until the pull request changes. Checks
-- are asked for each one that changed, and again for those whose checks
-- were still running, since a CI run does not change the pull request.
-- The repository list comes only with a full sync.
module Usta.Forge.Forgejo
  ( forgejo
  , whoAmI
  ) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (nub, sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import qualified Network.HTTP.Client as HTTP
import Servant.Client (BaseUrl, ClientM)
import Data.Traversable (for)
import Text.Read (readMaybe)

import Usta.Forge (CloneUrls (..), Failure (..), Fetched (..), Forge (..), Known (..), LineNote (..))
import qualified Usta.Forge.Forgejo.Api as Api
import Usta.Forge.Http (Client, connect, run)
import Usta.Types.Diff (parseDiff)
import Usta.Types.Discussion (Discussion (..), NewIssue (..), NewPull (..), Verdict (..))
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
import qualified Usta.Types.Thread as Th
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
  running <- newIORef Map.empty
  -- Who the token signs in as, asked once, to find the user's reviews.
  me <- newIORef Nothing
  let myLogin = readIORef me >>= \case
        Just l -> pure l
        Nothing -> do
          l <- (.login) <$> run client Api.api.me
          writeIORef me (Just l)
          pure l
  pure Forge
    { notifications = \known -> Fetched Nothing <$> notificationsOf client host known.since
    , pullRequests = \known -> Fetched Nothing <$> pullRequestsOf client details running host known.since
    , issues = \known -> Fetched Nothing <$> issuesOf client host known.since
    , repositories = \known -> case known.since of
        Just _ -> pure NotModified
        Nothing -> Fetched Nothing <$> repositoriesOf client host
    , markRead = \thread -> () <$ run client (Api.api.markThread thread (Api.ThreadQuery (Named Api.Read)))
    , markUnread = \thread -> () <$ run client (Api.api.markThread thread (Api.ThreadQuery (Named Api.Unread)))
    , discussion = \repo n -> do
        d <- run client (Api.api.issue (owner repo) (shortName repo) n)
        cs <- run client (Api.api.issueComments (owner repo) (shortName repo) n)
        pure Discussion
          { opening = d.opening
          , comments = map (.comment) cs
          , closed = d.closed
          , merged = d.merged
          , assignees = d.assignees
          , labels = d.labels
          , milestone = d.milestone
          }
    , comment = \repo n text ->
        (.comment) <$> run client (Api.api.postComment (owner repo) (shortName repo) n (Api.NewComment text))
    , setClosed = \repo n closed ->
        () <$ run client (Api.api.editIssue (owner repo) (shortName repo) n (Api.IssueEdit closed))
    , merge = \repo n how delete ->
        () <$ run client (Api.api.merge (owner repo) (shortName repo) n Api.MergeOption {how = how, deleteBranch = delete})
    , review = \repo n verdict text notes ->
        () <$ run client (Api.api.review (owner repo) (shortName repo) n Api.NewReview
          {verdict = verdict, body = text, lines = [Api.LineComment {path = x.path, line = x.line, body = x.body} | x <- notes]})
    , createIssue = \repo new -> do
        ids <- labelIds client repo new.labels
        created <- run client $ Api.api.createIssue (owner repo) (shortName repo) Api.CreateIssue
          {title = new.title, body = new.body, labels = ids, assignees = new.assignees}
        pure created.number
    , createPull = \repo new -> do
        ids <- labelIds client repo new.labels
        -- Forgejo takes a title starting with "WIP:" for a draft.
        let title = if new.draft then "WIP: " <> new.title else new.title
        created <- run client $ Api.api.createPull (owner repo) (shortName repo) Api.CreatePull
          {title = title, body = new.body, head = new.from, base = new.into, labels = ids}
        unless (null new.reviewers) $
          () <$ run client (Api.api.requestReviewers (owner repo) (shortName repo) created.number (Api.Reviewers new.reviewers))
        pure created.number
    , branches = \repo -> map (.name) <$> run client
        (everyPage (\page limit -> Api.api.branches (owner repo) (shortName repo) Api.ListQuery {page = page, limit = limit}))
    , labels = \repo -> map (.name) <$> repoLabels client repo
    , tree = \repo ref -> do
        let page n = run client $ Api.api.tree (owner repo) (shortName repo) ref
              Api.TreeQuery {recursive = True, page = Just n, perPage = Named (Just 1000)}
            -- Pages until every entry the forge counted has come.
            collect n got = do
              t <- page n
              let now = got <> map (.entry) t.entries
              if null t.entries || length now >= t.totalCount then pure now else collect (n + 1) now
        collect 1 []
    , file = \repo ref path ->
        run client (Api.api.file (owner repo) (shortName repo) (T.splitOn "/" path) Api.RefQuery {ref = ref})
    , compare = \repo into tip -> do
        c <- run client (Api.api.compare (owner repo) (shortName repo) (Api.BaseHead into tip))
        pure (map (.commit) c.commits, c.files)
    , cloneUrls = \repo -> do
        r <- run client (Api.api.repository (owner repo) (shortName repo))
        pure CloneUrls {https = r.cloneUrl, ssh = r.sshUrl}
    , pullDiff = \repo n -> do
        text <- run client (Api.api.pullDiff (owner repo) (shortName repo) (Api.DiffOf n))
        either (throwIO . Refused) pure (parseDiff text)
    , pullCommits = \repo n -> map (.commit) <$> run client
        (everyPage (\page limit -> Api.api.pullCommits (owner repo) (shortName repo) n Api.ListQuery {page = page, limit = limit}))
    , threads = \repo n -> do
        login <- myLogin
        given <- run client (everyPage (\page limit -> Api.api.reviews (owner repo) (shortName repo) n Api.ListQuery {page = page, limit = limit}))
        said <- concat <$> traverse (\r -> run client (Api.api.reviewComments (owner repo) (shortName repo) n r.id)) [r | r <- given, r.comments > 0]
        let mine = [t | r <- given, Just u <- [r.user], u.login == login, Just t <- [r.submittedAt]]
        pure Th.Threads {threads = threadsOf said, lastReviewed = if null mine then Nothing else Just (maximum mine)}
    -- Forgejo has no reply to a thread; a comment on the same line joins it.
    , reply = \repo n t text -> () <$ run client (Api.api.review (owner repo) (shortName repo) n
        Api.NewReview {verdict = CommentOnly, body = "", lines = [Api.LineComment {path = t.path, line = t.line, body = text}]})
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
repoLabels :: Client -> RepoName -> IO [Api.RepoLabel]
repoLabels client repo = run client
  (everyPage (\page limit -> Api.api.labels (owner repo) (shortName repo) Api.ListQuery {page = page, limit = limit}))

-- | The ids of the labels named; a name the repository lacks is refused.
labelIds :: Client -> RepoName -> [Text] -> IO [Int64]
labelIds _ _ [] = pure []
labelIds client repo names = do
  known <- repoLabels client repo
  for names $ \name -> case [l.id | l <- known, l.name == name] of
    found : _ -> pure found
    [] -> throwIO (Refused ("The repository has no label " <> name <> "."))

everyPage :: (Maybe Int -> Maybe Int -> ClientM [a]) -> ClientM [a]
everyPage fetch = go 1
  where
    go n = do
      items <- fetch (Just n) (Just perPage)
      if length items < perPage then pure items else (items <>) <$> go (n + 1)
    perPage = 50

-- | The last 30 days of notifications, read ones too; only those updated
-- after @since@, when given.
notificationsOf :: Client -> Host -> Maybe UTCTime -> IO [N.Notification]
notificationsOf client host since = do
  month <- addUTCTime (-30 * nominalDay) <$> getCurrentTime
  threads <- run client $ everyPage $ \page limit ->
    Api.api.notifications Api.NotificationQuery {all = True, since = Just (maybe month (max month) since), page = page, limit = limit}
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

-- | The open pull requests involving the user, or, after @since@, those
-- updated since in any state and those whose checks were still running,
-- which @running@ keeps.
pullRequestsOf
  :: Client -> IORef (Map.Map (RepoName, Number) Details) -> IORef (Map.Map (RepoName, Number) (Api.Issue, [Involvement]))
  -> Host -> Maybe UTCTime -> IO [P.PullRequest]
pullRequestsOf client cache running host since = do
  changed <- searchAll client Api.Pulls (maybe Api.Open (const Api.AnyState) since) since ways
  waiting <- readIORef running
  let seen = Set.fromList [keyOf i | (i, _) <- changed]
      again = [entry | since /= Nothing, (key, entry) <- Map.toList waiting, key `Set.notMember` seen]
      asked = changed <> again
  found <- traverse one asked
  writeIORef running (Map.fromList [(keyOf i, entry) | (entry@(i, _), pr) <- zip asked found, pr.checks == Just P.ChecksRunning])
  pure found
  where
    keyOf i = (i.repository.fullName, i.number)
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

-- | Line comments as conversations: Forgejo keeps the comments on one
-- line of a file together, so they are grouped the same way.
threadsOf :: [Api.ReviewComment] -> [Th.Thread]
threadsOf said = mapMaybe thread (Map.elems grouped)
  where
    grouped = Map.fromListWith (flip (<>)) [((c.path, lineOf c), [c]) | c <- sortOn (.createdAt) said]
    lineOf c = if c.position > 0 then c.position else c.originalPosition
    thread cs = case cs of
      [] -> Nothing
      first : _ -> Just Th.Thread
        { path = first.path
        , line = lineOf first
        -- The hunk ends at the commented line; a few lines show the code.
        , code = reverse (take 4 (reverse (drop 1 (T.lines first.diffHunk))))
        , commit = T.take 7 first.commitId
        , resolvedBy = listToMaybe [u.login | c <- cs, Just u <- [c.resolver]]
        , notes = [Th.Note {author = u.login, body = c.body, createdAt = c.createdAt} | c <- cs, Just u <- [c.user]]
        , url = first.htmlUrl
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

-- | Open issues involving the user, and those closed in the last 14 days;
-- after @since@, those updated since in any state.
issuesOf :: Client -> Host -> Maybe UTCTime -> IO [I.Issue]
issuesOf client host since = do
  fortnight <- addUTCTime (-14 * nominalDay) <$> getCurrentTime
  let mine = [w | w <- ways, w.involvement /= AsReviewer]
  found <- case since of
    Just t -> searchAll client Api.Issues Api.AnyState (Just t) mine
    Nothing -> (<>) <$> searchAll client Api.Issues Api.Open Nothing mine <*> searchAll client Api.Issues Api.Closed (Just fortnight) mine
  pure (map fromIssue found)
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
