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

import Control.Exception (throwIO, try)
import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.Traversable (for)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Aeson (ToJSON (..), Value (Null, String), object, (.=))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.ByteString (ByteString)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Network.HTTP.Client as HTTP
import Servant.API (getResponse, lookupResponseHeader, ResponseHeader (..))
import Servant.Client (BaseUrl (..), ClientM, Scheme (Https))
import Text.Read (readMaybe)

import Usta.Forge (CloneUrls (..), Failure (..), Fetched (..), Forge (..), Known (..), LineNote (..))
import qualified Usta.Forge.GitHub.Api as Api
import Usta.Forge.Http (Client, connect, run, runChanged, runDone)
import Usta.Types.Host (Host)
import Usta.Types.Involvement (Involvement (..))
import qualified Usta.Types.Issue as I
import qualified Usta.Types.Branch as Br
import qualified Usta.Types.Check as C
import qualified Usta.Types.Commit as Cm
import Usta.Types.Diff (FileDiff, parseDiff)
import qualified Usta.Types.Discussion as D
import Usta.Types.Discussion (Merge (..), Verdict (..))
import qualified Usta.Types.Readiness as Rd
import Usta.Types.Readiness (UpdateStyle (..))
import qualified Usta.Types.Thread as Th
import qualified Usta.Types.Release as Rel
import qualified Usta.Types.Search as S
import qualified Usta.Types.Tree as Tr
import qualified Usta.Types.Workflow as W
import qualified Data.Aeson.Key as Key
import qualified Usta.Types.Label as L
import qualified Usta.Types.Listing as Lt
import qualified Usta.Types.Milestone as M
import Usta.Types.Number (Number)
import Usta.Types.RepoName (RepoName, owner, shortName)
import qualified Usta.Types.Reviewer as Rv
import qualified Usta.Types.Timeline as Tl
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
      -- Every page of a REST list, 100 at a time.
      pages :: (Api.ListQuery -> ClientM [a]) -> IO [a]
      pages route = go 1
        where
          go n = do
            got <- run rest (route Api.ListQuery {page = Just n, perPage = Named (Just 100)})
            if length got < 100 || n >= 20 then pure got else (got <>) <$> go (n + 1)
      commitsOn repo branch path n count = traverse commitOf =<< run rest (Api.api.commits (owner repo) (shortName repo)
        Api.CommitsQuery {sha = branch, path = path, page = Just n, perPage = Named (Just count)})
      stateOfPull repo n = asked graph (Api.graph.pullState Api.GraphQuery {query = Api.pullStateQuery, variables = itemVariables repo n Nothing})
      pullMutate repo n name extra = do
        st <- stateOfPull repo n
        () <$ asked graph (Api.graph.mutate Api.GraphQuery {query = Api.pullMutation name extra, variables = object ["id" .= st.nodeId]})
  -- Who the token signs in as, asked once.
  me <- newIORef Nothing
  let myLogin = readIORef me >>= \case
        Just l -> pure l
        Nothing -> do
          l <- (.login) <$> run rest Api.api.me
          writeIORef me (Just l)
          pure l
  pure Forge
    { notifications = notificationsOf rest host
    , pullRequests = \known -> Fetched Nothing <$> pullRequestsOf graph host known.since
    , issues = \known -> Fetched Nothing <$> issuesOf graph host known.since
    , repositories = \known -> case known.since of
        Just _ -> pure NotModified
        Nothing -> Fetched Nothing <$> repositoriesOf graph host
    , markRead = \thread -> () <$ run rest (Api.api.markThread thread)
    , markUnread = \_ -> throwIO (Refused "GitHub cannot mark a notification unread again.")
    , cloneUrls = \repo -> do
        r <- run rest (Api.api.repository (owner repo) (shortName repo))
        pure CloneUrls {https = r.cloneUrl, ssh = r.sshUrl}
    , pullDiff = \repo n -> run rest (Api.api.pullDiff (owner repo) (shortName repo) n) >>= diffOf
    , pullCommits = \repo n -> traverse commitOf =<< pages (Api.api.pullCommits (owner repo) (shortName repo) n)
    , threads = \repo n -> do
        st <- stateOfPull repo n
        let mine = [t | r <- st.reviews, r.author == Just st.viewer, r.state /= "PENDING", Just t <- [r.submittedAt]]
        notes <- traverse threadOf st.threads
        pure Th.Threads {threads = [t | Just t <- notes], lastReviewed = if null mine then Nothing else Just (maximum mine)}
    , checks = \repo n -> (\(Api.Checks cs) -> map checkOf cs) <$> asked graph (Api.graph.checks Api.GraphQuery {query = Api.checksQuery, variables = itemVariables repo n Nothing})
    , workflows = \repo -> do
        Api.Workflows ws <- run rest (Api.api.workflows (owner repo) (shortName repo) Api.ListQuery {page = Just 1, perPage = Named (Just 100)})
        pure [name | (name, _, state) <- ws, state /= "deleted"]
    , startable = \repo branch -> do
        let (o, r) = (owner repo, shortName repo)
        Api.Workflows ws <- run rest (Api.api.workflows o r Api.ListQuery {page = Just 1, perPage = Named (Just 100)})
        -- Each file as the branch has it; one it lacks, or that cannot be
        -- started by hand, is left out.
        found <- for [(name, path) | (name, path, state) <- ws, state == "active", ".github/workflows/" `T.isPrefixOf` path] $ \(name, path) -> do
          got <- try (run rest (Api.api.file o r (T.splitOn "/" path) (Just branch)))
          pure $ case got of
            Right bytes | Right (Just is) <- W.inputsOf bytes -> [W.Workflow {name = name, file = path, inputs = is}]
            Left (_ :: Failure) -> []
            _ -> []
        pure (concat found)
    , dispatch = \repo w branch values ->
        runDone rest (Api.api.dispatch (owner repo) (shortName repo) (snd (T.breakOnEnd "/" w.file))
          (object ["ref" .= branch, "inputs" .= object [Key.fromText k .= v | (k, v) <- values]]))
    , rerun = \repo i failedOnly -> runDone rest ((if failedOnly then Api.api.rerunFailed else Api.api.rerun) (owner repo) (shortName repo) i)
    , cancelRun = \repo i -> runDone rest (Api.api.cancelRun (owner repo) (shortName repo) i)
    , rerunJob = \repo job -> runDone rest (Api.api.rerunJob (owner repo) (shortName repo) (fromIntegral job.run))
    , jobSteps = \repo job -> do
        j <- run rest (Api.api.job (owner repo) (shortName repo) (fromIntegral job.run))
        pure (stateOf j.status j.conclusion, [(name, stateOf status conclusion) | (name, status, conclusion) <- j.steps])
    , checkLog = \repo job -> decodeUtf8Lenient <$> run rest (Api.api.jobLog (owner repo) (shortName repo) (fromIntegral job.run))
    , reviewers = \repo n -> do
        st <- stateOfPull repo n
        let reviewed = [ Rv.Reviewer {who = Rv.Person l, stance = s', stale = r.commit /= Just st.headSha && s' == Rv.Approved}
                       | r <- st.latest, Just l <- [r.author], Just s' <- [stanceOf r.state] ]
            waiting = [Rv.Reviewer {who = either Rv.Team Rv.Person w, stance = Rv.Waiting, stale = False} | w <- st.requested]
        pure (waiting <> [r | r <- reviewed, r.who `notElem` map (.who) waiting])
    , requestReview = \repo n whos ->
        () <$ run rest (Api.api.requestReviewers (owner repo) (shortName repo) n (reviewersBody whos))
    , unrequestReview = \repo n who ->
        () <$ run rest (Api.api.unrequestReviewers (owner repo) (shortName repo) n (reviewersBody [who]))
    , readiness = \repo n -> do
        st <- stateOfPull repo n
        behind <- (.behindBy) <$> run rest (Api.api.compare (owner repo) (shortName repo) (Api.BaseHead st.baseBranch (headRef repo st)))
        pure Rd.Readiness
          { draft = st.draft, behind = behind, conflicts = st.mergeable == "CONFLICTING"
          , requiredApprovals = st.requiredApprovals, scheduled = st.autoMerge }
    , markReady = \repo n -> pullMutate repo n "markPullRequestReadyForReview" ""
    , updateBranch = \repo n style -> case style of
        ByMerge -> () <$ run rest (Api.api.updateBranch (owner repo) (shortName repo) n (object []))
        ByRebase -> pullMutate repo n "updatePullRequestBranch" ", updateMethod: REBASE"
    , scheduleMerge = \repo n how _ -> pullMutate repo n "enablePullRequestAutoMerge" (", mergeMethod: " <> mergeName how)
    , cancelScheduledMerge = \repo n -> pullMutate repo n "disablePullRequestAutoMerge" ""
    , reviewCandidates = \repo _ -> do
        let (o, r) = (owner repo, shortName repo)
        people <- pages (Api.api.assignees o r)
        -- Only a repository in an organization has teams.
        teams <- either (\(_ :: Failure) -> []) id <$> try (run rest (Api.api.teams o r))
        pure $ [Rv.Candidate {who = Rv.Person p.login, about = "", changed = [], lastChanged = Nothing, members = Nothing} | p <- people]
          <> [Rv.Candidate {who = Rv.Team t.slug, about = t.name, changed = [], lastChanged = Nothing, members = Nothing} | t <- teams]
    , reply = \repo n t text -> case commentIdIn t.url of
        Just i -> () <$ run rest (Api.api.replyToComment (owner repo) (shortName repo) n i (Api.Body text))
        Nothing -> throwIO (Refused "usta could not tell which comment this thread starts with.")
    , recentCommits = \repo branch -> commitsOn repo (Just branch) Nothing 1 10
    , readme = \repo branch -> do
        found <- try (run rest (Api.api.readme (owner repo) (shortName repo) (Just branch)))
        case found of
          -- No README is a 404.
          Left (Refused _) -> pure Nothing
          Left e -> throwIO e
          Right r -> do
            bytes <- run rest (Api.api.file (owner repo) (shortName repo) (T.splitOn "/" r.path) (Just branch))
            pure (Just (r.name, decodeUtf8Lenient bytes))
    , commitDiff = \repo sha -> run rest (Api.api.commitDiff (owner repo) (shortName repo) (unparse sha)) >>= diffOf
    , discussion = \repo n -> discussionOf graph repo n
    , comment = \repo n text -> do
        c <- run rest (Api.api.postComment (owner repo) (shortName repo) n (Api.Body text))
        restComment c
    , editComment = \repo n which text -> case which of
        Just i -> () <$ run rest (Api.api.editComment (owner repo) (shortName repo) i (Api.Body text))
        Nothing -> () <$ run rest (Api.api.editIssue (owner repo) (shortName repo) n (object ["body" .= text]))
    , deleteComment = \repo i -> () <$ run rest (Api.api.deleteComment (owner repo) (shortName repo) i)
    , editTitle = \repo n title -> () <$ run rest (Api.api.editIssue (owner repo) (shortName repo) n (object ["title" .= title]))
    , watching = \repo n -> do
        d <- discussionNode graph repo n Nothing
        pure $ case d.subscription of
          Just "SUBSCRIBED" -> D.Subscribed
          Just "IGNORED" -> D.Muted
          _ -> D.Participating
    , setWatching = \repo n wanted -> do
        d <- discussionNode graph repo n Nothing
        let state = case wanted of
              D.Subscribed -> "SUBSCRIBED" :: Text
              D.Muted -> "IGNORED"
              D.Participating -> "UNSUBSCRIBED"
        () <$ asked graph (Api.graph.mutate Api.GraphQuery {query = Api.subscriptionMutation, variables = object ["id" .= d.nodeId, "state" .= state]})
    , timeline = \repo n -> do
        Api.Item nodes <- asked graph (Api.graph.timeline Api.GraphQuery {query = Api.timelineQuery, variables = itemVariables repo n Nothing})
        eventsOf nodes
    , repoPullRequests = \repo listing from -> do
        found <- pageOf graph Api.graph.searchPulls Api.pullFields (listed "is:pr" repo listing) from
        (\ps -> found {Lt.items = ps}) <$> traverse (\p -> pullOf host (p, [])) found.items
    , repoIssues = \repo listing from -> do
        found <- pageOf graph Api.graph.searchIssues Api.issueFields (listed "is:issue" repo listing) from
        (\is -> found {Lt.items = is}) <$> traverse (\i -> issueOf host (i, [])) found.items
    , repoCounts = \repo pulls words' -> do
        let kind = if pulls then "is:pr" else "is:issue"
            q w = listed kind repo Lt.Listing {which = w, order = Lt.RecentlyUpdated, words = words'}
        c <- asked graph (Api.graph.counts Api.GraphQuery
          { query = Api.countsQuery
          , variables = object ["opened" .= q Lt.Opened, "drafts" .= q Lt.Drafts, "merged" .= q Lt.Merged, "shut" .= q Lt.Shut, "every" .= q Lt.Every] })
        pure $ if pulls
          then Lt.Counts {opened = Just c.opened, drafts = Just c.drafts, merged = Just c.merged, shut = Just c.shut, every = Just c.every}
          else Lt.Counts {opened = Just c.opened, drafts = Nothing, merged = Nothing, shut = Just c.shut, every = Just c.every}
    , runs = \repo -> do
        let (o, r) = (owner repo, shortName repo)
        Api.WorkflowRuns latest <- run rest (Api.api.runs o r Api.ListQuery {page = Just 1, perPage = Named (Just 20)})
        for latest $ \w -> do
          Api.Jobs jobs <- run rest (Api.api.runJobs o r w.id)
          pure C.Run
            { id = w.id
            , number = w.number, workflow = w.workflow, title = w.title, branch = w.branch, event = w.event
            , sha = w.sha, state = stateOf w.status w.conclusion, started = w.started, updated = w.updated
            , url = w.url, jobs = map jobCheck jobs }
    , react = \repo n which feeling adding -> do
        let (o, r) = (owner repo, shortName repo)
            body = Api.ReactionBody feeling
        login <- myLogin
        case (which, adding) of
          (Just i, True) -> runDone rest (Api.api.reactToComment o r i body)
          (Nothing, True) -> runDone rest (Api.api.reactToIssue o r n body)
          (Just i, False) -> do
            given <- run rest (Api.api.commentReactions o r i (Just (Api.feelingName feeling)) (Just 100))
            mapM_ (\x -> run rest (Api.api.unreactToComment o r i x.id)) [x | x <- given, x.user == Just login]
          (Nothing, False) -> do
            given <- run rest (Api.api.issueReactions o r n (Just (Api.feelingName feeling)) (Just 100))
            mapM_ (\x -> run rest (Api.api.unreactToIssue o r n x.id)) [x | x <- given, x.user == Just login]
    , setClosed = \repo n closed ->
        () <$ run rest (Api.api.editIssue (owner repo) (shortName repo) n (object ["state" .= (if closed then "closed" else "open" :: Text)]))
    , merge = \repo n how delete -> do
        let (o, r) = (owner repo, shortName repo)
        st <- stateOfPull repo n
        () <$ run rest (Api.api.merge o r n (object ["merge_method" .= T.toLower (mergeName how)]))
        -- A branch in a fork is the fork owner's to delete.
        when (delete && st.headRepo == Just repo) $
          () <$ run rest (Api.api.deleteBranch o r (T.splitOn "/" st.headBranch))
    , review = \repo n verdict text notes ->
        () <$ run rest (Api.api.review (owner repo) (shortName repo) n (object
          [ "event" .= (case verdict of Approve -> "APPROVE"; RequestChanges -> "REQUEST_CHANGES"; CommentOnly -> "COMMENT" :: Text)
          , "body" .= text
          , "comments" .= [object ["path" .= x.path, "line" .= x.line, "side" .= ("RIGHT" :: Text), "body" .= x.body] | x <- notes] ]))
    , discardPending = \repo n -> do
        st <- stateOfPull repo n
        let pending = [i | r <- st.reviews, r.state == "PENDING", r.author == Just st.viewer, Just i <- [r.id]]
        mapM_ (\i -> run rest (Api.api.deleteReview (owner repo) (shortName repo) n i)) pending
        pure (length pending)
    , createIssue = \repo new -> do
        created <- run rest (Api.api.createIssue (owner repo) (shortName repo) (object
          ["title" .= new.title, "body" .= new.body, "labels" .= new.labels, "assignees" .= map unparse new.assignees]))
        pure created.number
    , createPull = \repo new -> do
        let (o, r) = (owner repo, shortName repo)
        created <- run rest (Api.api.createPull o r (object
          ["title" .= new.title, "body" .= new.body, "head" .= new.from, "base" .= new.into, "draft" .= new.draft]))
        unless (null new.labels) $ () <$ run rest (Api.api.setLabels o r created.number (Api.LabelNames new.labels))
        unless (null new.reviewers) $
          () <$ run rest (Api.api.requestReviewers o r created.number (object ["reviewers" .= map unparse new.reviewers]))
        pure created.number
    , branches = \repo -> map (.name) <$> refsOf graph repo "refs/heads/"
    , branchList = \repo -> map (\r -> Br.Branch {name = r.name, pushed = r.at, ahead = Nothing, behind = Nothing}) <$> refsOf graph repo "refs/heads/"
    , tags = \repo -> map (\r -> Br.Tag {name = r.name, at = r.at}) <$> refsOf graph repo "refs/tags/"
    , upload = \_ _ _ _ -> throwIO (Refused "GitHub has no API for attachments: add the file to the comment on github.com.")
    , search = \words' -> do
        let ask kind = asked graph (Api.graph.found Api.GraphQuery {query = Api.foundQuery, variables = object ["q" .= words', "type" .= (kind :: Text)]})
        issuesFound <- ask "ISSUE"
        reposFound <- ask "REPOSITORY"
        pure [ S.Found {kind = kindOf f.kind, repo = f.repo, number = f.number, title = f.title, closed = f.closed, updated = f.updated}
             | f <- issuesFound.nodes <> reposFound.nodes ]
    , starred = \repo -> do
        found <- try (run rest (Api.api.starred (owner repo) (shortName repo)))
        case found of
          Right _ -> pure True
          -- 404 is GitHub's "not starred".
          Left (Refused _) -> pure False
          Left e -> throwIO e
    , setStarred = \repo on ->
        () <$ run rest ((if on then Api.api.star else Api.api.unstar) (owner repo) (shortName repo))
    , releases = \repo -> do
        rs <- run rest (Api.api.releases (owner repo) (shortName repo) Api.ListQuery {page = Just 1, perPage = Named (Just 30)})
        pure (map releaseOf rs)
    , fileHistory = \repo branch path -> commitsOn repo (Just branch) (Just path) 1 50
    , history = \repo branch n -> commitsOn repo (Just branch) Nothing n 50
    , labels = \repo -> map (\l -> L.Definition {name = l.name, color = l.color, description = l.description})
        <$> pages (\q -> Api.api.labels (owner repo) (shortName repo) q)
    , setLabels = \repo n names -> () <$ run rest (Api.api.setLabels (owner repo) (shortName repo) n (Api.LabelNames names))
    , createLabel = \repo d ->
        () <$ run rest (Api.api.createLabel (owner repo) (shortName repo) Api.RestLabel {name = d.name, color = d.color, description = d.description})
    , assignable = \repo -> do
        people <- pages (\q -> Api.api.assignees (owner repo) (shortName repo) q)
        pure [Rv.Candidate {who = Rv.Person p.login, about = "", changed = [], lastChanged = Nothing, members = Nothing} | p <- people]
    , setAssignees = \repo n logins -> () <$ run rest (Api.api.editIssue (owner repo) (shortName repo) n (object ["assignees" .= map unparse logins]))
    , milestones = \repo -> map milestoneOf <$> pages (\q -> Api.api.milestones (owner repo) (shortName repo) (Just "all") q)
    , setMilestone = \repo n title -> do
        known <- pages (\q -> Api.api.milestones (owner repo) (shortName repo) (Just "all") q)
        target <- case title of
          Nothing -> pure Null
          Just t -> case [m.number | m <- known, m.title == t] of
            found : _ -> pure (toJSON found)
            [] -> throwIO (Refused ("The repository has no milestone " <> t <> "."))
        () <$ run rest (Api.api.editIssue (owner repo) (shortName repo) n (object ["milestone" .= target]))
    , compare = \repo into tip -> do
        c <- run rest (Api.api.compare (owner repo) (shortName repo) (Api.BaseHead into tip))
        (\cs -> (cs, c.files)) <$> traverse commitOf c.commits
    , compareDiff = \repo into tip -> run rest (Api.api.compareDiff (owner repo) (shortName repo) (Api.BaseHead into tip)) >>= diffOf
    , tree = \repo ref -> do
        t <- run rest (Api.api.tree (owner repo) (shortName repo) ref (Just 1))
        pure [Tr.Entry {path = e.path, kind = if e.folder then Tr.Folder else Tr.File, size = e.size} | e <- t.entries]
    , file = \repo ref path -> run rest (Api.api.file (owner repo) (shortName repo) (T.splitOn "/" path) (Just ref))
    }

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
      found <- asked client (route Api.GraphQuery
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
      Api.Repos found <- asked client (Api.graph.repositories Api.GraphQuery
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

itemVariables :: RepoName -> Number -> Maybe Text -> Value
itemVariables repo n after =
  object ["owner" .= owner repo, "name" .= shortName repo, "number" .= unparse n, "after" .= maybe Null String after]

-- | The first page of the conversation, or the one after @after@.
discussionNode :: Client -> RepoName -> Number -> Maybe Text -> IO Api.DiscussionNode
discussionNode graph repo n after = do
  asked graph (Api.graph.discussion Api.GraphQuery {query = Api.discussionQuery, variables = itemVariables repo n after})

-- | The description and every comment, a page of 100 at a time.
discussionOf :: Client -> RepoName -> Number -> IO D.Discussion
discussionOf graph repo n = do
  first <- discussionNode graph repo n Nothing
  let more after = do
        d <- discussionNode graph repo n (Just after)
        maybe (pure d.comments.nodes) (fmap (d.comments.nodes <>) . more) d.comments.next
  rest' <- maybe (pure []) more first.comments.next
  opening <- commentOf first.opening
  said <- traverse commentOf (first.comments.nodes <> rest')
  -- A pull request's reviews with text of their own read as comments.
  reviewed <- traverse (\r -> (\c -> c {D.review = verdictOf r.state}) <$> commentOf r.comment) [r | r <- first.reviews, not (T.null (T.strip r.comment.body))]
  let comments = sortOn (.createdAt) (said <> reviewed)
  pure D.Discussion
    { opening = opening
    , comments = comments
    , closed = first.closed
    , merged = first.merged
    , assignees = first.assignees
    , labels = [L.Label {name = l.name, color = l.color} | l <- first.labels]
    , milestone = first.milestone
    }

commentOf :: Api.CommentNode -> IO D.Comment
commentOf c = do
  author <- maybe (parseM "ghost") pure c.author
  pure D.Comment
    { author = author
    , body = c.body
    , createdAt = c.createdAt
    , id = c.databaseId
    , edited = c.edited
    , url = c.url
    , reactions = [D.Reaction {feeling = f, count = g.count, mine = g.mine} | g <- c.reactions, g.count > 0, Just f <- [Api.feelingOf g.content]]
    , review = Nothing
    }

restComment :: Api.RestComment -> IO D.Comment
restComment c = do
  author <- maybe (parseM "ghost") pure c.author
  pure D.Comment {author = author, body = c.body, createdAt = c.createdAt, id = Just c.id, edited = c.updatedAt > c.createdAt, url = c.url, reactions = [], review = Nothing}

milestoneOf :: Api.RestMilestone -> M.Milestone
milestoneOf m = M.Milestone {title = m.title, open = m.open, due = m.due, closedAt = m.closedAt, openItems = m.openItems, closedItems = m.closedItems}

-- | The timeline in usta's terms. GitHub lists each commit of a push on
-- its own; the ones in a row are one push here.
eventsOf :: [Api.TimelineNode] -> IO [Tl.Event]
eventsOf nodes = do
  ghost <- parseM "ghost"
  let actorOf e = fromMaybe ghost e.actor
      one e = case e.kind of
        "LabeledEvent" -> labeled True
        "UnlabeledEvent" -> labeled False
        "AssignedEvent" -> Tl.Assigned True <$> e.assignee
        "UnassignedEvent" -> Tl.Assigned False <$> e.assignee
        "ReviewRequestedEvent" -> Tl.ReviewRequested True <$> e.reviewer
        "ReviewRequestRemovedEvent" -> Tl.ReviewRequested False <$> e.reviewer
        "MilestonedEvent" -> Just (Tl.Milestoned Nothing e.milestone)
        "DemilestonedEvent" -> Just (Tl.Milestoned e.milestone Nothing)
        "RenamedTitleEvent" -> Tl.Renamed <$> e.previous <*> e.current
        "ClosedEvent" -> Just Tl.Closed
        "ReopenedEvent" -> Just Tl.Reopened
        "MergedEvent" -> Just Tl.Merged
        "CrossReferencedEvent" -> uncurry Tl.Referenced <$> e.source
        "AutoMergeEnabledEvent" -> Just (Tl.MergeScheduled True)
        "AutoMergeDisabledEvent" -> Just (Tl.MergeScheduled False)
        "HeadRefDeletedEvent" -> Just Tl.BranchDeleted
        -- One with text is a comment in the conversation.
        "PullRequestReview" | maybe True (T.null . T.strip) e.reviewBody -> e.reviewState >>= verdictOf >>= \v -> if v == CommentOnly then Nothing else Just (Tl.Reviewed v)
        _ -> Nothing
        where
          labeled on = (\l -> Tl.Labeled on L.Label {name = l.name, color = l.color}) <$> e.label
      go xs = case xs of
        [] -> []
        e : more | e.kind == "PullRequestCommit" ->
          let (pushed, after) = span ((== "PullRequestCommit") . (.kind)) (e : more)
              final = last pushed
           in [Tl.Event {actor = fromMaybe ghost final.commitAuthor, at = t, what = Tl.Pushed (length pushed)} | Just t <- [final.committedAt]] <> go after
        e : more -> [Tl.Event {actor = actorOf e, at = t, what = w} | Just t <- [e.at], Just w <- [one e]] <> go more
  pure (go nodes)

-- | A GraphQL call; what GitHub said went wrong is the 'Failure'.
asked :: Client -> ClientM (Api.Answer a) -> IO a
asked graph call = run graph call >>= \case
  Api.Answer a -> pure a
  Api.Problem message -> throwIO (Refused ("GitHub said: " <> message))

-- | One page of a search, with GitHub's count of all it finds.
pageOf :: Client -> (Api.GraphQuery -> ClientM (Api.Answer (Api.Page a))) -> Text -> Text -> Maybe Text -> IO (Lt.Paged a)
pageOf client route fields q from = do
  found <- asked client (route Api.GraphQuery
    {query = Api.searchQuery fields, variables = object ["q" .= q, "after" .= maybe Null String from]})
  pure Lt.Paged {items = found.nodes, total = Just found.total, next = found.next}

-- | A repository's tab as a search: "is:pr repo:o/r is:merged sort:…".
listed :: Text -> RepoName -> Lt.Listing -> Text
listed kind repo listing = T.unwords (filter (not . T.null) [kind, "repo:" <> unparse repo, which, order, T.strip listing.words])
  where
    pull = kind == "is:pr"
    which = case listing.which of
      Lt.Opened -> if pull then "is:open draft:false" else "is:open"
      Lt.Drafts -> "is:open draft:true"
      Lt.Merged -> "is:merged"
      Lt.Shut -> if pull then "is:closed is:unmerged" else "is:closed"
      Lt.Every -> ""
    order = case listing.order of
      Lt.RecentlyUpdated -> "sort:updated-desc"
      Lt.Newest -> "sort:created-desc"
      Lt.Oldest -> "sort:created-asc"

diffOf :: Text -> IO [FileDiff]
diffOf text = either (throwIO . Refused) pure (parseDiff text)

commitOf :: Api.RestCommit -> IO Cm.Commit
commitOf c = do
  sha <- parseM c.sha
  let (subject, rest') = T.breakOn "\n" c.message
  pure Cm.Commit {sha = sha, author = c.author, authoredAt = c.authoredAt, subject = subject, body = T.strip rest'}

-- | Every branch or tag, at most 1000.
refsOf :: Client -> RepoName -> Text -> IO [Api.RefNode]
refsOf graph repo prefix = go (1 :: Int) Nothing
  where
    go n after = do
      Api.RefPage found <- asked graph (Api.graph.refs Api.GraphQuery
        { query = Api.refsQuery
        , variables = object ["owner" .= owner repo, "name" .= shortName repo, "prefix" .= prefix, "after" .= maybe Null String after] })
      case found.next of
        Just cursor | n < 10 -> (found.nodes <>) <$> go (n + 1) (Just cursor)
        _ -> pure found.nodes

releaseOf :: Api.RestRelease -> Rel.Release
releaseOf r = Rel.Release
  { name = fromMaybe r.tag (r.name >>= \n -> if T.null n then Nothing else Just n)
  , tag = r.tag
  , notes = fromMaybe "" r.notes
  , author = maybe "" unparse r.author
  , published = r.published
  , draft = r.draft
  , prerelease = r.prerelease
  , url = r.url
  , assets = [Rel.Asset {name = a.name, size = a.size, downloads = a.downloads, url = a.url} | a <- r.assets]
  }

-- | A run's or job's state from GitHub's status and conclusion.
stateOf :: Text -> Maybe Text -> C.State
stateOf status conclusion = case (status, conclusion) of
  ("completed", Just c)
    | c == "success" -> C.Passed
    | c `elem` ["cancelled", "skipped", "neutral", "stale"] -> C.Skipped
    | otherwise -> C.Failed
  ("in_progress", _) -> C.Running
  _ -> C.Waiting

-- | A job as a check; its log is asked by the job's id.
jobCheck :: Api.RestJob -> C.Check
jobCheck j = C.Check
  { name = j.name
  , state = stateOf j.status j.conclusion
  , description = case (j.startedAt, j.completedAt) of
      (Just a, Just b) -> "Took " <> took (diffUTCTime b a)
      _ -> if j.status == "in_progress" then "Running" else "Waiting"
  , url = Just j.url
  , job = Just C.Job {run = fromIntegral j.id, index = 0}
  }

took :: NominalDiffTime -> Text
took d = let n = round d :: Int in
  if n < 60 then T.pack (show n) <> "s" else T.pack (show (n `div` 60)) <> "m " <> T.pack (show (n `mod` 60)) <> "s"

checkOf :: Api.CheckNode -> C.Check
checkOf c = C.Check
  { name = c.name
  , state = case c.kind of
      "CheckRun" -> stateOf (maybe "" T.toLower c.status) (T.toLower <$> c.conclusion)
      _ -> case c.state of
        Just "SUCCESS" -> C.Passed
        Just "FAILURE" -> C.Failed
        Just "ERROR" -> C.Failed
        Just "PENDING" -> C.Running
        _ -> C.Waiting
  , description = fromMaybe "" c.description
  , url = c.url
  -- A check run of GitHub Actions has the job's id; others link out.
  , job = if c.kind == "CheckRun" && maybe False ("/actions/runs/" `T.isInfixOf`) c.url
      then (\i -> C.Job {run = fromIntegral i, index = 0}) <$> c.jobId
      else Nothing
  }

kindOf :: Text -> S.Kind
kindOf k = case k of
  "PullRequest" -> S.PullRequest
  "Repository" -> S.Repository
  _ -> S.Issue

reviewersBody :: [Rv.Who] -> Value
reviewersBody whos = object
  [ "reviewers" .= [unparse l | Rv.Person l <- whos]
  , "team_reviewers" .= [t | Rv.Team t <- whos] ]

-- | The branch to compare with: in a fork, as "owner:branch".
headRef :: RepoName -> Api.PullState -> Text
headRef repo st = case st.headRepo of
  Just other | other /= repo -> owner other <> ":" <> st.headBranch
  _ -> st.headBranch

mergeName :: Merge -> Text
mergeName how = case how of
  Squash -> "SQUASH"
  MergeCommit -> "MERGE"
  Rebase -> "REBASE"

stanceOf :: Text -> Maybe Rv.Stance
stanceOf state = lookup state [("APPROVED", Rv.Approved), ("CHANGES_REQUESTED", Rv.ChangesRequested), ("COMMENTED", Rv.Commented)]

-- | A thread with its notes; one GitHub cannot place on a line is left out.
threadOf :: Api.ThreadNode -> IO (Maybe Th.Thread)
threadOf t = case (t.notes, t.line <|> t.originalLine) of
  (first : _, Just line) -> do
    notes <- traverse (\c -> (\a -> Th.Note {author = a, body = c.body, createdAt = c.createdAt}) <$> maybe (parseM "ghost") pure c.author) t.notes
    pure $ Just Th.Thread
      { path = t.path
      , line = line
      -- The hunk ends at the commented line; a few lines show the code.
      , code = reverse (take 4 (reverse (drop 1 (T.lines first.hunk))))
      , commit = maybe "" (T.take 7) first.commit
      , resolvedBy = t.resolvedBy
      , notes = notes
      , url = first.url
      }
  _ -> pure Nothing

-- | The comment a thread's link points at: "…#discussion_r12345".
commentIdIn :: Text -> Maybe Int64
commentIdIn url = readMaybe (T.unpack (T.takeWhile isDigit (snd (T.breakOnEnd "discussion_r" url))))

verdictOf :: Text -> Maybe Verdict
verdictOf state = lookup state [("APPROVED", Approve), ("CHANGES_REQUESTED", RequestChanges), ("COMMENTED", CommentOnly)]
