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
  , siteOf
  ) where

import Control.Exception (throwIO, try)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (nub, partition, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import qualified Network.HTTP.Client as HTTP
import Servant.API (ResponseHeader (..), getResponse, lookupResponseHeader)
import Servant.Client (BaseUrl (..), ClientM)
import Data.Traversable (for)
import Text.Read (readMaybe)

import Usta.Forge (CloneUrls (..), Failure (..), Fetched (..), Forge (..), Known (..), LineNote (..))
import qualified Usta.Forge.Forgejo.Api as Api
import Usta.Forge.Http (Client, connect, run)
import qualified Data.ByteString.Lazy as BL
import Servant.Multipart.API (FileData (..), MultipartData (..))
import Servant.Multipart.Client (genBoundary)
import qualified Usta.Types.Branch as Br
import qualified Usta.Types.Release as Rel
import qualified Usta.Types.Search as S
import qualified Usta.Types.Check as C
import Usta.Types.Diff (FileDiff (..), parseDiff)
import qualified Usta.Types.Discussion as D
import Usta.Types.Discussion (Discussion (..), NewIssue (..), NewPull (..), Verdict (..))
import Usta.Types.Host (Host)
import Usta.Types.Involvement (Involvement (..))
import qualified Usta.Types.Issue as I
import qualified Usta.Types.Label as L
import qualified Usta.Types.Listing as Lt
import Usta.Types.Milestone (Milestone (..))
import Usta.Types.Login (Login)
import qualified Usta.Types.Notification as N
import Usta.Types.Number (Number)
import Usta.Types.Parse (parseM, unparse)
import qualified Usta.Types.PullRequest as P
import Usta.Types.Ref (Ref (..))
import qualified Usta.Types.Thread as Th
import Usta.Types.RepoName (RepoName, owner, shortName)
import qualified Usta.Types.Readiness as Rd
import qualified Usta.Types.Reviewer as Rv
import qualified Usta.Types.Repository as R
import Usta.Types.Servant.NamedQueryParam (Named (..))
import Usta.Types.Token (Token)

-- | The forge of the account @token@ signs in as, on @host@, whose API is at
-- @base@ (normally @https://host/api/v1@); @token@ is read for each call.
forgejo :: HTTP.Manager -> BaseUrl -> Host -> IO Token -> IO Forge
forgejo manager base host token = do
  let client = connect manager base (authorization <$> token)
      -- The site's own pages, for what the API lacks.
      site = connect manager base {baseUrlPath = siteOf base.baseUrlPath} (authorization <$> token)
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
        let (o, r) = (owner repo, shortName repo)
        d <- run client (Api.api.issue o r n)
        cs <- run client (Api.api.issueComments o r n)
        login <- myLogin
        -- One more call for each comment: Forgejo lists reactions apart.
        opened <- run client (everyPage (\page limit -> concat <$> Api.api.issueReactions o r n Api.ListQuery {page = page, limit = limit}))
        said <- traverse (\c -> case c.comment.id of
          Just i -> (\rs -> c.comment {D.reactions = tally login (concat rs)}) <$> run client (Api.api.commentReactions o r i)
          Nothing -> pure c.comment) cs
        pure Discussion
          { opening = d.opening {D.reactions = tally login opened}
          , comments = said
          , closed = d.closed
          , merged = d.merged
          , assignees = d.assignees
          , labels = d.labels
          , milestone = d.milestone
          }
    , comment = \repo n text ->
        (.comment) <$> run client (Api.api.postComment (owner repo) (shortName repo) n (Api.NewComment text))
    , editComment = \repo n which text -> case which of
        Just i -> () <$ run client (Api.api.editComment (owner repo) (shortName repo) i Api.BodyEdit {body = text})
        Nothing -> () <$ run client (Api.api.editBody (owner repo) (shortName repo) n Api.BodyEdit {body = text})
    , deleteComment = \repo i -> () <$ run client (Api.api.deleteComment (owner repo) (shortName repo) i)
    , repoPullRequests = \repo listing from -> do
        (latest, total, next) <- repoPage client repo Api.Pulls listing from
        found <- traverse (\i -> pullRequestOf client details host (i, [])) latest
        -- Forgejo lists open and drafts together, and merged and closed:
        -- each page keeps the ones asked for, and only "all" has a total.
        let keep pr = case listing.which of
              Lt.Opened -> pr.state == P.Open
              Lt.Drafts -> pr.state == P.Draft
              Lt.Merged -> pr.state == P.Merged
              Lt.Shut -> pr.state == P.Closed
              Lt.Every -> True
        pure Lt.Paged {items = filter keep found, total = if listing.which == Lt.Every then total else Nothing, next = next}
    , repoIssues = \repo listing from -> do
        (latest, total, next) <- repoPage client repo Api.Issues listing from
        pure Lt.Paged {items = map (issueOf host) [(i, []) | i <- latest], total = total, next = next}
    -- Forgejo counts open (drafts too) and closed (merged too): pull
    -- requests get only their total.
    , repoCounts = \repo pulls words' -> if pulls
        then (\t -> Lt.Counts Nothing Nothing Nothing Nothing t) <$> countOf client repo Api.Pulls Api.AnyState words'
        else (\o c t -> Lt.Counts o Nothing Nothing c t)
          <$> countOf client repo Api.Issues Api.Open words'
          <*> countOf client repo Api.Issues Api.Closed words'
          <*> countOf client repo Api.Issues Api.AnyState words'
    , discardPending = \repo n -> do
        login <- myLogin
        given <- run client (everyPage (\page limit -> Api.api.reviews (owner repo) (shortName repo) n Api.ListQuery {page = page, limit = limit}))
        let mine = [r.id | r <- given, r.state == "PENDING", Just u <- [r.user], u.login == login]
        mapM_ (\i -> run client (Api.api.deleteReview (owner repo) (shortName repo) n i)) mine
        pure (length mine)
    , runs = \repo -> do
        Api.Tasks tasks <- run client (Api.api.tasks (owner repo) (shortName repo) Api.ListQuery {page = Just 1, limit = Just 50})
        pure (runsOf tasks)
    , timeline = \repo n -> mapMaybe (.event) <$> run client
        (everyPage (\page limit -> Api.api.timeline (owner repo) (shortName repo) n Api.ListQuery {page = page, limit = limit}))
    , watching = \repo n -> (.watching) <$> run client (Api.api.subscription (owner repo) (shortName repo) n)
    , setWatching = \repo n wanted -> do
        let (o, r) = (owner repo, shortName repo)
        now <- (.watching) <$> run client (Api.api.subscription o r n)
        login <- myLogin
        -- Forgejo keeps a subscription or an ignore, and has no way back to
        -- neither.
        unless (now == wanted) $ case wanted of
          D.Subscribed -> () <$ run client (Api.api.subscribe o r n login)
          D.Muted -> () <$ run client (Api.api.unsubscribe o r n login)
          D.Participating -> throwIO (Refused "Forgejo cannot go back to only when taking part; choose Subscribed or Muted.")
    , editTitle = \repo n title -> () <$ run client (Api.api.editTitle (owner repo) (shortName repo) n Api.PullEdit {title = title})
    , react = \repo n which feeling adding -> do
        let (o, r) = (owner repo, shortName repo)
            body = Api.ReactionBody feeling
        case (which, adding) of
          (Just i, True) -> () <$ run client (Api.api.reactToComment o r i body)
          (Just i, False) -> () <$ run client (Api.api.unreactToComment o r i body)
          (Nothing, True) -> () <$ run client (Api.api.reactToIssue o r n body)
          (Nothing, False) -> () <$ run client (Api.api.unreactToIssue o r n body)
    , setClosed = \repo n closed ->
        () <$ run client (Api.api.editIssue (owner repo) (shortName repo) n (Api.IssueEdit closed))
    , merge = \repo n how delete ->
        () <$ run client (Api.api.merge (owner repo) (shortName repo) n Api.MergeOption {how = how, deleteBranch = delete, whenChecksPass = False})
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
          () <$ run client (Api.api.requestReviewers (owner repo) (shortName repo) created.number Api.Reviewers {reviewers = new.reviewers, teams = []})
        pure created.number
    , branches = \repo -> map (.name) <$> run client
        (everyPage (\page limit -> Api.api.branches (owner repo) (shortName repo) Api.ListQuery {page = page, limit = limit}))
    , labels = \repo -> map (\l -> L.Definition {name = l.name, color = l.color, description = l.description}) <$> repoLabels client repo
    , setLabels = \repo n names -> do
        ids <- labelIds client repo names
        () <$ run client (Api.api.setIssueLabels (owner repo) (shortName repo) n Api.LabelIds {labels = ids})
    , assignable = \repo -> do
        people <- run client (Api.api.assignees (owner repo) (shortName repo))
        pure [Rv.Candidate {who = Rv.Person p.login, about = p.fullName, changed = [], lastChanged = Nothing, members = Nothing} | p <- people]
    , setAssignees = \repo n logins ->
        () <$ run client (Api.api.setAssignees (owner repo) (shortName repo) n Api.AssigneesEdit {assignees = logins})
    , milestones = \repo -> map (.milestone) . sortOn dueOrder <$> milestonesOf client repo
    , setMilestone = \repo n title -> do
        known <- milestonesOf client repo
        target <- case title of
          Nothing -> pure 0
          Just t -> case [m.id | m <- known, m.milestone.title == t] of
            found : _ -> pure found
            [] -> throwIO (Refused ("The repository has no milestone " <> t <> "."))
        () <$ run client (Api.api.setMilestone (owner repo) (shortName repo) n Api.MilestoneEdit {milestone = target})
    , createLabel = \repo d ->
        () <$ run client (Api.api.createLabel (owner repo) (shortName repo) Api.NewLabel {name = d.name, color = d.color, description = d.description})
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
    , compareDiff = \repo into tip -> do
        text <- run site (Api.web.compareDiff (owner repo) (shortName repo) (Api.BaseHeadDiff into tip))
        either (throwIO . Refused) pure (parseDiff text)
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
    , checks = \repo n -> do
        pull <- run client (Api.api.pullRequest (owner repo) (shortName repo) n)
        combined <- run client (Api.api.status (owner repo) (shortName repo) pull.head.sha)
        pure (map (checkOf host) combined.statuses)
    -- The first attempt's: the status links the job, not its attempts.
    , jobSteps = \_ _ -> throwIO (Refused "Forgejo does not list a job's steps.")
    -- Forgejo has no list of workflows: they are the files of the first
    -- of its workflow folders the default branch has, named as its runs
    -- name them, by file.
    , workflows = \repo -> do
        let inFolder dir = either (\(_ :: Failure) -> []) (\es -> [e.name | e <- es, e.kind == "file", any (`T.isSuffixOf` e.name) [".yml", ".yaml"]])
              <$> try (run client (Api.api.folder (owner repo) (shortName repo) [dir, "workflows"]))
        found <- traverse inFolder [".forgejo", ".gitea", ".github"]
        pure (take 1 [names | names@(_ : _) <- found] >>= id)
    , checkLog = \repo job -> run site (Api.web.jobLog (owner repo) (shortName repo) job.run job.index 1)
    , reviewers = \repo n -> do
        pull <- run client (Api.api.pullRequest (owner repo) (shortName repo) n)
        given <- run client (everyPage (\page limit -> Api.api.reviews (owner repo) (shortName repo) n Api.ListQuery {page = page, limit = limit}))
        pure (reviewersOf pull given)
    , requestReview = \repo n whos ->
        () <$ run client (Api.api.requestReviewers (owner repo) (shortName repo) n (requestOf whos))
    , unrequestReview = \repo n who ->
        () <$ run client (Api.api.unrequestReviewers (owner repo) (shortName repo) n (requestOf [who]))
    , readiness = \repo n -> do
        let (o, r) = (owner repo, shortName repo)
        pull <- run client (Api.api.pullRequest o r n)
        -- The base's commits the head lacks.
        missing <- run client (Api.api.compare o r (Api.BaseHead pull.head.sha pull.base.sha))
        -- Only admins may read protections; without them none are asked.
        rules <- either (\(_ :: Failure) -> []) id <$> try (run client (Api.api.branchProtections o r))
        -- A scheduled merge shows only in the timeline; the latest word holds.
        events <- run client (everyPage (\page limit -> Api.api.timeline o r n Api.ListQuery {page = page, limit = limit}))
        let schedule = [e.kind | e <- events, e.kind `elem` ["pull_scheduled_merge", "pull_cancel_scheduled_merge"]]
        pure Rd.Readiness
          { draft = pull.draft
          , behind = length missing.commits
          -- Forgejo calls every draft unmergeable, conflicts or not.
          , conflicts = not pull.mergeable && not pull.draft
          , requiredApprovals = maximum (0 : [p.requiredApprovals | p <- rules, matches p.rule pull.base.ref])
          , scheduled = not pull.merged && take 1 (reverse schedule) == ["pull_scheduled_merge"]
          }
    , markReady = \repo n -> do
        pull <- run client (Api.api.pullRequest (owner repo) (shortName repo) n)
        () <$ run client (Api.api.editPull (owner repo) (shortName repo) n Api.PullEdit {title = unDraft pull.title})
    , updateBranch = \repo n style -> () <$ run client (Api.api.updatePull (owner repo) (shortName repo) n (Api.Style style))
    , scheduleMerge = \repo n how delete ->
        () <$ run client (Api.api.scheduleMerge (owner repo) (shortName repo) n Api.MergeOption {how = how, deleteBranch = delete, whenChecksPass = True})
    , cancelScheduledMerge = \repo n -> () <$ run client (Api.api.cancelAutoMerge (owner repo) (shortName repo) n)
    , reviewCandidates = \repo pr -> do
        let (o, r) = (owner repo, shortName repo)
        people <- run client (everyPage (\page limit -> Api.api.collaborators o r Api.ListQuery {page = page, limit = limit}))
        -- A repository a person owns has no teams; Forgejo refuses to list them.
        teams <- either (\(_ :: Failure) -> []) id <$> try (run client (Api.api.teams o r))
        sized <- traverse (\t -> (,) t . either (\(_ :: Failure) -> Nothing) (Just . length) <$> try (run client (Api.api.teamMembers t.id))) teams
        -- Suggestions are a help: without them the list still comes.
        (author, touched) <- case pr of
          Nothing -> pure (Nothing, [])
          Just n -> either (\(_ :: Failure) -> (Nothing, [])) id <$> try (do
            pull <- run client (Api.api.pullRequest o r n)
            text <- run client (Api.api.pullDiff o r (Api.DiffOf n))
            let paths = take 10 (either (const []) (map (.path)) (parseDiff text))
                commitsOf p = map ((,) p) <$> run client (Api.api.fileCommits o r
                  Api.FileCommitsQuery {sha = pull.base.ref, path = p, limit = 5, stat = False, verification = False, files = False})
            (,) (Just pull.user.login) . concat <$> traverse commitsOf paths)
        pure (candidatesOf author people touched sized)
    -- Forgejo has no reply to a thread; a comment on the same line joins it.
    , reply = \repo n t text -> () <$ run client (Api.api.review (owner repo) (shortName repo) n
        Api.NewReview {verdict = CommentOnly, body = "", lines = [Api.LineComment {path = t.path, line = t.line, body = text}]})
    , recentCommits = \repo branch -> map (.commit) <$> run client
        (Api.api.commits (owner repo) (shortName repo)
          Api.CommitsQuery {sha = branch, path = Nothing, page = Nothing, limit = 10, stat = False, verification = False, files = False})
    , fileHistory = \repo branch file -> map (.commit) <$> run client
        (Api.api.commits (owner repo) (shortName repo)
          Api.CommitsQuery {sha = branch, path = Just file, page = Just 1, limit = 50, stat = False, verification = False, files = False})
    , history = \repo branch n -> map (.commit) <$> run client
        (Api.api.commits (owner repo) (shortName repo)
          Api.CommitsQuery {sha = branch, path = Nothing, page = Just n, limit = 50, stat = False, verification = False, files = False})
    , branchList = \repo -> map (\b -> Br.Branch {name = b.name, pushed = b.pushed, ahead = Nothing, behind = Nothing}) <$> run client
        (everyPage (\page limit -> Api.api.branches (owner repo) (shortName repo) Api.ListQuery {page = page, limit = limit}))
    , upload = \repo n name bytes -> do
        boundary <- genBoundary
        let form = MultipartData [] [FileData "attachment" name (mimeOf name) (BL.fromStrict bytes)]
        (.browserDownloadUrl) <$> run client (Api.api.upload (owner repo) (shortName repo) n (Just name) (boundary, form))
    -- Forgejo answers 404 for a repository the user has not starred.
    , search = \words' -> do
        let issuesOf' kind = run client (Api.api.search Api.SearchQuery
              { q = Just words', kind = Named kind, state = Api.AnyState, since = Nothing, assigned = Nothing
              , created = Nothing, mentioned = Nothing, reviewRequested = Named Nothing, page = Just 1, limit = Just 10 })
            found kind i = S.Found { kind = kind, repo = unparse i.repository.fullName, number = Just (unparse i.number)
                                   , title = i.title, closed = isJust i.closedAt, updated = i.updatedAt }
        pulls <- issuesOf' Api.Pulls
        issues <- issuesOf' Api.Issues
        Api.FoundRepositories repos <- run client (Api.api.searchRepositories (Just words') (Just 10))
        pure ( map (found S.PullRequest) pulls <> map (found S.Issue) issues
            <> [ S.Found {kind = S.Repository, repo = unparse r.fullName, number = Nothing, title = r.description, closed = False, updated = r.updatedAt}
               | r <- repos ] )
    , starred = \repo -> do
        answer <- try (run client (Api.api.isStarred (owner repo) (shortName repo)))
        case answer of
          Right _ -> pure True
          Left (Refused why) | "404" `T.isInfixOf` why -> pure False
          Left failure -> throwIO failure
    , setStarred = \repo on -> () <$ run client ((if on then Api.api.star else Api.api.unstar) (owner repo) (shortName repo))
    , releases = \repo -> map releaseOf <$> run client
        (everyPage (\page limit -> Api.api.releases (owner repo) (shortName repo) Api.ListQuery {page = page, limit = limit}))
    , tags = \repo -> map (\t -> Br.Tag {name = t.name, at = t.made}) <$> run client
        (everyPage (\page limit -> Api.api.tags (owner repo) (shortName repo) Api.ListQuery {page = page, limit = limit}))
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
whoAmI manager base token = (.login) <$> run (connect manager base (pure (authorization token))) Api.api.me

authorization :: Token -> ByteString
authorization token = "token " <> encodeUtf8 (unparse token)

-- | Every page of a list, 50 at a time.
repoLabels :: Client -> RepoName -> IO [Api.RepoLabel]
repoLabels client repo = run client
  (everyPage (\page limit -> Api.api.labels (owner repo) (shortName repo) Api.ListQuery {page = page, limit = limit}))

milestonesOf :: Client -> RepoName -> IO [Api.ApiMilestone]
milestonesOf client repo = run client
  (everyPage (\page limit -> Api.api.milestones (owner repo) (shortName repo) Api.MilestoneQuery {state = "all", page = page, limit = limit}))

-- | Open ones first, those due soonest first, those without a date last.
dueOrder :: Api.ApiMilestone -> (Bool, Bool, Maybe UTCTime, Text)
dueOrder m = (not m.milestone.open, m.milestone.due == Nothing, m.milestone.due, m.milestone.title)

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
      { q = Nothing
      , kind = Named kind
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
  found <- traverse (pullRequestOf client cache host) asked
  writeIORef running (Map.fromList [(keyOf i, entry) | (entry@(i, _), pr) <- zip asked found, pr.checks == Just P.ChecksRunning])
  pure found
  where
    keyOf i = (i.repository.fullName, i.number)

-- | A pull request found by a search or a list, with its branches, state,
-- checks and review; the details are kept until it changes.
pullRequestOf :: Client -> IORef (Map.Map (RepoName, Number) Details) -> Host -> (Api.Issue, [Involvement]) -> IO P.PullRequest
pullRequestOf client cache host (i, involvement) = do
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
  | pull.closed = P.Closed
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

-- | Each reviewer's latest review that counts, oldest first, then those
-- asked who have not reviewed, people and teams. Forgejo keeps a reviewer
-- among the requested after they review and does not list requests among
-- the reviews, so a request made again after a review does not show. The
-- author's own comments are no review.
reviewersOf :: Api.PullRequest -> [Api.Review] -> [Rv.Reviewer]
reviewersOf pull given = reviewed <> pending <> teams
  where
    counted = [(u.login, (stance, r.stale)) | r <- sortOn (.id) given, not r.dismissed, Just u <- [r.user], u.login /= pull.user.login, Just stance <- [stanceOf r.state]]
    latest = Map.fromList counted
    order = nub (map fst counted)
    requested = map (.login) pull.requestedReviewers
    reviewed =
      [ Rv.Reviewer {who = Rv.Person l, stance = stance, stale = stale}
      | l <- order, Just (stance, stale) <- [Map.lookup l latest] ]
    pending = [Rv.Reviewer {who = Rv.Person l, stance = Rv.Waiting, stale = False} | l <- requested, l `notElem` order]
    teams = [Rv.Reviewer {who = Rv.Team t.name, stance = Rv.Waiting, stale = False} | t <- pull.requestedTeams]
    stanceOf s = case s of
      "APPROVED" -> Just Rv.Approved
      "REQUEST_CHANGES" -> Just Rv.ChangesRequested
      "COMMENT" -> Just Rv.Commented
      _ -> Nothing

-- | Collaborators who changed the files, latest first, then the other
-- collaborators, then teams; never the author. @touched@ pairs a file with
-- a commit that changed it.
candidatesOf :: Maybe Login -> [Api.Person] -> [(Text, Api.Authored)] -> [(Api.Team, Maybe Int)] -> [Rv.Candidate]
candidatesOf author people touched teams = sortOn (Down . (.lastChanged)) suggested <> others <> teamCandidates
  where
    everyone = [p | p <- people, Just p.login /= author]
    person p = Rv.Candidate {who = Rv.Person p.login, about = p.fullName, changed = changedBy p.login, lastChanged = lastBy p.login, members = Nothing}
    byOf login = sortOn (Down . snd) [(path, c.date) | (path, c) <- touched, Just u <- [c.author], u.login == login]
    changedBy login = nub (map (fileName . fst) (byOf login))
    lastBy login = snd <$> listToMaybe (byOf login)
    (suggested, others) = partition (not . null . (.changed)) (map person everyone)
    teamCandidates = [Rv.Candidate {who = Rv.Team t.name, about = t.description, changed = [], lastChanged = Nothing, members = size} | (t, size) <- teams]
    fileName = last . (:) "" . T.splitOn "/"

-- | A branch protection's rule against a branch name: the name itself, or
-- a pattern where @*@ stands for any text.
matches :: Text -> Text -> Bool
matches rule name = case T.splitOn "*" rule of
  [exact] -> exact == name
  first : rest -> first `T.isPrefixOf` name && go (T.drop (T.length first) name) rest
  [] -> False
  where
    go left parts = case parts of
      [lastPart] -> lastPart `T.isSuffixOf` left
      part : more -> case T.breakOn part left of
        (_, found) | not (T.null part) && T.null found -> False
                   | otherwise -> go (T.drop (T.length part) found) more
      [] -> True

-- | A title without Forgejo's draft marks, "WIP:" or "[WIP]".
unDraft :: Text -> Text
unDraft title = T.stripStart (go (T.stripStart title))
  where
    go t = case [rest | p <- ["wip:", "[wip]"], let (h, rest) = T.splitAt (T.length p) t, T.toLower h == p] of
      rest : _ -> rest
      [] -> t

-- | Reactions counted per feeling, in the forges' order, marking the
-- user's own.
tally :: Login -> [Api.ApiReaction] -> [D.Reaction]
tally login rs =
  [ D.Reaction {feeling = f, count = length mine' + length others, mine = not (null mine')}
  | f <- [minBound .. maxBound]
  , let (mine', others) = partition ((== login) . (.user)) [x | x <- rs, x.feeling == f]
  , not (null mine' && null others) ]

-- | People and teams as Forgejo takes them in a review request.
requestOf :: [Rv.Who] -> Api.Reviewers
requestOf whos = Api.Reviewers {reviewers = [l | Rv.Person l <- whos], teams = [t | Rv.Team t <- whos]}

-- | The site's root from the API's path: "/api/v1" off its end.
siteOf :: String -> String
siteOf path = maybe path T.unpack (T.stripSuffix "/api/v1" (T.pack path))

-- | A status as a check. Forgejo Actions says a skipped job "Has been
-- skipped" in a success, and one waiting "Waiting to run" or "Blocked by
-- required conditions" in a pending one, like a running one.
checkOf :: Host -> Api.CommitStatus -> C.Check
checkOf host s = C.Check
  { name = fromMaybe s.context (T.stripPrefix "/ " s.context)
  , state = case s.status of
      "success" | "skipped" `T.isInfixOf` T.toLower s.description -> C.Skipped
      "success" -> C.Passed
      "warning" -> C.Passed
      "skipped" -> C.Skipped
      "failure" -> C.Failed
      "error" -> C.Failed
      _ | any (`T.isPrefixOf` s.description) ["Waiting", "Blocked"] -> C.Waiting
        | otherwise -> C.Running
  , description = s.description
  , url = case s.targetUrl of
      "" -> Nothing
      u | "/" `T.isPrefixOf` u -> Just ("https://" <> unparse host <> u)
        | otherwise -> Just u
  , job = jobOf s.targetUrl
  }

-- | Jobs gathered into their runs, newest run first. A run's jobs come in
-- the order the run made them, which is how its pages number them.
runsOf :: [Api.Task] -> [C.Run]
runsOf tasks =
  [ C.Run
      { number = n
      , workflow = t.workflowId
      , title = t.displayTitle
      , branch = t.headBranch
      , event = t.event
      , sha = t.headSha
      , state = overall (map (.state) jobs)
      , started = minimum (map (.startedAt) ts)
      , updated = maximum (map (.updatedAt) ts)
      , url = t.url
      , jobs = jobs
      }
  | ts@(t : _) <- map (sortOn (.id)) (sortOn (Down . maximum . map (.id)) (Map.elems byRun))
  , Just n <- [readMaybe (T.unpack (T.takeWhileEnd (/= '/') t.url))]
  , let jobs = zipWith (jobCheck n) [0 ..] ts
  ]
  where
    byRun = Map.fromListWith (flip (<>)) [(t.url, [t]) | t <- tasks]
    jobCheck n i t = C.Check
      { name = t.name
      , state = taskState t.status
      , description = case taskState t.status of
          C.Running -> "Running"
          C.Waiting -> "Waiting"
          C.Skipped -> "Skipped"
          _ -> "Took " <> took (diffUTCTime t.updatedAt t.startedAt)
      , url = Just (t.url <> "/jobs/" <> T.pack (show i))
      , job = Just C.Job {run = n, index = i}
      }
    took d = let s = round d :: Int in
      if s < 60 then T.pack (show s) <> "s" else T.pack (show (s `div` 60)) <> "m " <> T.pack (show (s `mod` 60)) <> "s"
    taskState s = case s of
      "success" -> C.Passed
      "failure" -> C.Failed
      "cancelled" -> C.Failed
      "skipped" -> C.Skipped
      "running" -> C.Running
      _ -> C.Waiting
    overall states
      | C.Failed `elem` states = C.Failed
      | C.Running `elem` states = C.Running
      | C.Waiting `elem` states = C.Waiting
      | all (== C.Skipped) states = C.Skipped
      | otherwise = C.Passed

releaseOf :: Api.ApiRelease -> Rel.Release
releaseOf r = Rel.Release
  { name = if T.null r.name then r.tagName else r.name
  , tag = r.tagName
  , notes = r.body
  , author = maybe "" (\u -> unparse u.login) r.author
  , published = r.publishedAt
  , draft = r.draft
  , prerelease = r.prerelease
  , url = r.htmlUrl
  , assets = [Rel.Asset {name = x.name, size = x.size, downloads = x.downloadCount, url = x.browserDownloadUrl} | x <- r.assets]
  }

-- | A file's type by its name, for an upload.
mimeOf :: Text -> Text
mimeOf name = case T.toLower (T.takeWhileEnd (/= '.') name) of
  "png" -> "image/png"
  "jpg" -> "image/jpeg"
  "jpeg" -> "image/jpeg"
  "gif" -> "image/gif"
  "webp" -> "image/webp"
  "svg" -> "image/svg+xml"
  "pdf" -> "application/pdf"
  "txt" -> "text/plain"
  "md" -> "text/markdown"
  "zip" -> "application/zip"
  _ -> "application/octet-stream"

-- | The job a status links, "…/actions/runs/{run}/jobs/{index}".
jobOf :: Text -> Maybe C.Job
jobOf u = case reverse (T.splitOn "/" u) of
  index : "jobs" : run' : "runs" : "actions" : _ ->
    C.Job <$> readMaybe (T.unpack run') <*> readMaybe (T.unpack index)
  _ -> Nothing

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
  pure (map (issueOf host) found)

issueOf :: Host -> (Api.Issue, [Involvement]) -> I.Issue
issueOf host (i, involvement) = I.Issue
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

wordsOf :: Lt.Listing -> Maybe Text
wordsOf listing = let w = T.strip listing.words in if T.null w then Nothing else Just w

-- | How many there are in a state, from Forgejo's count of a page of one.
countOf :: Client -> RepoName -> Api.IssueType -> Api.IssueState -> Text -> IO (Maybe Int)
countOf client repo kind state words' = do
  answer <- run client (Api.api.repoIssues (owner repo) (shortName repo)
    Api.RepoIssuesQuery {kind = Named kind, state = state, sort = Nothing, q = wordsOf (Lt.Listing Lt.Every Lt.RecentlyUpdated words'), page = Just 1, limit = Just 1})
  pure $ case lookupResponseHeader @"X-Total-Count" answer of
    Header t -> Just t
    _ -> Nothing

-- | A page of a repository's issues or pull requests: 20 at a time, as
-- each pull request costs more calls; the cursor is the page's number.
repoPage :: Client -> RepoName -> Api.IssueType -> Lt.Listing -> Maybe Text -> IO ([Api.Issue], Maybe Int, Maybe Text)
repoPage client repo kind listing from = do
  let n = fromMaybe 1 (from >>= readMaybe . T.unpack) :: Int
      state = case listing.which of
        Lt.Opened -> Api.Open
        Lt.Drafts -> Api.Open
        Lt.Merged -> Api.Closed
        Lt.Shut -> Api.Closed
        Lt.Every -> Api.AnyState
      sort = case listing.order of
        Lt.RecentlyUpdated -> "recentupdate"
        Lt.Newest -> "latest"
        Lt.Oldest -> "oldest"
  answer <- run client (Api.api.repoIssues (owner repo) (shortName repo)
    Api.RepoIssuesQuery {kind = Named kind, state = state, sort = Just sort, q = wordsOf listing, page = Just n, limit = Just 20})
  let latest = getResponse answer
      total = case lookupResponseHeader @"X-Total-Count" answer of
        Header t -> Just t
        _ -> Nothing
  pure (latest, total, if length latest < 20 then Nothing else Just (T.pack (show (n + 1))))
