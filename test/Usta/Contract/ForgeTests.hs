{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | usta's GitHub and Forgejo code against their recorded answers: every
-- operation that reads from a forge reads its answer.
module Usta.Contract.ForgeTests (tests) where

import Control.Exception (evaluate)
import Control.Monad (void)

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (..))
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import System.Environment (lookupEnv)

import Usta.Contract.Replay (Server (..), start)
import Usta.Forge (Fetched (..), Forge (..), Known (..))
import Usta.Forge.Forgejo (forgejo)
import qualified Usta.Forge.GitHub as GitHub
import Usta.Types.Check (Check (..))
import Usta.Types.Commit (Commit (..))
import Usta.Types.Listing (Listing (..), Order (..), Which (..))
import Usta.Types.Parse (parseM)
import Usta.Types.Release (Release (..))
import Usta.Types.Number (Number)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Token (Token)

tests :: TestTree
tests =
  dependentTestGroup "Contract" AllFinish
    [ forgeTests "forgejo" forgejoPlace $ \server -> do
        token <- recordedToken "USTA_FORGEJO_TOKEN_FILE"
        manager <- newManager defaultManagerSettings
        host <- parseM "localhost:3443"
        forgejo manager server.base {baseUrlPath = "/api/v1"} host (pure token)
    , forgeTests "github" githubPlace $ \server -> do
        token <- parseM "recorded"
        manager <- newManager defaultManagerSettings
        host <- parseM "github.com"
        GitHub.github manager GitHub.Urls {rest = server.base, graph = server.base {baseUrlPath = "/graphql"}} host (pure token)
    ]

-- | Where on the forge the operations look.
data Place = Place
  { repo :: Text
    -- ^ A repository with releases, runs and issues.
  , pull :: Int
    -- ^ A pull request in it with checks.
  , issue :: Int
  , branch :: Text
  , path :: Text
    -- ^ A file on that branch.
  , reviewed :: (Text, Int)
    -- ^ A pull request with review threads.
  }

forgejoPlace, githubPlace :: Place
forgejoPlace = Place {repo = "tester/usta", pull = 4, issue = 2, branch = "master", path = "README.md", reviewed = ("tester/made-by-usta", 2)}
githubPlace = Place {repo = "abdivasiyev/usta", pull = 258, issue = 261, branch = "master", path = "README.md", reviewed = ("abdivasiyev/usta", 258)}

-- | Each operation as a test, on one server; the last says whether usta
-- asked for anything that was not recorded.
forgeTests :: String -> Place -> (Server -> IO Forge) -> TestTree
forgeTests name place connect =
  withResource (start name) (.finish) $ \getServer ->
    let forge = getServer >>= connect
     in dependentTestGroup name AllFinish $
          [testCase op (forge >>= run) | (op, run) <- operations place]
            <> [testCase "asked only for what was recorded" (getServer >>= (.missed) >>= (@?= []))]

-- | The token the recording signs in with, from a file; any while
-- answers are replayed.
recordedToken :: String -> IO Token
recordedToken variable = lookupEnv variable >>= maybe (parseM "recorded") (\file -> readFile file >>= parseM . T.strip . T.pack)

-- | Every reading operation, its answer read to the end.
operations :: Place -> [(String, Forge -> IO ())]
operations place =
  [ ("notifications", \f -> f.notifications none >>= listed)
  , ("pull requests", \f -> f.pullRequests none >>= listed)
  , ("issues", \f -> f.issues none >>= listed)
  , ("repositories", \f -> f.repositories none >>= listed)
  , ("a repository", \f -> repo >>= f.repository >>= done)
  , ("fork owners", \f -> f.forkOwners >>= done)
  , ("forks", \f -> repo >>= f.forks >>= done)
  , ("its parent", \f -> repo >>= f.parentOf >>= done)
  , ("can push", \f -> repo >>= f.canPush >>= done)
  , ("clone addresses", \f -> repo >>= f.cloneUrls >>= done . show)
  , ("a pull request's diff", \f -> at f.pullDiff >>= nonEmpty)
  , ("a pull request's commits", \f -> at f.pullCommits >>= nonEmpty)
  , ("review threads", \f -> reviewedAt f.threads >>= done)
  , ("checks", \f -> at f.checks >>= done)
  , ("a job's log", \f -> at f.checks >>= \cs -> case mapMaybe (.job) cs of
        j : _ -> repo >>= \r -> f.checkLog r j >>= done
        [] -> pure ())
  , ("reviewers", \f -> at f.reviewers >>= done)
  , ("readiness", \f -> at f.readiness >>= done)
  , ("who may review", \f -> repo >>= \r -> number place.pull >>= \n -> f.reviewCandidates r (Just n) >>= done)
  , ("recent commits", \f -> repo >>= \r -> f.recentCommits r place.branch >>= nonEmpty)
  , ("a commit's diff", \f -> repo >>= \r -> f.recentCommits r place.branch >>= \case
        c : _ -> f.commitDiff r c.sha >>= done
        [] -> assertFailure "no commits")
  , ("the readme", \f -> repo >>= \r -> f.readme r place.branch >>= done)
  , ("a conversation", \f -> at f.discussion >>= done)
  , ("an issue's conversation", \f -> repo >>= \r -> number place.issue >>= f.discussion r >>= done)
  , ("watching", \f -> at f.watching >>= done)
  , ("a timeline", \f -> at f.timeline >>= done)
  , ("the repository's pull requests", \f -> repo >>= \r -> f.repoPullRequests r (Listing Every RecentlyUpdated "") Nothing >>= done)
  , ("the repository's issues", \f -> repo >>= \r -> f.repoIssues r (Listing Opened Newest "") Nothing >>= done)
  , ("the repository's counts", \f -> repo >>= \r -> f.repoCounts r True "" >>= done)
  , ("runs", \f -> repo >>= f.runs >>= done)
  , ("workflows", \f -> repo >>= f.workflows >>= done)
  , ("startable workflows", \f -> repo >>= \r -> f.startable r place.branch >>= done)
  , ("branches", \f -> repo >>= f.branches >>= nonEmpty)
  , ("branches in full", \f -> repo >>= f.branchList >>= nonEmpty)
  , ("tags", \f -> repo >>= f.tags >>= done)
  , ("search", \f -> f.search "usta" >>= done)
  , ("starred", \f -> repo >>= f.starred >>= done)
  , ("releases, a bot's among them", \f -> repo >>= f.releases >>= \rs -> do
        nonEmpty rs
        void (evaluate (length (concatMap (show . (.tag)) rs))))
  , ("a file's history", \f -> repo >>= \r -> f.fileHistory r place.branch place.path >>= nonEmpty)
  , ("history", \f -> repo >>= \r -> f.history r place.branch 1 >>= nonEmpty)
  , ("labels", \f -> repo >>= f.labels >>= done)
  , ("who may be assigned", \f -> repo >>= f.assignable >>= done)
  , ("milestones", \f -> repo >>= f.milestones >>= done)
  , ("comparing branches", \f -> repo >>= \r -> f.compare r place.branch place.branch >>= done)
  , ("the files", \f -> repo >>= \r -> f.tree r place.branch >>= nonEmpty)
  , ("a file", \f -> repo >>= \r -> f.file r place.branch place.path >>= \b -> assertBool "empty" (b /= mempty))
  ]
  where
    none = Known {etag = Nothing, since = Nothing}
    repo = parseM place.repo :: IO RepoName
    number :: Int -> IO Number
    number n = parseM n
    at :: (RepoName -> Number -> IO a) -> IO a
    at op = repo >>= \r -> number place.pull >>= op r
    reviewedAt :: (RepoName -> Number -> IO a) -> IO a
    reviewedAt op = parseM (fst place.reviewed) >>= \r -> number (snd place.reviewed) >>= op r

-- | Read to the end: whatever a forge sent is all parsed.
done :: Show a => a -> IO ()
done = void . evaluate . length . show

nonEmpty :: Show a => [a] -> IO ()
nonEmpty xs = assertBool "nothing came" (not (null xs)) >> done xs

listed :: Show a => Fetched [a] -> IO ()
listed = \case
  Fetched {items} -> nonEmpty items
  NotModified -> assertFailure "not modified, with no ETag sent"

