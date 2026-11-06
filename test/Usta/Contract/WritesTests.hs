{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What usta sends when it changes something on a forge, checked against
-- the forge's own description of its API: Forgejo 13's swagger, GitHub's
-- OpenAPI description and GraphQL schema (the parts usta uses, in
-- test/contract). A stand-in answers the reads as the forge did and keeps
-- the writes, so nothing changes on a real forge.
module Usta.Contract.WritesTests (tests) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, decode, eitherDecodeFileStrict)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value (..))
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (..))
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Usta.Contract.GraphQL (mutationConforms)
import Usta.Contract.Replay (Server (..), Written (..), standIn)
import Usta.Contract.Swagger (conforms)
import Usta.Forge (Failure (..), Forge (..), Fork (..), LineNote (..), NewRepo (..))
import Usta.Forge.Forgejo (forgejo)
import qualified Usta.Forge.GitHub as GitHub
import Usta.Types.Check (Job (..))
import Usta.Types.Discussion (Feeling (..), Merge (..), NewIssue (..), NewPull (..), Verdict (..), Watching (..))
import Usta.Types.Label (Definition (..))
import Usta.Types.Number (Number)
import Usta.Types.Parse (parseM)
import Usta.Types.Readiness (UpdateStyle (..))
import Usta.Types.RepoName (RepoName)
import Usta.Types.Reviewer (Who (..))
import Usta.Types.Thread (Thread (..), Threads (..))
import Usta.Types.Workflow (Workflow (..))

tests :: TestTree
tests =
  testGroup
    "Writes"
    [ forgeWrites "forgejo" forgejoPlace connectForgejo (conforms forgejoSpec) $ \get ->
        [ testCase "refuses what it has no API for" $ do
            f <- get >>= connectForgejo . fst
            r <- repoOf forgejoPlace
            outcomes <- sequence
              [ try (f.rerun r 1 False)
              , try (f.cancelRun r 1)
              , try (on forgejoPlace forgejoPlace.pull $ \pr n -> f.setWatching pr n Participating) ]
            map (either refusedOnly (const False)) outcomes @?= [True, True, True]
        , testCase "watching what is watched sends nothing" $
            sendsNothing get connectForgejo (\f -> on forgejoPlace forgejoPlace.pull $ \r n -> f.setWatching r n Subscribed)
        , testCase "with nothing pending, nothing is discarded" $
            sendsNothing get connectForgejo (\f -> reviewedOf forgejoPlace >>= \(r, n) -> f.discardPending r n >>= (@?= 0))
        ]
    , forgeWrites "github" githubPlace connectGitHub githubConforms $ \get ->
        [ testCase "refuses what it has no API for" $ do
            f <- get >>= connectGitHub . fst
            unread <- try (parseM "1" >>= f.markUnread)
            attach <- try (on githubPlace githubPlace.issue $ \r n -> () <$ f.upload r n "notes.txt" (B8.pack "hello"))
            map (either refusedOnly (const False)) [unread, attach] @?= [True, True]
        ]
    ]
  where
    refusedOnly = \case
      Refused _ -> True
      _ -> False

-- | Where on a forge the writes go.
data Place = Place
  { repo :: Text
  , issue :: Int
  , pull :: Int
  , reviewed :: (Text, Int)
    -- ^ A pull request with review threads.
  , branch :: Text
  , org :: Text
  , person :: Text
  , workflow :: Text
  }

forgejoPlace, githubPlace :: Place
forgejoPlace = Place {repo = "tester/usta", issue = 2, pull = 4, reviewed = ("tester/made-by-usta", 2), branch = "master", org = "teamorg", person = "tester", workflow = ".forgejo/workflows/deploy.yml"}
githubPlace = Place {repo = "abdivasiyev/usta", issue = 261, pull = 258, reviewed = ("abdivasiyev/usta", 258), branch = "master", org = "someorg", person = "lambdajon", workflow = ".github/workflows/release.yml"}

forgejoSpec :: Value
forgejoSpec = unsafePerformIO (eitherDecodeFileStrict "test/contract/forgejo-swagger.json" >>= either fail pure)
{-# NOINLINE forgejoSpec #-}

githubSpec :: Value
githubSpec = unsafePerformIO (eitherDecodeFileStrict "test/contract/github-openapi.json" >>= either fail pure)
{-# NOINLINE githubSpec #-}

githubSchema :: Text
githubSchema = unsafePerformIO (T.readFile "test/contract/github-schema.graphql")
{-# NOINLINE githubSchema #-}

-- | A REST write against the OpenAPI description, a GraphQL one against
-- the schema.
githubConforms :: Written -> Either Text ()
githubConforms w
  | w.path == "/graphql" = case decode w.sent of
      Just (Object o) | Just (String q) <- KeyMap.lookup "query" o -> mutationConforms githubSchema q
      _ -> Left "a GraphQL request with no query"
  | otherwise = conforms githubSpec w

connectForgejo :: Server -> IO Forge
connectForgejo server = do
  manager <- newManager defaultManagerSettings
  host <- parseM "localhost:3443"
  token <- parseM "stand-in"
  forgejo manager server.base {baseUrlPath = "/api/v1"} host (pure token)

connectGitHub :: Server -> IO Forge
connectGitHub server = do
  manager <- newManager defaultManagerSettings
  host <- parseM "github.com"
  token <- parseM "stand-in"
  GitHub.github manager GitHub.Urls {rest = server.base, graph = server.base {baseUrlPath = "/graphql"}} host (pure token)

-- | Each write of 'writes' as a test on one stand-in, then @more@, then
-- whether every read was recorded.
forgeWrites :: String -> Place -> (Server -> IO Forge) -> (Written -> Either Text ()) -> (IO (Server, IO [Written]) -> [TestTree]) -> TestTree
forgeWrites name place connect check more =
  withResource (standIn name (const (200, "{}"))) (\(s, _) -> s.finish) $ \get ->
    dependentTestGroup name AllFinish $
      [testCase what (sends get connect check what act) | (what, act) <- writes place, applies name what]
        <> more get
        <> [ testCase "reads only what was recorded" $ get >>= \(server, _) -> server.missed >>= (@?= [])
           , testCase "kept for trimming GitHub's description" $ dump name get ]
  where
    -- GitHub reruns and cancels runs; Forgejo has no API for it.
    applies forge what
      | forge == "github" = what `notElem` ["mark it unread", "attach a file"]
      | otherwise = what `notElem` ["run again", "run a job again", "cancel a run", "mark it unread"]

-- | Runs @act@; it must send at least one write, and each must be one the
-- forge takes. What the forge answers does not matter here.
sends :: IO (Server, IO [Written]) -> (Server -> IO Forge) -> (Written -> Either Text ()) -> String -> (Forge -> IO ()) -> IO ()
sends get connect check name act = do
  (server, written) <- get
  f <- connect server
  before <- length <$> written
  _ <- try (act f) :: IO (Either SomeException ())
  new <- drop before <$> written
  assertBool (name <> " sent nothing") (not (null new))
  case [T.unpack (w.method <> " " <> w.path <> ": " <> e) | w <- new, Left e <- [check w]] of
    [] -> pure ()
    problems -> assertFailure (unlines problems)

sendsNothing :: IO (Server, IO [Written]) -> (Server -> IO Forge) -> (Forge -> IO ()) -> IO ()
sendsNothing get connect act = do
  (server, written) <- get
  f <- connect server
  before <- length <$> written
  act f
  after <- length <$> written
  after - before @?= 0

-- | With USTA_DUMP_WRITES set, every write's method and path, for
-- test/contract/trim-github.py.
dump :: String -> IO (Server, IO [Written]) -> IO ()
dump name get = lookupEnv "USTA_DUMP_WRITES" >>= mapM_ (\file -> do
  (_, written) <- get
  ws <- written
  appendFile file (unlines [name <> " " <> T.unpack w.method <> " " <> T.unpack w.path | w <- ws]))

on :: Place -> Int -> (RepoName -> Number -> IO a) -> IO a
on place n act = repoOf place >>= \r -> parseM n >>= act r

repoOf :: Place -> IO RepoName
repoOf place = parseM place.repo

reviewedOf :: Place -> IO (RepoName, Number)
reviewedOf place = (,) <$> parseM (fst place.reviewed) <*> parseM (snd place.reviewed)

-- | Every operation that changes something.
writes :: Place -> [(String, Forge -> IO ())]
writes p =
  [ ("mark a notification read", \f -> parseM "1" >>= f.markRead)
  , ("mark it unread", \f -> parseM "1" >>= f.markUnread)
  , ("comment", \f -> on p p.issue $ \r n -> () <$ f.comment r n "From usta's tests")
  , ("edit a comment", \f -> on p p.issue $ \r n -> f.editComment r n (Just 1) "Edited")
  , ("edit the description", \f -> on p p.issue $ \r n -> f.editComment r n Nothing "A new description")
  , ("delete a comment", \f -> repoOf p >>= \r -> f.deleteComment r 1)
  , ("edit the title", \f -> on p p.issue $ \r n -> f.editTitle r n "A new title")
  , ("react", \f -> on p p.issue $ \r n -> f.react r n Nothing Rocket True)
  , ("take a reaction back", \f -> on p p.issue $ \r n -> f.react r n Nothing Rocket False)
  , ("react to a comment", \f -> on p p.issue $ \r n -> f.react r n (Just 1) Heart True)
  , ("close", \f -> on p p.issue $ \r n -> f.setClosed r n True)
  , ("merge", \f -> on p p.pull $ \r n -> f.merge r n Squash True)
  , ("merge when checks pass", \f -> on p p.pull $ \r n -> f.scheduleMerge r n MergeCommit False)
  , ("cancel a scheduled merge", \f -> on p p.pull $ \r n -> f.cancelScheduledMerge r n)
  , ("update the branch by merging", \f -> on p p.pull $ \r n -> f.updateBranch r n ByMerge)
  , ("update the branch by rebasing", \f -> on p p.pull $ \r n -> f.updateBranch r n ByRebase)
  , ("mark ready", \f -> on p p.pull $ \r n -> f.markReady r n)
  , ("review with a line note", \f -> reviewedOf p >>= \(r, n) -> f.review r n RequestChanges "Please look" [LineNote {path = "README.md", line = 1, body = "Here"}])
  , ("approve", \f -> reviewedOf p >>= \(r, n) -> f.review r n Approve "" [])
  , ("reply to a thread", \f -> reviewedOf p >>= \(r, n) -> firstThread f r n >>= \t -> f.reply r n t "Done")
  , ("ask for a review", \f -> on p p.pull $ \r n -> parseM p.person >>= \l -> f.requestReview r n [Person l])
  , ("withdraw it", \f -> on p p.pull $ \r n -> parseM p.person >>= \l -> f.unrequestReview r n (Person l))
  , ("file an issue", \f -> repoOf p >>= \r -> parseM p.person >>= \l -> () <$ f.createIssue r NewIssue {title = "From usta", body = "Body", labels = [], assignees = [l]})
  , ("open a pull request", \f -> repoOf p >>= \r -> () <$ f.createPull r NewPull {title = "From usta", body = "Body", from = "feature", into = p.branch, labels = [], reviewers = [], draft = True})
  , ("set labels", \f -> on p p.issue $ \r n -> f.labels r >>= \ls -> f.setLabels r n [d.name | d <- take 1 ls])
  , ("make a label", \f -> repoOf p >>= \r -> parseM "d73a4a" >>= \c -> f.createLabel r Definition {name = "from-usta", color = c, description = "Made by usta's tests"})
  , ("assign", \f -> on p p.issue $ \r n -> parseM p.person >>= \l -> f.setAssignees r n [l])
  , ("take the milestone away", \f -> on p p.issue $ \r n -> f.setMilestone r n Nothing)
  , ("mute", \f -> on p p.pull $ \r n -> f.setWatching r n Muted)
  , ("star", \f -> repoOf p >>= \r -> f.setStarred r True)
  , ("unstar", \f -> repoOf p >>= \r -> f.setStarred r False)
  , ("fork", \f -> repoOf p >>= \r -> () <$ f.fork r Fork {owner = Nothing, name = "usta-fork", defaultBranchOnly = True})
  , ("make a repository", \f -> () <$ f.createRepository NewRepo {owner = Nothing, name = "from-usta", description = "Made by usta's tests", private = True, readme = True})
  , ("make one in an organization", \f -> () <$ f.createRepository NewRepo {owner = Just p.org, name = "from-usta", description = "", private = False, readme = False})
  , ("attach a file", \f -> on p p.issue $ \r n -> () <$ f.upload r n "notes.txt" (B8.pack "hello"))
  , ("start a workflow", \f -> repoOf p >>= \r -> f.dispatch r Workflow {name = "workflow", file = p.workflow, inputs = []} p.branch [("target", "staging")])
  , ("run again", \f -> repoOf p >>= \r -> f.rerun r 1 True)
  , ("cancel a run", \f -> repoOf p >>= \r -> f.cancelRun r 1)
  , ("run a job again", \f -> repoOf p >>= \r -> f.rerunJob r Job {run = 1, index = 1})
  ]

-- | The pull request's first thread; usta's own repository has none on
-- GitHub, so one there is made up, as GitHub's links name its comment.
firstThread :: Forge -> RepoName -> Number -> IO Thread
firstThread f r n = f.threads r n >>= \ts -> case ts.threads of
  t : _ -> pure t
  [] -> pure Thread {path = "README.md", line = 1, code = [], commit = "", resolvedBy = Nothing, notes = [], url = "https://github.com/abdivasiyev/usta/pull/258#discussion_r1234"}
