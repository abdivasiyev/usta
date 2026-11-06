{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What usta sends when it changes something on Forgejo, checked
-- against Forgejo's own description of its API: every write's path,
-- method, query and body. A stand-in answers the reads as Forgejo did
-- and keeps the writes, so nothing changes on a real forge.
module Usta.Contract.WritesTests (tests) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value, eitherDecodeFileStrict)
import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl (..))
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (DependencyType (..), TestTree, dependentTestGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Usta.Contract.Replay (Server (..), Written (..), standIn)
import Usta.Contract.Swagger (conforms)
import Usta.Forge (Failure (..), Forge (..), Fork (..), LineNote (..), NewRepo (..))
import Usta.Forge.Forgejo (forgejo)
import Usta.Types.Discussion (Feeling (..), Merge (..), NewIssue (..), NewPull (..), Verdict (..), Watching (..))
import Usta.Types.Label (Definition (..))
import Usta.Types.Number (Number)
import Usta.Types.Parse (parseM)
import Usta.Types.Readiness (UpdateStyle (..))
import Usta.Types.RepoName (RepoName)
import Usta.Types.Reviewer (Who (..))
import Usta.Types.Thread (Thread, Threads (..))
import Usta.Types.Workflow (Workflow (..))

tests :: TestTree
tests =
  withResource (standIn "forgejo" (const (200, "{}"))) (\(s, _) -> s.finish) $ \get ->
    dependentTestGroup "Forgejo's writes" AllFinish $
      [testCase name (sends get name act) | (name, act) <- writes]
        <> [ testCase "watching what is watched sends nothing" $ sendsNothing get (\f -> on 4 $ \r n -> f.setWatching r n Subscribed)
           , testCase "with nothing pending, nothing is discarded" $ sendsNothing get (\f -> reviewed >>= \(r, n) -> f.discardPending r n >>= (@?= 0))
           , testCase "reads only what was recorded" $ get >>= \(server, _) -> server.missed >>= (@?= [])
           , testCase "refuses what it has no API for" $ do
               (server, _) <- get
               f <- connect server
               r <- repo
               rerun <- try (f.rerun r 1 False)
               cancel <- try (f.cancelRun r 1)
               participating <- try (on 4 $ \pr n -> f.setWatching pr n Participating)
               map (either refusedOnly (const False)) [rerun, cancel, participating] @?= [True, True, True]
           ]
  where
    refusedOnly = \case
      Refused _ -> True
      _ -> False

-- | The swagger Forgejo serves at /swagger.v1.json, from the test server.
spec :: Value
spec = unsafePerformIO (eitherDecodeFileStrict "test/contract/forgejo-swagger.json" >>= either fail pure)
{-# NOINLINE spec #-}

connect :: Server -> IO Forge
connect server = do
  manager <- newManager defaultManagerSettings
  host <- parseM "localhost:3443"
  token <- parseM "stand-in"
  forgejo manager server.base {baseUrlPath = "/api/v1"} host (pure token)

-- | Runs @act@; it must send at least one write, and each must be one
-- Forgejo takes. What Forgejo answers does not matter here.
sends :: IO (Server, IO [Written]) -> String -> (Forge -> IO ()) -> IO ()
sends get name act = do
  (server, written) <- get
  f <- connect server
  before <- length <$> written
  _ <- try (act f) :: IO (Either SomeException ())
  new <- drop before <$> written
  assertBool (name <> " sent nothing") (not (null new))
  case [T.unpack (w.method <> " " <> w.path <> ": " <> e) | w <- new, Left e <- [conforms spec w]] of
    [] -> pure ()
    problems -> assertFailure (unlines problems)

sendsNothing :: IO (Server, IO [Written]) -> (Forge -> IO ()) -> IO ()
sendsNothing get act = do
  (server, written) <- get
  f <- connect server
  before <- length <$> written
  act f
  after <- length <$> written
  after - before @?= 0

on :: Int -> (RepoName -> Number -> IO a) -> IO a
on n act = repo >>= \r -> parseM n >>= act r

repo :: IO RepoName
repo = parseM "tester/usta"

reviewed :: IO (RepoName, Number)
reviewed = (,) <$> parseM "tester/made-by-usta" <*> parseM 2

-- | Every operation that changes something.
writes :: [(String, Forge -> IO ())]
writes =
  [ ("mark a notification read", \f -> parseM "1" >>= f.markRead)
  , ("mark it unread", \f -> parseM "1" >>= f.markUnread)
  , ("comment", \f -> on 2 $ \r n -> () <$ f.comment r n "From usta's tests")
  , ("edit a comment", \f -> on 2 $ \r n -> f.editComment r n (Just 1) "Edited")
  , ("edit the description", \f -> on 2 $ \r n -> f.editComment r n Nothing "A new description")
  , ("delete a comment", \f -> repo >>= \r -> f.deleteComment r 1)
  , ("edit the title", \f -> on 2 $ \r n -> f.editTitle r n "A new title")
  , ("react", \f -> on 2 $ \r n -> f.react r n Nothing Rocket True)
  , ("take a reaction back", \f -> on 2 $ \r n -> f.react r n Nothing Rocket False)
  , ("react to a comment", \f -> on 2 $ \r n -> f.react r n (Just 1) Heart True)
  , ("close", \f -> on 2 $ \r n -> f.setClosed r n True)
  , ("merge", \f -> on 4 $ \r n -> f.merge r n Squash True)
  , ("merge when checks pass", \f -> on 4 $ \r n -> f.scheduleMerge r n MergeCommit False)
  , ("cancel a scheduled merge", \f -> on 4 $ \r n -> f.cancelScheduledMerge r n)
  , ("update the branch", \f -> on 4 $ \r n -> f.updateBranch r n ByMerge)
  , ("mark ready", \f -> on 4 $ \r n -> f.markReady r n)
  , ("review with a line note", \f -> reviewed >>= \(r, n) -> f.review r n RequestChanges "Please look" [LineNote {path = "README.md", line = 1, body = "Here"}])
  , ("approve", \f -> reviewed >>= \(r, n) -> f.review r n Approve "" [])
  , ("reply to a thread", \f -> reviewed >>= \(r, n) -> firstThread f r n >>= \t -> f.reply r n t "Done")
  , ("ask for a review", \f -> on 4 $ \r n -> parseM "reviewer" >>= \l -> f.requestReview r n [Person l])
  , ("withdraw it", \f -> on 4 $ \r n -> parseM "reviewer" >>= \l -> f.unrequestReview r n (Person l))
  , ("file an issue", \f -> repo >>= \r -> parseM "tester" >>= \l -> () <$ f.createIssue r NewIssue {title = "From usta", body = "Body", labels = [], assignees = [l]})
  , ("open a pull request", \f -> repo >>= \r -> () <$ f.createPull r NewPull {title = "From usta", body = "Body", from = "feature", into = "master", labels = [], reviewers = [], draft = True})
  , ("set labels", \f -> on 2 $ \r n -> f.labels r >>= \ls -> f.setLabels r n [d.name | d <- take 1 ls])
  , ("make a label", \f -> repo >>= \r -> parseM "d73a4a" >>= \c -> f.createLabel r Definition {name = "from-usta", color = c, description = "Made by usta's tests"})
  , ("assign", \f -> on 2 $ \r n -> parseM "tester" >>= \l -> f.setAssignees r n [l])
  , ("take the milestone away", \f -> on 2 $ \r n -> f.setMilestone r n Nothing)
  , ("mute", \f -> on 4 $ \r n -> f.setWatching r n Muted)
  , ("star", \f -> repo >>= \r -> f.setStarred r True)
  , ("unstar", \f -> repo >>= \r -> f.setStarred r False)
  , ("fork", \f -> repo >>= \r -> () <$ f.fork r Fork {owner = Nothing, name = "usta-fork", defaultBranchOnly = False})
  , ("make a repository", \f -> () <$ f.createRepository NewRepo {owner = Nothing, name = "from-usta", description = "Made by usta's tests", private = True, readme = True})
  , ("make one in an organization", \f -> () <$ f.createRepository NewRepo {owner = Just "teamorg", name = "from-usta", description = "", private = False, readme = False})
  , ("attach a file", \f -> on 2 $ \r n -> () <$ f.upload r n "notes.txt" (B8.pack "hello"))
  , ("start a workflow", \f -> repo >>= \r -> f.dispatch r Workflow {name = "deploy", file = ".forgejo/workflows/deploy.yml", inputs = []} "master" [("target", "staging")])
  ]

firstThread :: Forge -> RepoName -> Number -> IO Thread
firstThread f r n = f.threads r n >>= \ts -> case ts.threads of
  t : _ -> pure t
  [] -> fail "no thread recorded"

