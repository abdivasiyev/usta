{-# LANGUAGE OverloadedStrings #-}

-- | How fast usta's heavy parts are, so "fast" stays true: reading and
-- coloring big diffs, laying out big diagrams, the commit graph, and
-- saving and reading a large cache. Compare with a baseline:
--
-- > cabal bench --benchmark-options='--csv bench/baseline.csv'
-- > cabal bench --benchmark-options='--baseline bench/baseline.csv --fail-if-slower 20'
module Main (main) where

import Data.Functor.Identity (Identity (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, fromGregorian)
import Database.Persist.Sql (ConnectionPool)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf, nfIO, whnfIO)

import Usta.DB (run, withDatabase)
import Usta.DB.Account (saveAccount)
import Usta.DB.Notification (inbox, saveNotifications)
import Usta.DB.PullRequest (pullRequests, savePullRequests)
import Usta.DB.Schema.Account (AccountId)
import qualified Usta.Diagram as Diagram
import Usta.Diagram.Scene (Font (..), Scene (..), Size (..), extent)
import Usta.Graph (Row (..), rows)
import Usta.Types.Account (Account (..), AuthMethod (..))
import Usta.Types.Diff (FileDiff (..), parseDiff)
import qualified Usta.Types.Forge as Kind
import Usta.Types.Insights (Node (..))
import Usta.Types.Involvement (Involvement (..))
import Usta.Types.Notification (Notification (..), Reason (..), Subject (..))
import Usta.Types.Parse (parseM)
import Usta.Types.PullRequest (PullRequest (..), State (..))
import Usta.Types.Ref (Ref (..))
import Usta.UI.Highlight (Palette (..), languageNamed, markup)

main :: IO ()
main = withSystemTempDirectory "usta-bench" $ \home -> do
  setEnv "HOME" home
  -- One cache, filled once: the benchmarks time the work, not making it.
  withCache $ \pool key -> do
    run pool (saveNotifications key manyNotes >> savePullRequests key at manyPulls)
    defaultMain
      [ bgroup "diff"
          [ bench "read 20,000 lines" (nf (either (const 0) (sum . map (length . (.lines))) . parseDiff) bigDiff)
          , bench "color 5,000 lines of Haskell" (nf (sum . map T.length . markup DarkPalette (languageNamed "haskell")) haskell)
          ]
      , bgroup "diagrams"
          [ bench "lay out a flowchart of 200 nodes" (nf (fmap covered . layout) bigFlowchart)
          , bench "lay out a sequence of 300 messages" (nf (fmap covered . layout) bigSequence)
          ]
      , bench "the graph of 5,000 commits" (nf (sum . map (\r -> r.column + r.width + length r.above + length r.below) . rows) history)
      , bgroup "cache"
          [ bench "save 1,000 pull requests again" (whnfIO (run pool (savePullRequests key at manyPulls)))
          , bench "read the inbox of 1,000" (nfIO (length <$> run pool inbox))
          , bench "read 1,000 pull requests" (nfIO (length <$> run pool pullRequests))
          ]
      ]

-- | Every number of the scene added up, so all of it is worked out.
covered :: Scene -> Double
covered s = s.size.w + s.size.h + sum [x + y | i <- s.items, (x, y) <- extent i]

at :: UTCTime
at = UTCTime (fromGregorian 2026 11 5) 0

bigDiff :: Text
bigDiff = T.unlines $ concat
  [ ["diff --git a/f" <> n <> ".hs b/f" <> n <> ".hs", "--- a/f" <> n <> ".hs", "+++ b/f" <> n <> ".hs", "@@ -1,100 +1,100 @@"]
      <> concat [[" keep " <> T.pack (show i), "-old " <> T.pack (show i), "+new " <> T.pack (show i)] | i <- [1 .. 66 :: Int]]
  | k <- [1 .. 100 :: Int], let n = T.pack (show k) ]

haskell :: Text
haskell = T.unlines (concat (replicate 1000 ["module A where", "-- | A comment.", "f :: Int -> Int", "f x = x + 1 -- one more", "g = \"text\" <> show (f 2)"]))

measure :: Font -> Text -> Identity Size
measure font t = Identity Size {w = 0.6 * font.size * fromIntegral (T.length t), h = 1.3 * font.size}

layout :: Text -> Either Text Scene
layout source = runIdentity . Diagram.draw Map.empty measure <$> Diagram.parse source

bigFlowchart :: Text
bigFlowchart = T.unlines ("flowchart TD" : ["  n" <> s i <> "[Step " <> s i <> "] --> n" <> s (i * 7 `mod` 200 + 1) | i <- [1 .. 200 :: Int]])
  where s = T.pack . show

bigSequence :: Text
bigSequence = T.unlines ("sequenceDiagram" : ["  " <> who i <> "->>" <> who (i + 1) <> ": message " <> T.pack (show i) | i <- [1 .. 300 :: Int]])
  where who i = ["usta", "Forge", "Cache", "Sync"] !! (i `mod` 4)

history :: [Node]
history =
  [ Node {sha = c i, parents = [c (i + 1)] <> [c (i + 3) | i `mod` 10 == 0], author = "a", at = at, subject = "", refs = []}
  | i <- [0 .. 4999 :: Int] ]
  where c i = "c" <> T.pack (show i)

-- | A fresh cache with one github.com account.
withCache :: (ConnectionPool -> AccountId -> IO a) -> IO a
withCache act = withSystemTempDirectory "usta-cache" $ \dir -> withDatabase (dir </> "usta.db") $ \pool -> do
  account <- Account Kind.GitHub <$> parseM "github.com" <*> parseM "abdivasiyev" <*> pure AccessToken
  key <- run pool (saveAccount at account)
  act pool key

manyPulls :: [PullRequest]
manyPulls = fromMaybe (error "the benchmark's data") $ traverse pull [1 .. 1000 :: Int]
  where
    pull n = do
      host <- parseM "github.com"
      repo <- parseM ("abdivasiyev/repo" <> T.pack (show (n `mod` 20)))
      number <- parseM n
      author <- parseM "abdivasiyev"
      comments <- parseM (n `mod` 9)
      pure PullRequest
        { ref = Ref {host = host, repo = repo, number = number}, title = "Pull request " <> T.pack (show n), author = author
        , headBranch = "feat/" <> T.pack (show n), headOwner = Nothing, baseBranch = "master", state = Open
        , checks = Nothing, review = Nothing, comments = comments, changedFiles = Nothing, commits = Nothing
        , involvement = [AsReviewer, AsAuthor], createdAt = at, updatedAt = addUTCTime (fromIntegral n) at }

manyNotes :: [Notification]
manyNotes = fromMaybe (error "the benchmark's data") $ traverse note [1 .. 1000 :: Int]
  where
    note n = do
      host <- parseM "github.com"
      repo <- parseM ("abdivasiyev/repo" <> T.pack (show (n `mod` 20)))
      thread <- parseM (T.pack (show n))
      number <- parseM n
      pure Notification
        { threadId = thread, host = host, repo = repo, subject = PullRequestSubject number, title = "Thread " <> T.pack (show n)
        , reason = Just ReviewRequested, unread = odd n, updatedAt = addUTCTime (fromIntegral n) at }
