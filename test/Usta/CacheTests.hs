{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The cache: what it keeps, what it refuses, and what goes with an
account.
-}
module Usta.CacheTests (tests) where

import Control.Exception (try)
import Data.Either (isLeft)
import Data.List (sort)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Usta.DB (WrongHost, run)
import Usta.DB.Account (deleteAccount, listAccounts, saveAccount)
import Usta.DB.Cache (clear)
import Usta.DB.Issue (issues, saveIssues)
import Usta.DB.Notification (InboxItem (..), inbox, saveNotifications, setDone, setUnread)
import Usta.DB.PullRequest (pullRequests, savePullRequests)
import Usta.DB.Repository (repositories, saveRepositories)
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (Fetched (..), Forge (..), Known (..))
import Usta.SyncTests (githubAccount, withCache)
import Usta.Types.Account (Account (..))
import Usta.Types.Count (Count)
import Usta.Types.Issue (Issue)
import Usta.Types.Notification (Notification (..), Subject (..))
import Usta.Types.Parse (parseM, unparse)
import Usta.Types.PullRequest (PullRequest (..))
import Usta.Types.Repository (Repository (..))

import Usta.Forge.Sample qualified as Sample

tests :: TestTree
tests =
    testGroup
        "Cache"
        [ testCase "what is saved reads back the same" $ withCache $ \pool -> do
            (key, lists) <- filled pool
            prs <- run pool pullRequests
            sort (map (show . (.ref) . snd) prs) @?= sort (map (show . (.ref)) lists.prs)
            map fst prs @?= map (const key) prs
            length <$> run pool issues >>= (@?= length lists.issues)
            rs <- run pool repositories
            sort (map (unparse . (.name) . snd) rs) @?= sort (map (unparse . (.name)) lists.repos)
            found <- run pool inbox
            sort (map (unparse . (.notification.threadId)) found) @?= sort (map (unparse . (.threadId)) lists.notes)
        , testCase "an item from another server is refused, and nothing is saved" $ withCache $ \pool -> do
            now <- getCurrentTime
            account <- githubAccount
            key <- run pool (saveAccount now account)
            homelab <- (\h -> Account {forge = account.forge, host = h, login = account.login, authMethod = account.authMethod}) <$> parseM "git.azizovich.uz"
            theirs <- Sample.new now homelab >>= \s -> items (s.forge.pullRequests none)
            mine <- Sample.new now account >>= \s -> items (s.forge.pullRequests none)
            refused <- try @WrongHost (run pool (savePullRequests key now (mine <> theirs)))
            assertBool "saved" (isLeft refused)
            length <$> run pool pullRequests >>= (@?= 0)
        , testCase "an account signing in again keeps its id" $ withCache $ \pool -> do
            now <- getCurrentTime
            account <- githubAccount
            first <- run pool (saveAccount now account)
            second <- run pool (saveAccount now account)
            first @?= second
            length <$> run pool listAccounts >>= (@?= 1)
        , testCase "removing an account removes what was synced for it" $ withCache $ \pool -> do
            (key, _) <- filled pool
            run pool (deleteAccount key)
            sizes pool >>= (@?= (0, 0, 0, 0))
        , testCase "clearing the cache keeps the accounts" $ withCache $ \pool -> do
            _ <- filled pool
            run pool clear
            sizes pool >>= (@?= (0, 0, 0, 0))
            length <$> run pool listAccounts >>= (@?= 1)
        , testCase "a list sync keeps the counts only a pull request's page fetched" $ withCache $ \pool -> do
            (key, lists) <- filled pool
            now <- getCurrentTime
            files <- parseM 12 :: IO Count
            run pool (savePullRequests key now [pr {changedFiles = Just files} | pr <- lists.prs])
            run pool (savePullRequests key now lists.prs)
            map ((.changedFiles) . snd) <$> run pool pullRequests >>= (@?= map (const (Just files)) lists.prs)
        , testCase "read, unread and done" $ withCache $ \pool -> do
            (key, _) <- filled pool
            thread <- parseM "1"
            run pool (setUnread key thread False >> setDone key thread True)
            let
                this = filter ((== thread) . (.notification.threadId))
            map (\i -> (i.notification.unread, i.done)) . this <$> run pool inbox >>= (@?= [(False, True)])
            run pool (setDone key thread False)
            map (.done) . this <$> run pool inbox >>= (@?= [False])
        , testCase "the inbox shows the pull request each is about" $ withCache $ \pool -> do
            _ <- filled pool
            found <- run pool inbox
            [isJustPr i | i <- found, aboutPull i.notification] @?= [True | i <- found, aboutPull i.notification]
        ]
    where
        none = Known {etag = Nothing, since = Nothing}
        aboutPull n = case n.subject of
            PullRequestSubject _ -> True
            _ -> False
        isJustPr i = maybe False (const True) i.pullRequest

-- | The sample forge's github.com lists.
data Lists = Lists {prs :: [PullRequest], issues :: [Issue], repos :: [Repository], notes :: [Notification]}

-- | A cache with the github.com account and its sample lists.
filled :: ConnectionPool -> IO (AccountId, Lists)
filled pool = do
    now <- getCurrentTime
    account <- githubAccount
    key <- run pool (saveAccount now account)
    forge <- (.forge) <$> Sample.new now account
    lists <- fetchAll forge
    run pool $ do
        saveNotifications key lists.notes
        savePullRequests key now lists.prs
        saveIssues key now lists.issues
        saveRepositories key now lists.repos
    pure (key, lists)
    where
        fetchAll :: Forge -> IO Lists
        fetchAll f =
            Lists
                <$> items (f.pullRequests none)
                <*> items (f.issues none)
                <*> items (f.repositories none)
                <*> items (f.notifications none)
        none = Known {etag = Nothing, since = Nothing}

items :: IO (Fetched [a]) -> IO [a]
items fetch =
    fetch >>= \case
        Fetched {items = xs} -> pure xs
        NotModified -> assertFailure "not modified" >> pure []

-- | Notifications, pull requests, issues, repositories in the cache.
sizes :: ConnectionPool -> IO (Int, Int, Int, Int)
sizes pool = run pool $ (,,,) <$> (length <$> inbox) <*> (length <$> pullRequests) <*> (length <$> issues) <*> (length <$> repositories)
