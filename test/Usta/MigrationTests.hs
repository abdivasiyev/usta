{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Caches written by earlier versions of usta still open, and keep what
was synced: a schema change that breaks one fails here first.

test/caches/<version>.db is a cache as that version left it, with the
sample forge's github.com lists. USTA_WRITE_CACHE=<version> writes one
for the version being built; keep one per release.
-}
module Usta.MigrationTests (tests) where

import Data.List (isSuffixOf, sort)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (rawExecute)
import System.Directory (copyFile, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeBaseName, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Usta.DB (run, withDatabase)
import Usta.DB.Account (listAccounts, saveAccount)
import Usta.DB.Issue (issues, saveIssues)
import Usta.DB.Notification (inbox, saveNotifications)
import Usta.DB.PullRequest (pullRequests, savePullRequests)
import Usta.DB.Repository (repositories, saveRepositories)
import Usta.Forge (Fetched (..), Forge (..), Known (..))
import Usta.SyncTests (githubAccount)
import Usta.Types.PullRequest (PullRequest (..))

import Usta.Forge.Sample qualified as Sample

tests :: TestTree
tests =
    testGroup
        "Migration"
        ( [testCase ("a cache from usta " <> v) (opens v) | v <- versions]
            <> [ testCase "a cache from before head owners" $ withSystemTempDirectory "usta-old" $ \dir -> do
                    let
                        file = dir </> "usta.db"
                    copyFile "test/caches/0.1.0.db" file
                    -- What 0.1.0's cache looked like before pull requests had
                    -- their head's owner.
                    withDatabase file $ \pool -> run pool (rawExecute "ALTER TABLE pull_requests DROP COLUMN head_owner" [])
                    withDatabase file $ \pool -> map ((.headOwner) . snd) <$> run pool pullRequests >>= (@?= [Nothing, Nothing, Nothing])
               , testCase "write a cache for this version, when asked" writeCache
               ]
        )

versions :: [String]
versions = unsafePerformIO (sort . map takeBaseName . filter (".db" `isSuffixOf`) <$> listDirectory "test/caches")
{-# NOINLINE versions #-}

{- | A copy of the version's cache opens, and holds the lists it was
written with.
-}
opens :: String -> IO ()
opens v = withSystemTempDirectory "usta-old" $ \dir -> do
    let
        file = dir </> "usta.db"
    copyFile ("test/caches" </> v <> ".db") file
    withDatabase file $ \pool -> do
        found <- run pool $ (,,,,) <$> (length <$> listAccounts) <*> (length <$> inbox) <*> (length <$> pullRequests) <*> (length <$> issues) <*> (length <$> repositories)
        found @?= (1, 4, 3, 4, 1)

writeCache :: IO ()
writeCache =
    lookupEnv "USTA_WRITE_CACHE"
        >>= mapM_
            ( \v -> do
                now <- getCurrentTime
                account <- githubAccount
                forge <- (.forge) <$> Sample.new now account
                let
                    none = Known {etag = Nothing, since = Nothing}
                    items fetch =
                        fetch >>= \case
                            Fetched {items = xs} -> pure xs
                            NotModified -> pure []
                ns <- items (forge.notifications none)
                prs <- items (forge.pullRequests none)
                is <- items (forge.issues none)
                rs <- items (forge.repositories none)
                withDatabase ("test/caches" </> v <> ".db") $ \pool -> run pool $ do
                    key <- saveAccount now account
                    saveNotifications key ns
                    savePullRequests key now prs
                    saveIssues key now is
                    saveRepositories key now rs
            )
