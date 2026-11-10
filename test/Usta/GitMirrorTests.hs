{-# LANGUAGE OverloadedStrings #-}

{- | usta's git commands against a real repository and its mirror: a
temporary origin with commits, branches, a tag, a merge, a pull
request's ref, and files with spaces, accents and quotes in their names.
-}
module Usta.GitMirrorTests (tests, git, gitAt, gitOut, dayAfter) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (proc, readProcess_, setEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

import Usta.Types.Branch (Branch (..), Tag (..))
import Usta.Types.Commit (Commit (..))
import Usta.Types.Diff (FileDiff (..))
import Usta.Types.Insights (Blamed (..), Change (..), Node (..))
import Usta.Types.Parse (unparse)
import Usta.Types.Tree (Entry (..))

import Usta.Git qualified as Git
import Usta.Types.Tree qualified as Tree

tests :: TestTree
tests =
    testGroup
        "Git mirrors"
        [ testCase "a full mirror has every branch, tag and pull request" $ withMirror Git.Full $ \m -> do
            names <- map (.name) <$> Git.branches m "main"
            sort names @?= ["feature", "main"]
            map (.name) <$> Git.tags m >>= (@?= ["v1", "first-tag"])
            Git.resolve m "refs/pull/1/head" >>= assertBool "no pull request ref" . isJust
        , testCase "a partial mirror reads the same" $ withMirror Git.Partial $ \m ->
            map (.subject) <$> Git.recent m "main" 10 >>= (@?= ["Merge feature", "Main moves on", "Add feature", "Add files", "First"])
        , testCase "an unknown ref resolves to nothing" $ withMirror Git.Full $ \m ->
            Git.resolve m "no-such-branch" >>= (@?= Nothing) . fmap unparse
        , testCase "branches say how far they are from the base" $ withMirror Git.Full $ \m -> do
            found <- Git.branches m "main"
            [(b.name, b.ahead, b.behind) | b <- found, b.name == "main"] @?= [("main", Just 0, Just 0)]
        , testCase "a pull request's diff and commits" $ withMirror Git.Full $ \m -> do
            -- Main before the merge, as the pull request saw it.
            Just base <- Git.resolve m "main~1"
            Just tip <- Git.resolve m "refs/pull/1/head"
            files <- Git.diff m base tip
            map (.path) files @?= ["feature.txt"]
            map (.subject) <$> Git.history m base tip >>= (@?= ["Add feature"])
        , testCase "one commit's diff, and the first commit's" $ withMirror Git.Full $ \m -> do
            commits <- Git.recent m "main" 10
            let
                firstCommit = last commits
            map (.path) <$> Git.commitDiff m firstCommit.sha >>= (@?= ["README.md"])
            added <- Git.commitDiff m (commits !! 3).sha
            sort (map (.path) added) @?= sort ["café.txt", "my file.txt", "say \"hi\".txt", "src/A.hs", "logo.bin"]
        , testCase "history a page at a time" $ withMirror Git.Full $ \m -> do
            map (.subject) <$> Git.page m "main" 1 2 >>= (@?= ["Main moves on", "Add feature"])
            map (.subject) <$> Git.page m "main" 10 2 >>= (@?= [])
        , testCase "the files, with names as they are" $ withMirror Git.Full $ \m -> do
            entries <- Git.tree m "main"
            sort [e.path | e <- entries, e.kind == Tree.File] @?= sort ["README.md", "café.txt", "feature.txt", "logo.bin", "my file.txt", "say \"hi\".txt", "src/A.hs"]
            [e.path | e <- entries, e.kind == Tree.Folder] @?= ["src"]
            [e.size | e <- entries, e.path == "src/A.hs"] @?= [Just 15]
        , testCase "a file's contents, binary too" $ withMirror Git.Full $ \m -> do
            Git.blob m "main" "src/A.hs" >>= (@?= "module A where\n")
            BL.length <$> Git.blob m "main" "logo.bin" >>= (@?= 4)
            Git.blob m "main" "say \"hi\".txt" >>= (@?= "hi\n")
        , testCase "the README" $ withMirror Git.Full $ \m ->
            Git.readme m "main" >>= (@?= Just ("README.md", "# demo\nmore\n"))
        , testCase "blame names each line's commit" $ withMirror Git.Full $ \m ->
            map (\b -> (b.summary, b.line)) <$> Git.blame m "main" "README.md" >>= (@?= [("First", "# demo"), ("Main moves on", "more")])
        , testCase "a file's history" $ withMirror Git.Full $ \m ->
            map (.subject) <$> Git.fileLog m "main" "README.md" 10 >>= (@?= ["Main moves on", "First"])
        , testCase "who changed how much" $ withMirror Git.Full $ \m -> do
            found <- Git.changes m "main" Nothing
            length found @?= 4
            all ((== "Tester") . (.author)) found @?= True
        , testCase "the graph has the merge and its two parents" $ withMirror Git.Full $ \m -> do
            nodes <- Git.graph m [] False 0 50
            [length n.parents | n <- nodes, n.subject == "Merge feature"] @?= [2]
            any (elem "tag: v1" . (.refs)) nodes @?= True
        , testCase "a new commit on the forge comes with a fetch, and a deleted branch goes" $ withRepos $ \origin m -> do
            Git.clone Git.Full (T.pack origin) m
            git origin ["commit", "--allow-empty", "-q", "-m", "Later"]
            git origin ["branch", "-D", "-q", "feature"]
            Git.fetch m
            map (.subject) <$> Git.recent m "main" 1 >>= (@?= ["Later"])
            map (.name) <$> Git.branches m "main" >>= (@?= ["main"])
        , testCase "a pull request checked out on a branch of its own" $ withMirror Git.Full $ \m -> do
            let
                dir = m <> "-work"
            Git.worktree m dir "review" "refs/pull/1/head" "feature"
            doesDirectoryExist dir >>= assertBool "no worktree"
            out <- gitOut dir ["config", "branch.review.merge"]
            T.strip out @?= "refs/heads/feature"
        , testCase "a clone of one branch under another name" $ withRepos $ \origin m -> do
            Git.cloneBranch (T.pack origin) m "feature" "mine"
            T.strip <$> gitOut m ["rev-parse", "--abbrev-ref", "HEAD"] >>= (@?= "mine")
        , testCase "git's failure says why" $ withRepos $ \_ m -> do
            found <- tryGit (Git.clone Git.Full "/no/such/repository" m)
            assertBool "no message" (either (not . T.null) (const False) found)
        ]

-- | An origin made by 'build', and a place for its mirror.
withRepos :: (FilePath -> FilePath -> IO ()) -> IO ()
withRepos act = withSystemTempDirectory "usta-git" $ \dir -> do
    let
        origin = dir </> "origin"
    build origin
    act origin (dir </> "mirror.git")

withMirror :: Git.Kind -> (FilePath -> IO ()) -> IO ()
withMirror kind act = withRepos $ \origin m -> Git.clone kind (T.pack ("file://" <> origin)) m >> act m

{- | main: First (tagged first-tag), Add files, Main moves on, then a
merge of feature (Add feature); refs/pull/1/head is feature's tip;
v1 is tagged on the merge.
-}
build :: FilePath -> IO ()
build origin = do
    createDirectoryIfMissing True origin
    git origin ["init", "-q", "-b", "main"]
    write "README.md" "# demo\n" >> commit "First"
    git origin ["tag", "first-tag"]
    createDirectoryIfMissing True (origin </> "src")
    write "src/A.hs" "module A where\n"
    write "my file.txt" "spaces\n"
    write "café.txt" "accents\n"
    write "say \"hi\".txt" "hi\n"
    BL.writeFile (origin </> "logo.bin") (BL.pack [0, 1, 2, 255])
    commit "Add files"
    git origin ["checkout", "-q", "-b", "feature"]
    write "feature.txt" "new\n" >> commit "Add feature"
    git origin ["update-ref", "refs/pull/1/head", "feature"]
    git origin ["checkout", "-q", "main"]
    write "README.md" "# demo\nmore\n" >> commit "Main moves on"
    gitAt (dayAfter 5) origin ["merge", "-q", "--no-ff", "-m", "Merge feature", "feature"]
    git origin ["tag", "v1"]
    where
        write name text = writeFile (origin </> name) text
        -- A day apart, so history comes in the same order every time.
        commit message = do
            git origin ["add", "-A"]
            n <- length . T.lines <$> (gitOut origin ["rev-list", "--all"] `catch` \(_ :: SomeException) -> pure "")
            gitAt (dayAfter n) origin ["commit", "-q", "-m", message]

git :: FilePath -> [String] -> IO ()
git dir args = void (gitOut dir args)

-- | Day @n + 1@ of 2026, as git takes a date.
dayAfter :: Int -> String
dayAfter n = "2026-01-0" <> show (n + 1) <> "T12:00:00Z"

-- | Git with the commit dated @day@.
gitAt :: String -> FilePath -> [String] -> IO ()
gitAt day dir args = void (gitWith [("GIT_AUTHOR_DATE", day), ("GIT_COMMITTER_DATE", day)] dir args)

gitOut :: FilePath -> [String] -> IO Text
gitOut = gitWith []

gitWith :: [(String, String)] -> FilePath -> [String] -> IO Text
gitWith extra dir args = do
    inherited <- getEnvironment
    let
        identity =
            [ ("GIT_AUTHOR_NAME", "Tester")
            , ("GIT_AUTHOR_EMAIL", "tester@noreply.localhost")
            , ("GIT_COMMITTER_NAME", "Tester")
            , ("GIT_COMMITTER_EMAIL", "tester@noreply.localhost")
            , ("GIT_CONFIG_GLOBAL", "/dev/null")
            , ("GIT_CONFIG_NOSYSTEM", "1")
            ]
    (out, _) <- readProcess_ (setEnv (extra <> identity <> inherited) (proc "git" (["-C", dir] <> args)))
    pure (decodeUtf8 (BL.toStrict out))

tryGit :: IO () -> IO (Either Text ())
tryGit act = (Right <$> act) `catch` \(Git.GitError e) -> pure (Left e)
