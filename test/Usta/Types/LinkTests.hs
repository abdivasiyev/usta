{-# LANGUAGE OverloadedStrings #-}

-- | Forge links: what each kind of path opens.
module Usta.Types.LinkTests (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Usta.Types.Link (Target (..), describe, parseLink, splitPlace)

tests :: TestTree
tests =
  testGroup
    "Link"
    [ testGroup
        "parseLink"
        [ opens "https://git.azizovich.uz/abdivasiyev/usta/pulls/41" (PullAt "git.azizovich.uz" "abdivasiyev/usta" 41 False)
        , opens "https://github.com/abdivasiyev/usta/pull/7/files" (PullAt "github.com" "abdivasiyev/usta" 7 True)
        , opens "https://gitlab.com/group/proj/-/merge_requests/3/diffs" (PullAt "gitlab.com" "group/proj" 3 True)
        , opens "github.com/abdivasiyev/usta/issues/261" (IssueAt "github.com" "abdivasiyev/usta" 261)
        , opens "usta://localhost:3443/tester/made-by-usta/pulls/2" (PullAt "localhost:3443" "tester/made-by-usta" 2 False)
        , opens "https://github.com/abdivasiyev/usta" (RepositoryAt "github.com" "abdivasiyev/usta")
        , opens "https://github.com/abdivasiyev/usta.git" (RepositoryAt "github.com" "abdivasiyev/usta")
        , opens "https://github.com/abdivasiyev/usta/" (RepositoryAt "github.com" "abdivasiyev/usta")
        , opens "https://github.com/abdivasiyev/usta?tab=readme" (RepositoryAt "github.com" "abdivasiyev/usta")
        , opens "  https://github.com/abdivasiyev/usta/pull/7#issuecomment-1  " (PullAt "github.com" "abdivasiyev/usta" 7 False)
        , opens "https://github.com/abdivasiyev/usta/blob/master/src/Main.hs#L12" (FileAt "github.com" "abdivasiyev/usta" "master/src/Main.hs" (Just 12))
        , opens "https://github.com/abdivasiyev/usta/blob/master/src/Main.hs#L12-L20" (FileAt "github.com" "abdivasiyev/usta" "master/src/Main.hs" (Just 12))
        , opens "https://git.azizovich.uz/a/b/src/branch/feat/x/README.md" (FileAt "git.azizovich.uz" "a/b" "feat/x/README.md" Nothing)
        , opens "https://github.com/abdivasiyev/usta/actions" (RepositoryAt "github.com" "abdivasiyev/usta")
        , testCase "a host alone is no link" $ parseLink "https://github.com" @?= Nothing
        , testCase "an owner alone is no link" $ parseLink "https://github.com/abdivasiyev" @?= Nothing
        , testCase "empty text is no link" $ parseLink "" @?= Nothing
        , testProperty "any pull request path opens it" pullPaths
        ]
    , testGroup
        "describe"
        [ testCase "a pull request's Files tab" $ describe (PullAt "h" "o/r" 41 True) @?= "pull request #41, Files tab"
        , testCase "an issue" $ describe (IssueAt "h" "o/r" 3) @?= "issue #3"
        , testCase "a file's line" $ describe (FileAt "h" "o/r" "main/src/A.hs" (Just 9)) @?= "A.hs line 9 in o/r"
        , testCase "a repository" $ describe (RepositoryAt "h" "o/r") @?= "the repository o/r"
        ]
    , testGroup
        "splitPlace"
        [ testCase "the longest branch wins" $ splitPlace ["feat", "feat/x"] "feat/x/src/A.hs" @?= ("feat/x", "src/A.hs")
        , testCase "an unknown branch is the first part" $ splitPlace [] "main/src/A.hs" @?= ("main", "src/A.hs")
        , testCase "a branch that is only a prefix of a part" $ splitPlace ["fe"] "feat/A.hs" @?= ("feat", "A.hs")
        ]
    ]

opens :: Text -> Target -> TestTree
opens link target = testCase (T.unpack link) (parseLink link @?= Just target)

pullPaths :: Property
pullPaths = property $ do
  let segment = Gen.text (Range.linear 1 12) Gen.alphaNum
  host <- forAll segment
  owner <- forAll segment
  name <- forAll segment
  n <- forAll (Gen.int (Range.linear 1 100000))
  kind <- forAll (Gen.element ["pulls", "pull"])
  parseLink ("https://" <> host <> "/" <> owner <> "/" <> name <> "/" <> kind <> "/" <> T.pack (show n))
    === Just (PullAt host (owner <> "/" <> name) n False)
