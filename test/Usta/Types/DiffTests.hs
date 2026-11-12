{-# LANGUAGE OverloadedStrings #-}

-- | Reading unified diffs, as git and Forgejo write them.
module Usta.Types.DiffTests (tests) where

import Data.Either (isLeft)
import Data.Text (Text)
import Hedgehog (Gen, Property, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Usta.Types.Diff (Change (..), FileDiff (..), Line (..), parseDiff)

tests :: TestTree
tests =
    testGroup
        "Diff"
        [ testCase "a modified file"
            $ parseDiff modified
                @?= Right
                    [ FileDiff
                        "src/A.hs"
                        Modified
                        False
                        [ Hunk "@@ -1,3 +1,3 @@ module A"
                        , Context 1 1 "a"
                        , Deletion 2 "b"
                        , Insertion 2 "B"
                        , Context 3 3 "c"
                        ]
                    ]
        , testCase "a new file"
            $ fmap (map (\f -> (f.path, f.change))) (parseDiff added) @?= Right [("new.txt", Added)]
        , testCase "a deleted file keeps its old path"
            $ fmap (map (\f -> (f.path, f.change))) (parseDiff deleted) @?= Right [("old.txt", Deleted)]
        , testCase "a pure rename"
            $ fmap (map (\f -> (f.path, f.change))) (parseDiff renamed) @?= Right [("b.txt", Renamed "a.txt")]
        , testCase "a binary file"
            $ fmap (map (\f -> (f.path, f.binary, f.lines))) (parseDiff binary) @?= Right [("logo.png", True, [])]
        , testCase "quoted paths"
            $ fmap (map (.path)) (parseDiff quoted) @?= Right ["with space.txt"]
        , testCase "a quoted path with escapes"
            $ fmap (map (.path)) (parseDiff escaped) @?= Right ["say \"hi\"\tcaf\233.txt"]
        , testCase "a quoted binary file, known only by its git line"
            $ fmap (map (.path)) (parseDiff quotedBinary) @?= Right ["my \"logo\".png"]
        , testCase "no newline at the end"
            $ fmap (map (length . (.lines))) (parseDiff noNewline) @?= Right [3]
        , testCase "several files"
            $ fmap (map (.path)) (parseDiff (modified <> added)) @?= Right ["src/A.hs", "new.txt"]
        , testCase "text before the first file"
            $ fmap (map (.path)) (parseDiff ("From abc\nSubject: x\n\n" <> modified)) @?= Right ["src/A.hs"]
        , testCase "nothing" $ parseDiff "" @?= Right []
        , testCase "a broken hunk header"
            $ assertBool "parsed" (isLeft (parseDiff "diff --git a/x b/x\n--- a/x\n+++ b/x\n@@ nonsense @@\n+a\n"))
        , testProperty "numbers lines as the hunk says" numbering
        ]

modified, added, deleted, renamed, binary, quoted, noNewline, escaped, quotedBinary :: Text
modified =
    T.unlines
        [ "diff --git a/src/A.hs b/src/A.hs"
        , "index 1111111..2222222 100644"
        , "--- a/src/A.hs"
        , "+++ b/src/A.hs"
        , "@@ -1,3 +1,3 @@ module A"
        , " a"
        , "-b"
        , "+B"
        , " c"
        ]
added =
    T.unlines
        ["diff --git a/new.txt b/new.txt", "new file mode 100644", "--- /dev/null", "+++ b/new.txt", "@@ -0,0 +1 @@", "+hello"]
deleted =
    T.unlines
        ["diff --git a/old.txt b/old.txt", "deleted file mode 100644", "--- a/old.txt", "+++ /dev/null", "@@ -1 +0,0 @@", "-bye"]
renamed =
    T.unlines ["diff --git a/a.txt b/b.txt", "similarity index 100%", "rename from a.txt", "rename to b.txt"]
binary =
    T.unlines ["diff --git a/logo.png b/logo.png", "index 1..2 100644", "Binary files a/logo.png and b/logo.png differ"]
quoted =
    T.unlines
        ["diff --git \"a/with space.txt\" \"b/with space.txt\"", "--- \"a/with space.txt\"", "+++ \"b/with space.txt\"", "@@ -1 +1 @@", "-a", "+b"]
escaped =
    T.unlines
        ["diff --git \"a/say \\\"hi\\\"\\tcaf\\303\\251.txt\" \"b/say \\\"hi\\\"\\tcaf\\303\\251.txt\"", "--- \"a/say \\\"hi\\\"\\tcaf\\303\\251.txt\"", "+++ \"b/say \\\"hi\\\"\\tcaf\\303\\251.txt\"", "@@ -1 +1 @@", "-a", "+b"]
quotedBinary =
    T.unlines ["diff --git \"a/my \\\"logo\\\".png\" \"b/my \\\"logo\\\".png\"", "index 1..2 100644", "Binary files differ"]
noNewline =
    T.unlines
        ["diff --git a/x b/x", "--- a/x", "+++ b/x", "@@ -1 +1 @@", "-a", "\\ No newline at end of file", "+b", "\\ No newline at end of file"]

{- | One hunk of any lines, written out and read back: each line keeps its
text, and the numbers count up from the hunk's start on each side.
-}
numbering :: Property
numbering = property $ do
    oldStart <- forAll (Gen.int (Range.linear 1 500))
    newStart <- forAll (Gen.int (Range.linear 1 500))
    body <- forAll (Gen.list (Range.linear 0 40) line)
    let
        header = "@@ -" <> num oldStart <> " +" <> num newStart <> " @@"
        text = T.unlines (["diff --git a/f b/f", "--- a/f", "+++ b/f", header] <> map fst body)
    fmap (map (.lines)) (parseDiff text) === Right [Hunk header : expected oldStart newStart (map snd body)]
    where
        num = T.pack . show
        line :: Gen (Text, (Char, Text))
        line = do
            kind <- Gen.element (" +-" :: String)
            t <- Gen.text (Range.linear 0 10) (Gen.element ("ab -+@\\" :: String))
            pure (T.cons kind t, (kind, t))
        expected _ _ [] = []
        expected o n ((kind, t) : rest) = case kind of
            '+' -> Insertion n t : expected o (n + 1) rest
            '-' -> Deletion o t : expected (o + 1) n rest
            _ -> Context o n t : expected (o + 1) (n + 1) rest
