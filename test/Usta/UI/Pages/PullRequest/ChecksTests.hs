{-# LANGUAGE OverloadedStrings #-}

-- | Job logs: their kinds of lines, and their colors.
module Usta.UI.Pages.PullRequest.ChecksTests (tests) where

import qualified Data.Text as T
import Hedgehog (Property, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Usta.UI.Pages.PullRequest.Checks (LogLine (..), ansiMarkup, ansiPieces, parseLog)

tests :: TestTree
tests =
  testGroup
    "Checks"
    [ testGroup
        "parseLog"
        [ testCase "GitHub's markers" $
            parseLog False (T.unlines ["##[group]Run tests", "##[command]cabal test", "ok", "##[endgroup]", "##[error]boom", "##[warning]slow"])
              @?= [Group "Run tests", Command "cabal test", Output "ok", EndGroup, Failure "boom", Note "Warning: slow"]
        , testCase "times at the start are dropped" $
            parseLog False "2026-11-05T13:39:22.7386312Z hello" @?= [Output "hello"]
        , testCase "Forgejo's steps" $
            parseLog False (T.unlines ["⭐ Run Main nix flake check", "🏁 Job succeeded"]) @?= [Step "nix flake check", Note "🏁 Job succeeded"]
        , testCase "errors are marked only in a failed job" $
            (parseLog True "fatal: no", parseLog False "fatal: no") @?= ([Failure "fatal: no"], [Output "fatal: no"])
        , testCase "a byte order mark" $ parseLog False "\xFEFFhi" @?= [Output "hi"]
        ]
    , testGroup
        "ansiPieces"
        [ testCase "a color and a reset" $
            ansiPieces "a\ESC[31mred\ESC[0mb" @?= [((Nothing, False), "a"), ((Just "#F28B82", False), "red"), ((Nothing, False), "b")]
        , testCase "bold and color at once" $
            ansiPieces "\ESC[1;36mx" @?= [((Nothing, False), ""), ((Just "#78D9EC", True), "x")]
        , testCase "a 256-color is not red" $
            ansiPieces "\ESC[38;5;31mx" @?= [((Nothing, False), ""), ((Nothing, False), "x")]
        , testCase "an RGB color is not a code of its own" $
            ansiPieces "\ESC[38;2;1;32;3mx" @?= [((Nothing, False), ""), ((Nothing, False), "x")]
        , testCase "other escapes are dropped" $
            ansiPieces "\ESC[2Kx" @?= [((Nothing, False), ""), ((Nothing, False), "x")]
        ]
    , testGroup
        "ansiMarkup"
        [ testCase "colors become spans" $
            ansiMarkup "\ESC[1;31merror\ESC[0m" @?= "<span foreground=\"#F28B82\" weight=\"bold\">error</span>"
        , testCase "text is escaped" $ ansiMarkup "a < b && c > d" @?= "a &lt; b &amp;&amp; c &gt; d"
        , testProperty "the text reads the same without its markup" markupKeepsText
        ]
    ]

-- | Markup minus its spans, unescaped, is the text without its codes.
markupKeepsText :: Property
markupKeepsText = property $ do
  pieces <- forAll (Gen.list (Range.linear 0 8) ((,) <$> Gen.element codes <*> Gen.text (Range.linear 0 6) (Gen.element ("ab<>&\" " :: String))))
  let raw = T.concat [c <> t | (c, t) <- pieces]
  unmark (ansiMarkup raw) === T.concat (map snd pieces)
  where
    codes = ["", "\ESC[0m", "\ESC[31m", "\ESC[1;32m", "\ESC[38;5;208m", "\ESC[2K", "\ESC[m"]
    unmark m = case T.splitOn "<span" (T.replace "</span>" "" m) of
      first : spans -> unescape (T.concat (first : map (T.drop 1 . T.dropWhile (/= '>')) spans))
      [] -> ""
    unescape = T.replace "&amp;" "&" . T.replace "&lt;" "<" . T.replace "&gt;" ">"
