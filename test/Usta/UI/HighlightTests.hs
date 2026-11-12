{-# LANGUAGE OverloadedStrings #-}

{- | Syntax colors: the right language for a file, and markup that keeps
every line and its text.
-}
module Usta.UI.HighlightTests (tests) where

import Data.Text (Text)
import Hedgehog (Property, annotateShow, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Usta.UI.Highlight (Palette (..), languageFor, languageName, languageNamed, markup, plain)

tests :: TestTree
tests =
    testGroup
        "Highlight"
        [ testCase "by file name"
            $ map (fmap languageName . languageFor) ["src/Usta/Git.hs", "flake.nix", "a.json", "Makefile", "notes.txt"]
                @?= [Just "Haskell", Just "Nix", Just "JSON", Just "Makefile", Nothing]
        , testCase "by a fence's name"
            $ map (fmap languageName . languageNamed) ["haskell", " Haskell ", "sh", "nosuchlanguage"]
                @?= [Just "Haskell", Just "Haskell", Just "Bash", Nothing]
        , testCase "plain text is escaped" $ plain "a <b> & \"c\"" @?= "a &lt;b&gt; &amp; &quot;c&quot;"
        , testCase "a keyword is colored"
            $ markup LightPalette (languageNamed "haskell") "module A where"
                @?= ["<span foreground=\"#6D28D9\">module</span> <span foreground=\"#1F47A8\">A</span> <span foreground=\"#6D28D9\">where</span>"]
        , testCase "a file's last newline makes no line"
            $ (length (markup LightPalette (languageNamed "haskell") "a = 1\nb = 2\n"), length (markup LightPalette Nothing "a\nb\n")) @?= (2, 2)
        , testCase "without a language, only escaped" $ markup DarkPalette Nothing "a<b\nc" @?= ["a&lt;b", "c"]
        , testProperty "one line of markup per line, with its text" (keepsLines "haskell")
        , testProperty "the same in Nix" (keepsLines "nix")
        , testProperty "the same in JSON" (keepsLines "json")
        ]

{- | Whatever the code, the markup has its lines, and each reads the same
once its spans and escapes are taken out.
-}
keepsLines :: Text -> Property
keepsLines name = property $ do
    ls <- forAll (Gen.list (Range.linear 1 12) (Gen.text (Range.linear 0 30) (Gen.element ("ab =<>&\"'{}()[]-#/*\\:;,.0123 " :: String))))
    palette <- forAll (Gen.element [LightPalette, DarkPalette])
    let
        text = T.intercalate "\n" ls
        marked = markup palette (languageNamed name) text
    annotateShow marked
    map unmark marked === T.lines text
    where
        unmark m = case T.splitOn "<span" (T.replace "</span>" "" m) of
            first : spans -> unescape (T.concat (first : map (T.drop 1 . T.dropWhile (/= '>')) spans))
            [] -> ""
        unescape = T.replace "&amp;" "&" . T.replace "&lt;" "<" . T.replace "&gt;" ">" . T.replace "&quot;" "\""
