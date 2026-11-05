{-# LANGUAGE OverloadedStrings #-}

-- | Which icon a file or folder gets.
module Usta.UI.Components.FileIconTests (tests) where

import Data.Aeson (Value (..), eitherDecodeFileStrict)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Foldable (toList)
import Data.List (nub, sort)
import qualified Data.Text as T
import Hedgehog (Property, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Directory (listDirectory)
import System.FilePath (dropExtension, takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Usta.UI.Components.FileIcon (fileIcon, folderIcon)

tests :: TestTree
tests =
  testGroup
    "FileIcon"
    [ testCase "by extension" $ fileIcon False "Main.hs" @?= "haskell"
    , testCase "by the longest extension first" $ fileIcon False "a.test.ts" @?= "test-ts"
    , testCase "by name before extension" $ fileIcon False "package.json" @?= "nodejs"
    , testCase "names in any case" $ fileIcon False "PACKAGE.JSON" @?= "nodejs"
    , testCase "a name the map writes in capitals" $ fileIcon False ".rhistory" @?= "r"
    , testCase "a light icon in the light theme" $ fileIcon True "Cargo.toml" @?= fileIcon True "x.toml"
    , testCase "the light toml icon" $ fileIcon True "x.toml" @?= "toml_light"
    , testCase "the dark toml icon" $ fileIcon False "x.toml" @?= "toml"
    , testCase "an unknown file" $ fileIcon False "notes.zzzz" @?= "file"
    , testCase "a whole path" $ (fileIcon False "src/Usta/Main.hs", folderIcon False False "app/src/") @?= ("haskell", "folder-src")
    , testCase "a folder" $ folderIcon False False "src" @?= "folder-src"
    , testCase "an open folder" $ folderIcon False True "src" @?= "folder-src-open"
    , testCase "an unknown folder" $ (folderIcon False False "zz", folderIcon False True "zz") @?= ("folder", "folder-open")
    , testCase "every icon the map names is drawn" everyIconDrawn
    , testProperty "every file gets an icon" anyFile
    ]

-- | The icons the map names all have an SVG beside it.
everyIconDrawn :: IO ()
everyIconDrawn = do
  svgs <- map dropExtension . filter ((== ".svg") . takeExtension) <$> listDirectory "data/file-icons"
  found <- eitherDecodeFileStrict "data/file-icons/map.json"
  case found of
    Left e -> assertFailure e
    Right v -> do
      let named = nub (sort ("file" : "folder" : "folder-open" : icons v))
      [n | n <- named, n `notElem` map T.pack svgs] @?= []
  where
    icons v = case v of
      String s -> [s]
      Object o -> concatMap icons (KeyMap.elems o)
      Array a -> concatMap icons (toList a)
      _ -> []

anyFile :: Property
anyFile = property $ do
  name <- forAll (Gen.text (Range.linear 0 30) (Gen.element ("abc./-_ ABC" :: String)))
  light <- forAll Gen.bool
  assert (not (T.null (fileIcon light name)))
