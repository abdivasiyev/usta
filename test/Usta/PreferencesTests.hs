{-# LANGUAGE OverloadedStrings #-}

-- | Settings on disk, and the ones fixed from Nix.
module Usta.PreferencesTests (tests) where

import Data.Aeson (eitherDecode, encode)
import Data.Either (isLeft)
import Hedgehog (Gen, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Usta.Preferences (Interval (..), Managed, Preferences (..), Theme (..), defaults, fixedKeys, keeping, load, over, save, seconds)

tests :: TestTree
tests =
  testGroup
    "Preferences"
    [ testProperty "survive JSON" (property (forAll preferences >>= \p -> tripping p encode eitherDecode))
    , testCase "an empty file is the defaults" $ eitherDecode "{}" @?= Right defaults
    , testCase "a value usta does not know is the default" $
        fmap (.theme) (eitherDecode @Preferences "{\"theme\":\"Purple\"}") @?= Right defaults.theme
    , testCase "the others are kept" $
        fmap (\p -> (p.theme, p.notify)) (eitherDecode @Preferences "{\"theme\":\"Purple\",\"notify\":false}") @?= Right (System, False)
    , testCase "a missing file is the defaults" $
        withSystemTempDirectory "usta" $ \dir -> load (dir </> "none.json") >>= (@?= defaults)
    , testCase "a broken file is the defaults" $
        withSystemTempDirectory "usta" $ \dir -> do
          writeFile (dir </> "p.json") "{not json"
          load (dir </> "p.json") >>= (@?= defaults)
    , testCase "saved and loaded" $
        withSystemTempDirectory "usta" $ \dir -> do
          let p = defaults {theme = Dark, interval = Manually, browser = Just "org.mozilla.firefox"}
          save (dir </> "a" </> "p.json") p
          load (dir </> "a" </> "p.json") >>= (@?= p)
    , testCase "every interval but by hand has seconds" $
        map seconds [minBound .. maxBound] @?= [Just 60, Just 120, Just 300, Just 900, Nothing]
    , testGroup
        "fixed from Nix"
        [ testCase "a name usta does not know is an error" $
            assertBool "read" (isLeft (eitherDecode @Managed "{\"theme\":\"Purple\"}"))
        , testCase "fixed keys" $ fmap fixedKeys (managed "{\"theme\":\"Dark\",\"notify\":false}") @?= Right ["theme", "notify"]
        , testCase "nothing fixed" $ fmap fixedKeys (managed "{}") @?= Right []
        , testCase "the fixed ones win" $
            fmap (\m -> (over m defaults).theme) (managed "{\"theme\":\"Dark\"}") @?= Right Dark
        , testCase "the user's file wins over the system's" $
            fmap (\m -> (over m defaults).theme) ((<>) <$> managed "{\"theme\":\"Dark\"}" <*> managed "{\"theme\":\"Light\"}") @?= Right Dark
        , testCase "a fixed choice keeps the user's own when saved" $
            fmap (\m -> (keeping m defaults defaults {theme = Dark, notify = False}))
              (managed "{\"theme\":\"Light\"}")
              @?= Right defaults {notify = False}
        , testProperty "over nothing changes nothing" $ property $ do
            p <- forAll preferences
            over mempty p === p
        ]
    ]
  where
    managed = eitherDecode @Managed

preferences :: Gen Preferences
preferences =
  Preferences
    <$> Gen.enumBounded
    <*> Gen.enumBounded
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.maybe (Gen.text (Range.linear 1 20) Gen.alphaNum)
