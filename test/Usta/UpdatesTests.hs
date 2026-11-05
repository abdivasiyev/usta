{-# LANGUAGE OverloadedStrings #-}

-- | Reading GitHub's latest release.
module Usta.UpdatesTests (tests) where

import Data.Aeson (eitherDecode)
import Data.Either (isLeft)
import Data.Version (makeVersion)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Data.Version (Version, parseVersion)
import qualified Data.Text as T
import Text.ParserCombinators.ReadP (readP_to_S)

import Usta.Updates (Release (..), current, newer)

-- | The version of the usta under test.
running :: Version
running = last [v | (v, "") <- readP_to_S parseVersion (T.unpack current)]

tests :: TestTree
tests =
  testGroup
    "Updates"
    [ testCase "a release's tag is its version" $
        fmap (.version) (eitherDecode @Release "{\"tag_name\":\"v1.2.3\",\"html_url\":\"https://x\"}") @?= Right (makeVersion [1, 2, 3])
    , testCase "a tag without v" $
        fmap (.version) (eitherDecode @Release "{\"tag_name\":\"0.2.0\",\"html_url\":\"https://x\"}") @?= Right (makeVersion [0, 2, 0])
    , testCase "a tag that is no version" $
        assertBool "read" (isLeft (fmap (.version) (eitherDecode @Release "{\"tag_name\":\"nightly\",\"html_url\":\"https://x\"}")))
    , testCase "a newer version" $ newer (Release (makeVersion [99]) "") @?= True
    , testCase "this version" $ newer (Release running "") @?= False
    , testCase "an older version" $ newer (Release (makeVersion [0]) "") @?= False
    ]
