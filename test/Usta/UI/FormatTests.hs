{-# LANGUAGE OverloadedStrings #-}

-- | How times and numbers read on the pages.
module Usta.UI.FormatTests (tests) where

import Data.Time (UTCTime (..), addUTCTime, fromGregorian, utc)
import Hedgehog (Property, assert, forAll, property, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Data.Text qualified as T
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Usta.UI.Format (Clock (..), ago, date, short, thousands)

-- | Thursday 5 November 2026, noon, in UTC.
noon :: UTCTime
noon = UTCTime (fromGregorian 2026 11 5) (12 * 3600)

at :: Clock
at = Clock {now = noon, zone = utc}

before :: Double -> UTCTime
before s = addUTCTime (realToFrac (negate s)) noon

tests :: TestTree
tests =
    testGroup
        "Format"
        [ testGroup
            "ago"
            [ testCase "seconds" $ ago at (before 30) @?= "just now"
            , testCase "the future" $ ago at (before (-600)) @?= "just now"
            , testCase "one minute" $ ago at (before 60) @?= "1 min ago"
            , testCase "minutes" $ ago at (before (59 * 60)) @?= "59 min ago"
            , testCase "an hour" $ ago at (before 3600) @?= "1 hour ago"
            , testCase "hours" $ ago at (before (5 * 3600)) @?= "5 hours ago"
            , testCase "a day" $ ago at (before 86400) @?= "1 day ago"
            , testCase "a week is a date" $ ago at (before (7 * 86400)) @?= "Oct 29"
            ]
        , testGroup
            "short"
            [ testCase "now" $ short at (before 10) @?= "now"
            , testCase "minutes" $ short at (before 300) @?= "5m"
            , testCase "hours" $ short at (before (3 * 3600)) @?= "3h"
            , testCase "yesterday" $ short at (before (30 * 3600)) @?= "Yesterday"
            , testCase "this week" $ short at (before (3 * 86400)) @?= "Mon"
            , testCase "older" $ short at (before (20 * 86400)) @?= "Oct 16"
            ]
        , testGroup
            "date"
            [ testCase "this year" $ date at (UTCTime (fromGregorian 2026 1 2) 0) @?= "Jan 2"
            , testCase "another year" $ date at (UTCTime (fromGregorian 2025 12 31) 0) @?= "Dec 31, 2025"
            ]
        , testGroup
            "thousands"
            [ testCase "small" $ thousands 999 @?= "999"
            , testCase "a thousand" $ thousands 1373 @?= "1,373"
            , testCase "millions" $ thousands 1234567 @?= "1,234,567"
            , testCase "negative" $ thousands (-1000) @?= "-1,000"
            , testProperty "only adds commas" thousandsCommas
            ]
        , testProperty "ago never fails" agoTotal
        ]

thousandsCommas :: Property
thousandsCommas = property $ do
    n <- forAll (Gen.int (Range.linearFrom 0 (-10 ^ (12 :: Int)) (10 ^ (12 :: Int))))
    let
        written = thousands n
    T.filter (/= ',') written === T.pack (show n)
    assert (all ((<= 3) . T.length) (T.splitOn "," (T.dropWhile (== '-') written)))

agoTotal :: Property
agoTotal = property $ do
    s <- forAll (Gen.double (Range.linearFrac (-1e8) 1e9))
    assert (not (T.null (ago at (before s))))
