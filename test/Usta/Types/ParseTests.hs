{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The law every domain type keeps: @parse . unparse ≡ Right@, and the
-- raw values forges really send are accepted.
module Usta.Types.ParseTests (tests) where

import Control.Monad (forM_)
import Data.Either (isRight)
import Data.Text (Text)
import qualified Data.Text as Text
import Hedgehog (Gen, Property, assert, forAll, property, success, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Usta.DB.Schema.Notification (SubjectKind)
import qualified Usta.Git as Git
import Usta.Types.Account (AuthMethod)
import Usta.Types.Commit (Sha)
import Usta.Types.Count (Count)
import Usta.Types.ETag (ETag)
import Usta.Types.Forge (Kind)
import Usta.Types.Host (Host)
import Usta.Types.Involvement (Involvement)
import Usta.Types.Label (Color)
import Usta.Types.Login (Login)
import Usta.Types.Mirror (Choice (..))
import Usta.Types.Notification (Reason)
import Usta.Types.Number (Number)
import Usta.Types.Parse (Parse (..))
import Usta.Types.PullRequest (Checks, Review, State)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Resource (Resource)
import Usta.Types.ThreadId (ThreadId)
import Usta.Types.Token (Token)

tests :: TestTree
tests =
  testGroup
    "Parse"
    [ testGroup
        "parse . unparse ≡ Right"
        [ testProperty "Login" (roundTrips @Login anyText)
        , testProperty "RepoName" (roundTrips @RepoName anyText)
        , testProperty "Host" (roundTrips @Host anyText)
        , testProperty "Color" (roundTrips @Color anyText)
        , testProperty "Sha" (roundTrips @Sha anyText)
        , testProperty "Token, by its text" tokenRoundTrips
        , testProperty "ETag" (roundTrips @ETag anyText)
        , testProperty "ThreadId" (roundTrips @ThreadId anyText)
        , testProperty "Count" (roundTrips @Count anyInt)
        , testProperty "Number" (roundTrips @Number anyInt)
        ]
    , testGroup
        "every word"
        [ everyOf @AuthMethod "AuthMethod"
        , everyOf @Kind "Kind"
        , everyOf @Involvement "Involvement"
        , everyOf @State "State"
        , everyOf @Checks "Checks"
        , everyOf @Review "Review"
        , everyOf @Resource "Resource"
        , everyOf @Reason "Reason"
        , everyOf @SubjectKind "SubjectKind"
        , everyValue "Choice" (Declined : [Mirrored k t | k <- [Git.Full, Git.Partial], t <- [minBound .. maxBound]])
        ]
    , testGroup
        "accepts what forges send"
        [ testProperty "logins" (accepts @Login login)
        , testProperty "repositories" (accepts @RepoName ((\o n -> o <> "/" <> n) <$> segment <*> segment))
        , testProperty "hosts" (accepts @Host host)
        , testProperty "colors" (accepts @Color (Gen.text (Range.singleton 6) Gen.hexit))
        , testProperty "commit hashes" (accepts @Sha (Gen.text (Range.singleton 40) Gen.hexit))
        , testCase "a bot's login" $ assertAccepts @Login "github-actions[bot]"
        , testCase "a release by a bot" $ assertAccepts @Login "dependabot[bot]"
        , testCase "a host with a port" $ assertAccepts @Host "localhost:3443"
        , testCase "a color with #" $ assertAccepts @Color "#D73A4A"
        ]
    , testGroup
        "rejects"
        [ testCase "a login with a slash" $ assertRejects @Login "a/b"
        , testCase "a login of dots" $ assertRejects @Login ".."
        , testCase "a lone [bot]" $ assertRejects @Login "[bot]"
        , testCase "a host with a scheme" $ assertRejects @Host "https://github.com"
        , testCase "a port out of range" $ assertRejects @Host "example.com:70000"
        , testCase "a repository without an owner" $ assertRejects @RepoName "usta"
        , testCase "a token with a space" $ assertBool "accepted" (not (isRight (parse @Token "ghp_ a")))
        , testCase "number zero" $ assertRejects @Number 0
        , testCase "a negative count" $ assertRejects @Count (-1)
        ]
    ]

-- | Whatever the parser accepts comes back the same through 'unparse'.
roundTrips :: forall a. (Parse a, Eq a, Show a, Show (Raw a)) => Gen (Raw a) -> Property
roundTrips gen = property $ do
  raw <- forAll gen
  case parse @a raw of
    Left _ -> success
    Right a -> parse (unparse a) === Right a

-- | Tokens have no 'Show', so nothing prints one: compare their text.
tokenRoundTrips :: Property
tokenRoundTrips = property $ do
  raw <- forAll anyText
  case parse @Token raw of
    Left _ -> success
    Right token -> fmap unparse (parse @Token (unparse token)) === Right (unparse token)

accepts :: forall a. (Parse a, Show (Raw a)) => Gen (Raw a) -> Property
accepts gen = property $ do
  raw <- forAll gen
  assert (isRight (parse @a raw))

assertAccepts :: forall a. (Parse a, Show a) => Raw a -> IO ()
assertAccepts raw = assertBool (either show (const "") (parse @a raw)) (isRight (parse @a raw))

assertRejects :: forall a. (Parse a, Show a) => Raw a -> IO ()
assertRejects raw = assertBool (either (const "") show (parse @a raw)) (not (isRight (parse @a raw)))

-- | Every value of a type with fixed words parses back from its word.
everyValue :: forall a. (Parse a, Eq a, Show a) => String -> [a] -> TestTree
everyValue name values = testCase name $ forM_ values $ \v -> parse (unparse v) @?= Right v

everyOf :: forall a. (Parse a, Bounded a, Enum a, Eq a, Show a) => String -> TestTree
everyOf name = everyValue name [minBound .. maxBound :: a]

-- | Text built from the characters the parsers care about.
anyText :: Gen Text
anyText = Gen.text (Range.linear 0 24) (Gen.element ("abzAZ09._-/:[]# @\t" :: String))

anyInt :: Gen Int
anyInt = Gen.int (Range.linearFrom 0 (-1000) 1000)

-- | A login as GitHub and Forgejo allow them, bots included.
login :: Gen Text
login = do
  name <- segment
  bot <- Gen.bool
  pure (if bot then name <> "[bot]" else name)

-- | An owner, a repository's name or a person's login.
segment :: Gen Text
segment = Gen.text (Range.linear 1 20) (Gen.element (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_" :: String))

host :: Gen Text
host = do
  labels <- Gen.list (Range.linear 1 4) (Gen.text (Range.linear 1 10) Gen.lower)
  port <- Gen.maybe (Gen.int (Range.linear 1 65535))
  pure (Text.intercalate "." labels <> maybe "" ((":" <>) . Text.pack . show) port)
