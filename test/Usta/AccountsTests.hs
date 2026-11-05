{-# LANGUAGE OverloadedStrings #-}

-- | Signing in and out: the token checked with the forge, kept in the
-- keyring, and nothing kept when a step fails.
module Usta.AccountsTests (tests) where

import Control.Exception (bracket, throwIO)
import qualified Data.Text as T
import Database.Persist.Sql (ConnectionPool)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status401)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client (BaseUrl (..), Scheme (Http))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Usta.Accounts (Accounts (..), Problem (..))
import qualified Usta.Accounts as Accounts
import Usta.Bus (newBus)
import Usta.DB (run)
import Usta.DB.Account (listAccounts)
import Usta.Keyring (Keyring (..), KeyringError (..))
import qualified Usta.Keyring as Keyring
import qualified Usta.Forge.GitHub as GitHub
import qualified Usta.Mirrors as Mirrors
import qualified Usta.Sync as Sync
import Usta.SyncTests (withCache)
import Usta.Types.Account (Account (..))
import Usta.Types.Forge (Kind (..))
import Usta.Types.Host (Host)
import Usta.Types.Login (Login)
import Usta.Types.Parse (parseM, unparse)
import Usta.Types.Token (Token)

tests :: TestTree
tests =
  testGroup
    "Accounts"
    [ testCase "a token the forge takes is kept, and the account saved" $ withForge True $ \t -> do
        signed <- t.accounts.signIn Forgejo t.host t.token
        assertBool (show signed) (either (const False) (const True) signed)
        fmap unparse <$> (t.login >>= t.keyring.load t.host) >>= (@?= Just "token-for-tests")
        map (\(_, a) -> unparse a.login) <$> run t.pool listAccounts >>= (@?= ["tester"])
        either (const (pure ())) (\key -> t.accounts.forgeOf key >>= assertBool "no forge" . maybe False (const True)) signed
    , testCase "a token the forge refuses keeps nothing" $ withForge False $ \t -> do
        t.accounts.signIn Forgejo t.host t.token >>= (@?= Left TokenRefused) . either Left (const (Right ()))
        (t.login >>= t.keyring.load t.host) >>= (@?= Nothing) . fmap unparse
        length <$> run t.pool listAccounts >>= (@?= 0)
    , testCase "a forge out of reach keeps nothing" $ withForge True $ \t -> do
        away <- parseM "127.0.0.1:1"
        t.accounts.signIn Forgejo away t.token >>= (@?= Left Unreachable) . either Left (const (Right ()))
        length <$> run t.pool listAccounts >>= (@?= 0)
    , testCase "a keyring that refuses keeps no account" $ withForgeKeyring refusing True $ \t -> do
        t.accounts.signIn Forgejo t.host t.token >>= (@?= Left (KeyringSaid "locked")) . either Left (const (Right ()))
        length <$> run t.pool listAccounts >>= (@?= 0)
    , testCase "a forge usta has no client for" $ withForge True $ \t ->
        t.accounts.signIn GitLab t.host t.token >>= (@?= Left (NotYet GitLab)) . either Left (const (Right ()))
    , testCase "signing out forgets the token and the account" $ withForge True $ \t -> do
        Right key <- t.accounts.signIn Forgejo t.host t.token
        t.accounts.remove key >>= (@?= Right ())
        (t.login >>= t.keyring.load t.host) >>= (@?= Nothing) . fmap unparse
        length <$> run t.pool listAccounts >>= (@?= 0)
        t.accounts.forgeOf key >>= assertBool "still connected" . maybe True (const False)
    ]

data Running = Running
  { accounts :: Accounts
  , keyring :: Keyring
  , pool :: ConnectionPool
  , host :: Host
  , token :: Token
  , login :: IO Login
  }

refusing :: IO Keyring
refusing = do
  inner <- Keyring.memory
  pure inner {save = \_ _ _ -> throwIO (KeyringError "locked")}

withForge :: Bool -> (Running -> IO ()) -> IO ()
withForge = withForgeKeyring Keyring.memory

-- | A Forgejo that knows the token as tester's, or refuses it; a cache,
-- a sync and mirrors of their own.
withForgeKeyring :: IO Keyring -> Bool -> (Running -> IO ()) -> IO ()
withForgeKeyring makeKeyring takes test = do
  let app req respond = case Wai.pathInfo req of
        ["api", "v1", "user"] | takes -> respond (Wai.responseLBS status200 json "{\"login\":\"tester\"}")
        _ | not takes -> respond (Wai.responseLBS status401 json "{\"message\":\"token is required\"}")
        _ -> respond (Wai.responseLBS status200 json "[]")
      json = [("Content-Type", "application/json")]
  Warp.testWithApplication (pure app) $ \port -> withCache $ \pool -> withSystemTempDirectory "usta-mirrors" $ \root -> do
    manager <- newManager defaultManagerSettings
    keyring <- makeKeyring
    changes <- newBus
    statuses <- newBus
    mirrorStatuses <- newBus
    bracket (Sync.start Sync.Settings {pool = pool, changes = changes, statuses = statuses, every = 3600} []) (.stop) $ \sync -> do
      mirrors <- Mirrors.start Mirrors.Settings {pool = pool, changes = changes, statuses = mirrorStatuses, root = root </> "mirrors"}
      accounts <- Accounts.connect Accounts.Settings
        { manager = manager, keyring = keyring, pool = pool, changes = changes, sync = sync, mirrors = mirrors
        , forgejoApi = \h -> BaseUrl Http (T.unpack (T.takeWhile (/= ':') (unparse h))) (portOf h) "/api/v1"
        , githubApi = GitHub.urlsOf
        }
      host <- parseM ("127.0.0.1:" <> T.pack (show port))
      token <- parseM "token-for-tests"
      test Running {accounts = accounts, keyring = keyring, pool = pool, host = host, token = token, login = parseM "tester"}
  where
    portOf h = read (T.unpack (T.drop 1 (T.dropWhile (/= ':') (unparse h))))
