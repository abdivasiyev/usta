{-# LANGUAGE OverloadedStrings #-}

-- | Signing in and out: where the keyring, the cache, the forges and sync
-- meet. An account is in the cache only while its token is in the keyring.
module Usta.Accounts
  ( Settings (..)
  , Accounts (..)
  , Problem (..)
  , describe
  , connect
  ) where

import Control.Exception (try)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import qualified Network.HTTP.Client as HTTP
import Servant.Client (BaseUrl)

import Usta.Bus (Bus, Change (..), publish)
import Usta.DB (run)
import Usta.DB.Account (deleteAccount, listAccounts, saveAccount)
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (Failure (..))
import Usta.Forge.Forgejo (forgejo, whoAmI)
import Usta.Keyring (Keyring (..), KeyringError (..))
import Usta.Sync (State (..), Status (..), Sync (..))
import Usta.Types.Account (Account (..), AuthMethod (..))
import Usta.Types.Forge (Kind (..))
import Usta.Types.Host (Host)
import Usta.Types.Token (Token)

type Settings :: Type
data Settings = Settings
  { manager :: HTTP.Manager
  , keyring :: Keyring
  , pool :: ConnectionPool
  , changes :: Bus Change
  , sync :: Sync
  , forgejoApi :: Host -> BaseUrl
    -- ^ Where a Forgejo host's API is: @https://host/api/v1@.
  }

type Accounts :: Type
data Accounts = Accounts
  { signIn :: Kind -> Host -> Token -> IO (Either Problem AccountId)
    -- ^ Checks the token with the forge, keeps it in the keyring, saves
    -- the account and starts syncing it. Nothing is kept when a step fails.
  , remove :: AccountId -> IO (Either Problem ())
    -- ^ Stops syncing the account, forgets its token, and deletes it with
    -- everything cached for it.
  }

-- | Why signing in or out did not happen.
type Problem :: Type
data Problem
  = TokenRefused
  | Unreachable
  | ForgeSaid Text
  | NotYet Kind
    -- ^ This kind of forge has no client yet.
  | KeyringSaid Text
  deriving stock (Eq, Show)

-- | The problem as the dialog says it.
describe :: Problem -> Text
describe problem = case problem of
  TokenRefused -> "The forge refused this token. Check that it is current and has the scopes listed."
  Unreachable -> "The server could not be reached."
  ForgeSaid message -> "The forge said: " <> message
  NotYet GitHub -> "Signing in to GitHub comes with the GitHub client."
  NotYet GitLab -> "Signing in to GitLab comes with the GitLab client."
  NotYet Forgejo -> "Signing in to Forgejo is not available."
  KeyringSaid message -> "The keyring did not keep the token: " <> message

-- | Starts syncing every saved account with its token from the keyring,
-- and gives the way to sign in and out. An account whose token is gone
-- shows as signed out.
connect :: Settings -> IO Accounts
connect settings = do
  saved <- run settings.pool listAccounts
  for_ saved $ \(key, account) -> resume key account
  pure Accounts {signIn = signIn, remove = remove}
  where
    resume key account = case account.forge of
      Forgejo -> do
        found <- try (settings.keyring.load account.host account.login)
        case found of
          Right (Just token) -> start key account.host token
          Right Nothing -> settings.sync.report Status {accountId = key, state = Failed SignedOut}
          Left (KeyringError message) ->
            settings.sync.report Status {accountId = key, state = Failed (Refused message)}
      other -> settings.sync.report Status {accountId = key, state = Failed (Refused (describe (NotYet other)))}

    start key host token = do
      forge <- forgejo settings.manager (settings.forgejoApi host) host token
      settings.sync.add key forge

    signIn kind host token = case kind of
      Forgejo -> do
        checked <- try (whoAmI settings.manager (settings.forgejoApi host) token)
        case checked of
          Left SignedOut -> pure (Left TokenRefused)
          Left Offline -> pure (Left Unreachable)
          Left (Refused message) -> pure (Left (ForgeSaid message))
          Right login -> do
            kept <- try (settings.keyring.save host login token)
            case kept of
              Left (KeyringError message) -> pure (Left (KeyringSaid message))
              Right () -> do
                now <- getCurrentTime
                key <- run settings.pool $
                  saveAccount now Account {forge = kind, host = host, login = login, authMethod = AccessToken}
                start key host token
                pure (Right key)
      other -> pure (Left (NotYet other))

    -- The token goes first: if the keyring refuses, the account stays as
    -- it was, still syncing.
    remove key = do
      saved <- run settings.pool listAccounts
      case lookup key saved of
        Nothing -> pure (Right ())
        Just account -> do
          forgotten <- try (settings.keyring.forget account.host account.login)
          case forgotten of
            Left (KeyringError message) -> pure (Left (KeyringSaid message))
            Right () -> do
              settings.sync.remove key
              run settings.pool (deleteAccount key)
              for_ [minBound .. maxBound] $ \resource ->
                publish settings.changes Change {accountId = key, resource = resource}
              pure (Right ())
