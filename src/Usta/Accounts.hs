{-# LANGUAGE OverloadedStrings #-}

-- | Signing in and out, and choosing mirrors: where the keyring, the cache,
-- the forges, sync and the mirrors meet. An account is in the cache only
-- while its token is in the keyring.
module Usta.Accounts
  ( Settings (..)
  , Accounts (..)
  , Origins (..)
  , Problem (..)
  , describe
  , connect
  , urlFor
  ) where

import Control.Exception (try)
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import qualified Network.HTTP.Client as HTTP
import Servant.Client (BaseUrl)

import Usta.Bus (Bus, Change (..), publish)
import Usta.DB (run)
import Usta.DB.Account (deleteAccount, listAccounts, saveAccount)
import Usta.DB.Mirror (choose)
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (CloneUrls (..), Failure (..), Forge (..))
import Usta.Forge.Forgejo (forgejo, whoAmI)
import Usta.Keyring (Keyring (..), KeyringError (..))
import Usta.Mirrors (Mirrors (..))
import Usta.Sync (State (..), Status (..), Sync (..))
import Usta.Types.Account (Account (..), AuthMethod (..))
import Usta.Types.Forge (Kind (..))
import Usta.Types.Host (Host)
import Usta.Types.Login (Login)
import Usta.Types.Mirror (Choice (..), Transport (..))
import Usta.Types.Parse (unparse)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Token (Token)

type Settings :: Type
data Settings = Settings
  { manager :: HTTP.Manager
  , keyring :: Keyring
  , pool :: ConnectionPool
  , changes :: Bus Change
  , sync :: Sync
  , mirrors :: Mirrors
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
  , mirror :: AccountId -> RepoName -> Choice -> IO (Either Problem ())
    -- ^ Records the user's answer for a repository; for a mirror, asks the
    -- forge where to clone from and starts cloning.
  , origins :: AccountId -> RepoName -> IO (Either Problem Origins)
    -- ^ Where a mirror would clone from over each transport, for showing
    -- before the user chooses.
  }

-- | The URL a mirror would use over each transport, or why it cannot.
type Origins :: Type
data Origins = Origins
  { https :: Either Problem Text
  , ssh :: Either Problem Text
  }
  deriving stock (Eq, Show)

-- | Why signing in or out did not happen.
type Problem :: Type
data Problem
  = TokenRefused
  | Unreachable
  | ForgeSaid Text
  | NotYet Kind
    -- ^ This kind of forge has no client yet.
  | KeyringSaid Text
  | NotSignedIn
    -- ^ The account has no forge: its token is gone, or its kind has no
    -- client.
  | NoHttps
    -- ^ The forge's clone URL is plain http, which would send the token
    -- unencrypted.
  | NoSsh
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
  NotSignedIn -> "Sign in to this account again first."
  NoHttps -> "The forge gives no https address for this repository; use ssh instead."
  NoSsh -> "The forge has ssh turned off; use https instead."

-- | Starts syncing every saved account with its token from the keyring,
-- and gives the way to sign in and out. An account whose token is gone
-- shows as signed out.
connect :: Settings -> IO Accounts
connect settings = do
  forges <- newIORef Map.empty
  saved <- run settings.pool listAccounts
  for_ saved $ \(key, account) -> resume forges key account
  pure Accounts {signIn = signIn forges, remove = remove forges, mirror = mirror forges, origins = origins forges}
  where
    resume forges key account = case account.forge of
      Forgejo -> do
        found <- try (settings.keyring.load account.host account.login)
        case found of
          Right (Just token) -> start forges key account.host account.login token
          Right Nothing -> settings.sync.report Status {accountId = key, state = Failed SignedOut}
          Left (KeyringError message) ->
            settings.sync.report Status {accountId = key, state = Failed (Refused message)}
      other -> settings.sync.report Status {accountId = key, state = Failed (Refused (describe (NotYet other)))}

    start forges key host login token = do
      forge <- forgejo settings.manager (settings.forgejoApi host) host token
      atomicModifyIORef' forges (\m -> (Map.insert key (login, forge) m, ()))
      settings.sync.add key forge

    signIn forges kind host token = case kind of
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
                start forges key host login token
                pure (Right key)
      other -> pure (Left (NotYet other))

    -- The token goes first: if the keyring refuses, the account stays as
    -- it was, still syncing.
    remove forges key = do
      saved <- run settings.pool listAccounts
      case lookup key saved of
        Nothing -> pure (Right ())
        Just account -> do
          forgotten <- try (settings.keyring.forget account.host account.login)
          case forgotten of
            Left (KeyringError message) -> pure (Left (KeyringSaid message))
            Right () -> do
              settings.sync.remove key
              atomicModifyIORef' forges (\m -> (Map.delete key m, ()))
              run settings.pool (deleteAccount key)
              for_ [minBound .. maxBound] $ \resource ->
                publish settings.changes Change {accountId = key, resource = resource}
              pure (Right ())

    -- Both URLs at once, from one call to the forge.
    origins forges key repo = do
      known <- Map.lookup key <$> readIORef forges
      case known of
        Nothing -> pure (Left NotSignedIn)
        Just (login, forge) -> do
          asked <- try (forge.cloneUrls repo)
          pure $ case asked of
            Left SignedOut -> Left TokenRefused
            Left Offline -> Left Unreachable
            Left (Refused message) -> Left (ForgeSaid message)
            Right urls -> Right Origins {https = urlFor Https login urls, ssh = urlFor Ssh login urls}

    mirror forges key repo choice = do
      now <- getCurrentTime
      case choice of
        Declined -> Right () <$ run settings.pool (choose now key repo Declined Nothing)
        Mirrored _ transport -> do
          found <- origins forges key repo
          case found >>= \o -> case transport of Https -> o.https; Ssh -> o.ssh of
            Left problem -> pure (Left problem)
            Right url -> do
              run settings.pool (choose now key repo choice (Just url))
              settings.mirrors.ensure key repo
              pure (Right ())

-- | The URL a mirror fetches from. Over https it names the user, which
-- tells usta's credential helper whose token to give.
urlFor :: Transport -> Login -> CloneUrls -> Either Problem Text
urlFor transport login urls = case transport of
  Https -> case T.stripPrefix "https://" urls.https of
    Just rest -> Right ("https://" <> unparse login <> "@" <> rest)
    Nothing -> Left NoHttps
  Ssh -> maybe (Left NoSsh) Right urls.ssh
