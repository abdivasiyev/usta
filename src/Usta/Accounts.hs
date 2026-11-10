{-# LANGUAGE OverloadedStrings #-}

{- | Signing in and out, and choosing mirrors: where the keyring, the cache,
the forges, sync and the mirrors meet. An account is in the cache only
while its token is in the keyring.
-}
module Usta.Accounts
    ( Settings (..)
    , Accounts (..)
    , Origins (..)
    , Problem (..)
    , describe
    , connect
    , urlFor
    )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, tryPutMVar)
import Control.Exception (try)
import Control.Monad (void, when)
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import Servant.Client (BaseUrl (..), showBaseUrl)

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP

import Usta.Bus (Bus, Change (..), publish)
import Usta.DB (run)
import Usta.DB.Account (deleteAccount, listAccounts, saveAccount)
import Usta.DB.Mirror (choose)
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (CloneUrls (..), Failure (..), Forge (..))
import Usta.Forge.Forgejo (forgejo, siteOf, whoAmI)
import Usta.Forge.GitHub (github)
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

import Usta.Forge.GitHub qualified as GitHub
import Usta.OAuth qualified as OAuth

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
    , githubApi :: Host -> GitHub.Urls
    -- ^ Where a GitHub host's APIs are: 'GitHub.urlsOf'.
    }

type Accounts :: Type
data Accounts = Accounts
    { signIn :: Kind -> Host -> Token -> IO (Either Problem AccountId)
    {- ^ Checks the token with the forge, keeps it in the keyring, saves
    the account and starts syncing it. Nothing is kept when a step fails.
    -}
    , remove :: AccountId -> IO (Either Problem ())
    {- ^ Stops syncing the account, forgets its token, and deletes it with
    everything cached for it.
    -}
    , mirror :: AccountId -> RepoName -> Choice -> IO (Either Problem ())
    {- ^ Records the user's answer for a repository; for a mirror, asks the
    forge where to clone from and starts cloning.
    -}
    , origins :: AccountId -> RepoName -> IO (Either Problem Origins)
    {- ^ Where a mirror would clone from over each transport, for showing
    before the user chooses.
    -}
    , forgeOf :: AccountId -> IO (Maybe Forge)
    -- ^ The account's forge while it is connected.
    , beginBrowser :: Host -> Text -> IO (Either Text OAuth.Waiting)
    -- ^ Opens a browser sign-in with an OAuth2 client ID; on the main thread.
    , signInBrowser :: Kind -> Host -> Text -> OAuth.Tokens -> IO (Either Problem AccountId)
    -- ^ Like 'signIn', for what a browser sign-in gave.
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
    | -- | This kind of forge has no client yet.
      NotYet Kind
    | KeyringSaid Text
    | {- | The account has no forge: its token is gone, or its kind has no
      client.
      -}
      NotSignedIn
    | {- | The forge's clone URL is plain http, which would send the token
      unencrypted.
      -}
      NoHttps
    | NoSsh
    deriving stock (Eq, Show)

-- | The problem as the dialog says it.
describe :: Problem -> Text
describe problem = case problem of
    TokenRefused -> "The forge refused this token. Check that it is current and has the scopes listed."
    Unreachable -> "The server could not be reached."
    ForgeSaid message -> "The forge said: " <> message
    NotYet GitHub -> "Signing in to GitHub is not available."
    NotYet GitLab -> "Signing in to GitLab comes with the GitLab client."
    NotYet Forgejo -> "Signing in to Forgejo is not available."
    KeyringSaid message -> "The keyring did not keep the token: " <> message
    NotSignedIn -> "Sign in to this account again first."
    NoHttps -> "The forge gives no https address for this repository; use ssh instead."
    NoSsh -> "The forge has ssh turned off; use https instead."

{- | Starts syncing every saved account with its token from the keyring,
and gives the way to sign in and out. An account whose token is gone
shows as signed out.
-}
connect :: Settings -> IO Accounts
connect settings = do
    forges <- newIORef Map.empty
    saved <- run settings.pool listAccounts
    for_ saved $ \(key, account) -> resume forges key account
    pure
        Accounts
            { signIn = signIn forges
            , remove = remove forges
            , mirror = mirror forges
            , origins = origins forges
            , forgeOf = \key -> fmap snd . Map.lookup key <$> readIORef forges
            , beginBrowser = \host client -> OAuth.begin settings.manager (siteFor host) client
            , signInBrowser = signInBrowser forges
            }
    where
        resume forges key account = case account.forge of
            Forgejo -> do
                found <- try (settings.keyring.load account.host account.login)
                case found of
                    Right (Just token) -> case OAuth.stored token of
                        Just (client, r) ->
                            refreshing account.host account.login client r Nothing
                                >>= start forges key account.host account.login
                        Nothing -> start forges key account.host account.login (pure token)
                    Right Nothing -> settings.sync.report Status {accountId = key, state = Failed SignedOut}
                    Left (KeyringError message) ->
                        settings.sync.report Status {accountId = key, state = Failed (Refused message)}
            GitHub -> do
                found <- try (settings.keyring.load account.host account.login)
                case found of
                    Right (Just token) -> startGitHub forges key account.host account.login (pure token)
                    Right Nothing -> settings.sync.report Status {accountId = key, state = Failed SignedOut}
                    Left (KeyringError message) ->
                        settings.sync.report Status {accountId = key, state = Failed (Refused message)}
            other -> settings.sync.report Status {accountId = key, state = Failed (Refused (describe (NotYet other)))}

        start forges key host login token = forgejo settings.manager (settings.forgejoApi host) host token >>= running forges key login
        startGitHub forges key host login token = github settings.manager (settings.githubApi host) host token >>= running forges key login
        running forges key login forge = do
            atomicModifyIORef' forges (\m -> (Map.insert key (login, forge) m, ()))
            settings.sync.add key forge

        siteFor host = T.pack (showBaseUrl (settings.forgejoApi host) {baseUrlPath = siteOf (settings.forgejoApi host).baseUrlPath})

        -- A browser sign-in's access token, refreshed a few minutes before it
        -- runs out; until the first answer, calls wait for it. A new refresh
        -- token from the forge goes to the keyring.
        refreshing host login client r0 initial = do
            current <- newIORef (fmap (.access) initial)
            first <- newEmptyMVar
            let
                wait t = threadDelay (max 60 (t.seconds - 300) * 1000000)
                loop r = do
                    got <- OAuth.refresh settings.manager (siteFor host) client r
                    case got of
                        Right t -> do
                            writeIORef current (Just t.access)
                            when (unparse t.refreshToken /= unparse r)
                                $ void (try (settings.keyring.save host login (OAuth.storing client t.refreshToken)) :: IO (Either KeyringError ()))
                            void (tryPutMVar first ())
                            wait t
                            loop t.refreshToken
                        Left _ -> do
                            void (tryPutMVar first ())
                            threadDelay 30000000
                            loop r
            void $ forkIO $ case initial of
                Just t -> putMVar first () >> wait t >> loop t.refreshToken
                Nothing -> loop r0
            pure (readMVar first >> maybe r0 id <$> readIORef current)

        signInBrowser forges kind host client tokens = case kind of
            Forgejo -> do
                checked <- try (whoAmI settings.manager (settings.forgejoApi host) tokens.access)
                case checked of
                    Left SignedOut -> pure (Left TokenRefused)
                    Left Offline -> pure (Left Unreachable)
                    Left (Refused message) -> pure (Left (ForgeSaid message))
                    Right login -> do
                        kept <- try (settings.keyring.save host login (OAuth.storing client tokens.refreshToken))
                        case kept of
                            Left (KeyringError message) -> pure (Left (KeyringSaid message))
                            Right () -> do
                                now <- getCurrentTime
                                key <-
                                    run settings.pool
                                        $ saveAccount now Account {forge = kind, host = host, login = login, authMethod = BrowserSignIn}
                                refreshing host login client tokens.refreshToken (Just tokens) >>= start forges key host login
                                pure (Right key)
            other -> pure (Left (NotYet other))

        signIn forges kind host token = case kind of
            Forgejo -> signInWith (whoAmI settings.manager (settings.forgejoApi host) token) (start forges)
            GitHub -> signInWith (GitHub.whoAmI settings.manager (settings.githubApi host) token) (startGitHub forges)
            other -> pure (Left (NotYet other))
            where
                signInWith asking begin = do
                    checked <- try asking
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
                                    key <-
                                        run settings.pool
                                            $ saveAccount now Account {forge = kind, host = host, login = login, authMethod = AccessToken}
                                    begin key host login (pure token)
                                    pure (Right key)

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

{- | The URL a mirror fetches from. Over https it names the user, which
tells usta's credential helper whose token to give.
-}
urlFor :: Transport -> Login -> CloneUrls -> Either Problem Text
urlFor transport login urls = case transport of
    Https -> case T.stripPrefix "https://" urls.https of
        Just rest -> Right ("https://" <> unparse login <> "@" <> rest)
        Nothing -> Left NoHttps
    Ssh -> maybe (Left NoSsh) Right urls.ssh
