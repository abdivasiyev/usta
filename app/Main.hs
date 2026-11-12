{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, displayException, handle, throwIO)
import Control.Monad (forM_, forever, unless, void, when)
import Data.GI.Base
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import GHC.Conc (setUncaughtExceptionHandler)
import GI.Gtk.Objects.Application (Application (..))
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Data.Text qualified as T
import Data.Text.IO qualified as T
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import Usta.Bus (newBus)
import Usta.DB (databaseFile, run, withDatabase)
import Usta.DB.Account (listAccounts, saveAccount)
import Usta.DB.Schema.Account (AccountId)
import Usta.Forge (Forge)
import Usta.Forge.Http (apiUrl)
import Usta.Types.Account (Account (..), AuthMethod (..))
import Usta.Types.Forge (Kind (..))
import Usta.Types.Host (Host)
import Usta.Types.Parse (parseM, unparse)
import Usta.Types.Token (Token)
import Usta.UI.Source (Source)

import Usta.Accounts qualified as Accounts
import Usta.Details qualified as Details
import Usta.Forge.GitHub qualified as GitHub
import Usta.Forge.Sample qualified as Sample
import Usta.Git.Credential qualified as Credential
import Usta.Handoff qualified as Handoff
import Usta.Keyring qualified as Keyring
import Usta.Log qualified as Log
import Usta.Mirrors qualified as Mirrors
import Usta.OAuth qualified as OAuth
import Usta.Preferences qualified as Preferences
import Usta.Sync qualified as Sync
import Usta.Types.Link qualified as Link
import Usta.UI.Browser qualified as Browser
import Usta.UI.Gallery qualified as Gallery
import Usta.UI.Platform qualified as Platform
import Usta.UI.Source qualified as Source
import Usta.UI.TextKeys qualified as TextKeys
import Usta.UI.Window qualified as Window
import Usta.Updates qualified as Updates
import Usta.UrlEvents qualified as UrlEvents

{- | @usta --gallery@ opens the component gallery instead of the app;
@usta --welcome@ and @usta --first-sync@ preview the first-run states;
@usta --sample@ runs the whole app against sample forges;
@usta --version@ says which usta it is;
@usta git-credential get@ is git's credential helper for mirrors.
-}
main :: IO ()
main = getArgs >>= choose
    where
        choose args
            | ["git-credential", action] <- args = newTlsManager >>= \m -> Credential.helper Keyring.system (current m) action
            | has "--version" = T.putStrLn ("usta " <> Updates.current)
            | has "--gallery" = runApp (void . Gallery.build)
            | has "--welcome" = runApp (void . Window.buildAt Window.FirstRun Source.samples)
            | has "--first-sync" = runApp (void . Window.buildAt Window.FirstSync Source.samples)
            | has "--sample" = sample
            | otherwise = cache
            where
                has = (`elem` args)

-- | A browser sign-in keeps a refresh token; git gets a fresh access token.
current :: Manager -> Host -> Token -> IO (Maybe Token)
current manager host token = case OAuth.stored token of
    Nothing -> pure (Just token)
    Just (client, r) -> either (const Nothing) (Just . (.access)) <$> OAuth.refresh manager ("https://" <> unparse host) client r

runApp :: (Application -> IO ()) -> IO ()
runApp open = do
    app <-
        new
            Application
            [ #applicationId := "io.github.abdivasiyev.Usta"
            , On #startup TextKeys.everywhere
            , On #activate (open ?self)
            ]
    -- macOS's Quit (the Dock, the app menu, Cmd Q) asks GTK, which ends the
    -- process at once unless the app has a quit action; with one, run
    -- returns and usta stops as it should.
    quit <- Gio.simpleActionNew "quit" Nothing
    void $ on quit #activate (\_ -> #quit app)
    Gio.actionMapAddAction app quit
    Gtk.applicationSetAccelsForAction app "app.quit" [Platform.keys "<Primary>q"]
    void $ #run app Nothing

{- | The real cache, with every saved account syncing on its keyring
token. Only Forgejo accounts sync until the other forges get clients.
-}
cache :: IO ()
cache = do
    -- A second usta hands its links to the one running, and stops there.
    args <- getArgs
    let
        links = [T.pack a | a <- args, isJust (Link.parseLink (T.pack a))]
    handed <- Handoff.send links
    unless handed (running links)

{- | The first usta: the window, and the links given to it or to a later
usta.
-}
running :: [Text] -> IO ()
running links = do
    file <- databaseFile
    manager <- newTlsManager
    withDatabase file $ \pool -> do
        saved <- run pool listAccounts
        changes <- newBus
        statuses <- newBus
        chosen <- Preferences.effective
        sync <- Sync.start Sync.Settings {pool = pool, changes = changes, statuses = statuses, every = 60} []
        sync.setEvery (Preferences.seconds chosen.interval)
        root <- Mirrors.defaultRoot
        mirrorStatuses <- newBus
        mirrors <- Mirrors.start Mirrors.Settings {pool = pool, changes = changes, statuses = mirrorStatuses, root = root}
        accounts <-
            Accounts.connect
                Accounts.Settings
                    { manager = manager
                    , keyring = Keyring.system
                    , pool = pool
                    , changes = changes
                    , sync = sync
                    , mirrors = mirrors
                    , forgejoApi = \host -> apiUrl host "/api/v1"
                    , githubApi = GitHub.urlsOf
                    }
        let
            details = Details.new Details.Settings {pool = pool, accounts = accounts, mirrors = mirrors}
            source =
                Source.withDetails
                    pool
                    details
                    ( Source.withMirrors
                        pool
                        accounts
                        mirrors
                        mirrorStatuses
                        (Source.withAccounts pool accounts (Source.fromDatabase pool changes statuses sync))
                    )
            start = if null saved then Window.FirstRun else Window.Ready
        crashed <- Log.start
        -- A configuration usta cannot read fixes nothing; the log says why.
        Preferences.loadManaged >>= either (Log.say "config") (const (pure ()))
        -- An error no one caught goes to the log, from any thread.
        setUncaughtExceptionHandler (\e -> Log.say "error" (T.pack (displayException e)))
        -- A usta:// link from macOS waits until the window is there.
        queued <- newIORef []
        opener <- newIORef (\l -> modifyIORef' queued (<> [l]))
        UrlEvents.listen (\l -> readIORef opener >>= ($ l))
        handle (\(e :: SomeException) -> Log.say "error" (T.pack (displayException e)) >> throwIO e)
            $ runApp
            $ \app -> do
                built <- newIORef Nothing
                let
                    window =
                        readIORef built >>= \case
                            Just w -> pure w
                            Nothing -> do
                                w <- Window.buildWith start crashed source app
                                writeIORef built (Just w)
                                pure w
                (_, openLink) <- window
                forM_ links (void . openLink)
                -- With usta as the default browser every web link comes here: a
                -- forge's opens in usta, any other goes on to the browser.
                let
                    fromMacOS l = do
                        Log.say "link" ("from macOS: " <> l)
                        took <- openLink l
                        if took
                            then window >>= Gtk.windowPresent . fst
                            else when (any (`T.isPrefixOf` l) ["http://", "https://"]) (Browser.open l)
                readIORef queued >>= mapM_ fromMacOS
                writeIORef opener fromMacOS
                Handoff.listen (void . openLink) (window >>= Gtk.windowPresent . fst)
        Log.stop

{- | Two sample accounts in a fresh cache file, synced each minute. Every
90 seconds the sample forges change and sync runs at once, so the pages
can be seen updating. The cache is a file, not @:memory:@, since the pool
may close an idle connection and an in-memory database goes with it.
-}
sample :: IO ()
sample = do
    file <- (</> "usta-sample.db") <$> getTemporaryDirectory
    removePathForcibly file
    withDatabase file $ \pool -> do
        now <- getCurrentTime
        github <- Account GitHub <$> parseM "github.com" <*> parseM "abdivasiyev" <*> pure AccessToken
        forgejo <- Account Forgejo <$> parseM "git.azizovich.uz" <*> parseM "abdivasiyev" <*> pure AccessToken
        samples <-
            traverse
                ( \account -> do
                    key <- run pool (saveAccount now account)
                    s <- Sample.new now account
                    pure (key, s)
                )
                [github, forgejo]
        source <- fromPool pool [(key, s.forge) | (key, s) <- samples]
        void $ forkIO $ forever $ do
            threadDelay 90000000
            time <- getCurrentTime
            changed <- traverse (\(_, s) -> s.advance time) samples
            when (or changed) source.syncNow
        runApp (void . Window.build source)

-- | A source reading @pool@, with sync running for @forges@ every minute.
fromPool :: ConnectionPool -> [(AccountId, Forge)] -> IO Source
fromPool pool forges = do
    changes <- newBus
    statuses <- newBus
    sync <- Sync.start Sync.Settings {pool = pool, changes = changes, statuses = statuses, every = 60} forges
    pure (Source.fromDatabase pool changes statuses sync)
