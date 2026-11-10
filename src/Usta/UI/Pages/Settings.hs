{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Settings: a section list on the left, every section on one scrolling
page on the right.
-}
module Usta.UI.Pages.Settings
    ( Auth (..)
    , Account (..)
    , Model (..)
    , sample
    , new
    , notice
    , confirm
    , css
    )
where

import Control.Concurrent (forkIO)
import Control.Monad (forM, forM_, unless, void, when)
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (findIndex)
import Data.Text (Text)

import Data.GI.Base qualified as GI
import Data.Text qualified as T
import GI.GLib qualified as GLib
import GI.Gtk qualified as Gtk

import Usta.Preferences (Interval (..), Preferences (..), Theme (..))
import Usta.UI.Style (Tone (..))

import Usta.Preferences qualified as Preferences
import Usta.Types.Link qualified as Link
import Usta.UI.Browser qualified as Browser
import Usta.UI.Components.Badge qualified as Badge
import Usta.UI.Components.Button qualified as Button
import Usta.UI.Components.Forge qualified as Forge
import Usta.UI.Components.NavItem qualified as NavItem
import Usta.UI.Components.Shortcut qualified as Shortcut
import Usta.UI.Platform qualified as Platform

-- | How usta signed in to the forge.
type Auth :: Type
data Auth = BrowserSignIn | AccessToken

type Account :: Type
data Account = Account
    { forge :: Forge.Kind
    , host :: Text
    , user :: Text
    , auth :: Auth
    , connected :: Bool
    , onRemove :: IO ()
    -- ^ Runs once the user confirmed removing the account.
    , onReauthenticate :: IO ()
    }

type Model :: Type
data Model = Model
    { accounts :: [Account]
    , preferences :: Preferences
    , onPreferences :: Preferences -> IO ()
    {- ^ Runs with every change of the sync interval, theme or
    notifications, which the caller saves and applies.
    -}
    , mirrorPath :: Text
    , onShowMirrors :: IO ()
    , onPrune :: IO (Either Text Int)
    , onClearCache :: IO (Either Text ())
    -- ^ Both run off the main thread, after the user confirmed.
    , shortcuts :: [(Text, Text)]
    -- ^ Keys and what they do, as the shortcut strip shows them.
    , canNotify :: Bool
    -- ^ 'False' when the desktop would not show usta's notifications.
    , fixed :: [Text]
    {- ^ The choices set in the Nix configuration, by their names in
    preferences.json; Settings shows them and does not change them.
    -}
    , version :: Text
    , onCheckUpdates :: IO (Either Text (Maybe (Text, IO ())))
    {- ^ Off the main thread: a newer version and what opens its release
    page, or 'Nothing' when this one is the latest.
    -}
    }

type Section :: Type
data Section = Accounts | Sync | Notifications | Links | Appearance | Updates | Keyboard
    deriving (Bounded, Enum, Eq)

sectionTitle :: Section -> Text
sectionTitle s = case s of
    Accounts -> "Accounts"
    Sync -> "Sync and storage"
    Notifications -> "Notifications"
    Links -> "Links"
    Appearance -> "Appearance"
    Updates -> "Updates"
    Keyboard -> "Keyboard"

-- | @onBack@ runs for Back; @onAddAccount@ for "Add account".
new :: IO () -> IO () -> Model -> IO Gtk.Widget
new onBack onAddAccount m = do
    page <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #hexpand := True
            , #cssClasses := ["u-settings"]
            ]

    bar <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationHorizontal
            , #spacing := 12
            , #cssClasses := ["settings-header"]
            ]
    back <-
        Button.new
            Button.defaults
                { Button.label = Just "Back"
                , Button.icon = Just "go-previous-symbolic"
                , Button.variant = Button.Flat
                }
    void $ GI.on back #clicked onBack
    #append bar back
    label "Settings" ["page-title"] >>= #append bar
    #append page bar

    content <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #spacing := 32
            , #widthRequest := 796
            , #halign := Gtk.AlignStart
            , #cssClasses := ["content"]
            ]
    -- Each change builds on the ones before it on this page.
    chosen <- newIORef m.preferences
    let
        change f = do
            p <- f <$> readIORef chosen
            writeIORef chosen p
            m.onPreferences p
    unless (null m.fixed) $ do
        note <- label "Some settings come from your Nix configuration (programs.usta.settings, in ~/.config/usta/config.json or /etc/xdg/usta/config.json). Change them there and rebuild." ["nix-note"]
        GI.set note [#wrap := True]
        #append content note
    sections <- forM [minBound .. maxBound] $ \s -> do
        widget <- section s m change onAddAccount
        #append content widget
        pure (s, widget)

    viewport <- GI.new Gtk.Viewport [#child := content]
    scroll <-
        GI.new
            Gtk.ScrolledWindow
            [ #child := viewport
            , #hexpand := True
            , #vexpand := True
            , #hscrollbarPolicy := Gtk.PolicyTypeNever
            ]

    nav <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #spacing := 2
            , #widthRequest := 240
            , #hexpand := False
            , #cssClasses := ["settings-nav"]
            ]
    items <- forM sections $ \(s, widget) -> do
        item <-
            NavItem.new
                NavItem.defaults
                    { NavItem.label = sectionTitle s
                    , NavItem.active = s == minBound
                    }
        #append nav item
        pure (item, widget)
    forM_ items $ \(item, widget) ->
        void $ GI.on item #clicked $ do
            forM_ items $ \(other, _) -> Gtk.widgetRemoveCssClass other "active"
            Gtk.widgetAddCssClass item "active"
            Gtk.viewportScrollTo viewport widget Nothing

    body <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #vexpand := True]
    #append body nav
    #append body scroll
    #append page body
    Gtk.toWidget page

section :: Section -> Model -> ((Preferences -> Preferences) -> IO ()) -> IO () -> IO Gtk.Widget
section s m change onAddAccount = do
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12]
    heading <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    title <- label (sectionTitle s) ["section-title"]
    GI.set title [#hexpand := True, #valign := Gtk.AlignCenter]
    #append heading title
    when (s == Accounts) $ do
        add <-
            Button.new
                Button.defaults
                    { Button.label = Just "Add account"
                    , Button.variant = Button.Primary
                    }
        void $ GI.on add #clicked onAddAccount
        #append heading add
    #append box heading

    card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-card"]]
    rows <- case s of
        Accounts
            | null m.accounts ->
                sequence [setting "No accounts yet" [plain "Add one to start syncing its pull requests, issues and notifications"] []]
            | otherwise -> mapM accountRow m.accounts
        Sync ->
            sequence
                [ do
                    let
                        intervals = [minBound .. maxBound] :: [Interval]
                    interval <- Gtk.dropDownNewFromStrings (map intervalName intervals)
                    Gtk.dropDownSetSelected interval (fromIntegral (fromEnum m.preferences.interval))
                    void $ GI.on interval (GI.PropertyNotify #selected) $ \_ -> do
                        i <- fromIntegral <$> Gtk.dropDownGetSelected interval
                        when (i < length intervals) $ change (\p -> p {interval = intervals !! i})
                    setting' "interval" "Sync forges" [plain "How often usta asks your forges for new activity"]
                        =<< sequence [Gtk.toWidget interval]
                , do
                    folder <- button "Show in folder" Button.Secondary (const m.onShowMirrors)
                    prune <- button "Prune unused" Button.Secondary $ \anchor ->
                        confirm
                            anchor
                            "Prune unused mirrors?"
                            "Deletes the clones of repositories that no longer have a mirror, such as a removed account's."
                            "Prune"
                            $ offThread m.onPrune
                            $ \result -> case result of
                                Right 0 -> notice anchor "Nothing to prune" "Every clone belongs to a mirror you chose."
                                Right n -> notice anchor "Mirrors pruned" (T.pack (show n) <> (if n == 1 then " unused clone was" else " unused clones were") <> " deleted.")
                                Left text -> notice anchor "The mirrors were not pruned" text
                    setting "Local git mirrors" [plain "Diffs are computed from bare clones in ", code m.mirrorPath] [folder, prune]
                , do
                    clear <- button "Clear cache" Button.Danger $ \anchor ->
                        confirm
                            anchor
                            "Clear the cache?"
                            "Everything synced is fetched again from your forges. Notifications marked done come back to the inbox."
                            "Clear cache"
                            $ offThread m.onClearCache
                            $ either (notice anchor "The cache was not cleared") (const (pure ()))
                    setting "Offline cache" [plain "Pull requests, issues and comments stored in a local database"] [clear]
                ]
        Notifications ->
            sequence
                [ setting'
                    "notify"
                    "Desktop notifications"
                    [ plain
                        ( if m.canNotify
                            then "Review requests, mentions and failed checks"
                            else "On macOS they show once usta is installed as an app; this copy runs outside one"
                        )
                    ]
                    =<< sequence [switch m.preferences.notify (\on p -> p {notify = on})]
                , setting' "onlyInBackground" "Only when usta is in the background" [plain "Stay quiet while you are looking at the app"]
                    =<< sequence [switch m.preferences.onlyInBackground (\on p -> p {onlyInBackground = on})]
                ]
        Links -> do
            opening <-
                setting'
                    "openLinks"
                    "Open forge links in usta"
                    [plain "Links to your forges in comments, in the search or given as ", code "usta <link>", plain " open the pull request, issue or repository here; every other link goes to the browser"]
                    =<< sequence [switch m.preferences.openLinks (\on p -> p {openLinks = on})]
            others <-
                if Platform.current /= Platform.MacOS
                    then setting "Other links open in" [plain "The system's default browser"] []
                    else do
                        found <- Browser.browsers
                        pick <- Gtk.dropDownNewFromStrings ("The system's default" : map (.name) found)
                        Gtk.dropDownSetSelected pick (maybe 0 (\b -> maybe 0 (fromIntegral . (+ 1)) (findIndex ((== b) . (.bundle)) found)) m.preferences.browser)
                        void $ GI.on pick (GI.PropertyNotify #selected) $ \_ -> do
                            i <- fromIntegral <$> Gtk.dropDownGetSelected pick
                            change (\p -> p {browser = if i == 0 || i > length found then Nothing else Just (found !! (i - 1)).bundle})
                        setting'
                            "browser"
                            "Other links open in"
                            [plain "Web links that are not your forges'. The system's default is Safari while usta is the default browser"]
                            =<< sequence [Gtk.toWidget pick]
            -- As the default browser usta gets every web link from other apps.
            everywhere <-
                if Platform.current /= Platform.MacOS
                    then setting "Links from other apps" [plain "Give usta a link: ", code "usta <link>"] []
                    else do
                        said <- label "" ["muted"]
                        make <- Button.new Button.defaults {Button.label = Just "Make usta the default browser"}
                        let
                            showState = do
                                now <- Browser.isDefault
                                GI.set said [#label := if now then "\10003 usta is the default browser" else "", #visible := now]
                                GI.set make [#visible := not now]
                        showState
                        void $ GI.on make #clicked $ do
                            Browser.makeDefault
                            -- macOS asks first; its answer shows a moment later.
                            void $ GLib.timeoutAddSeconds GLib.PRIORITY_DEFAULT 3 (showState >> pure False)
                        setting
                            "Forge links from other apps"
                            [plain "As the default web browser, usta opens links to your forges from any app and sends every other link on to the browser above"]
                            =<< sequence [Gtk.toWidget said, Gtk.toWidget make]
            entry <- GI.new Gtk.Entry [#placeholderText := "Paste a link", #widthRequest := 320]
            trying <- setting "Try a link" [plain "Paste one to see where it would open"] =<< sequence [Gtk.toWidget entry]
            answer <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8, #visible := False, #cssClasses := ["setting", "answer"]]
            mark <- GI.new Gtk.Image [#iconName := "object-select-symbolic"]
            said <- label "" []
            mapM_ (#append answer) =<< sequence [Gtk.toWidget mark, Gtk.toWidget said]
            let
                hosts = [a.host | a <- m.accounts]
            void $ GI.on entry #changed $ do
                t <- T.strip <$> Gtk.editableGetText entry
                let
                    where' = case Link.parseLink t of
                        Just target | target.host `elem` hosts -> Just ("Opens in usta: " <> Link.describe target)
                        Just target -> Just ("Opens in the browser: " <> target.host <> " is not one of your forges")
                        Nothing -> Nothing
                GI.set answer [#visible := not (T.null t)]
                GI.set mark [#visible := maybe False ("Opens in usta" `T.isPrefixOf`) where']
                GI.set said [#label := maybe "Opens in the browser: not a forge link" id where']
            sequence [pure opening, pure everywhere, pure others, pure trying, Gtk.toWidget answer]
        Appearance ->
            sequence
                [ setting' "theme" "Theme" [] =<< sequence [themeChoice m.preferences.theme (\t -> change (\p -> p {theme = t}))]
                ]
        Updates -> do
            checking <-
                setting'
                    "checkUpdates"
                    "Check for updates"
                    [plain "Once a day usta asks GitHub for its latest release. Nothing about you or your forges is sent"]
                    =<< sequence [switch m.preferences.checkUpdates (\on p -> p {checkUpdates = on})]
            said <- label "" ["muted"]
            now <- Button.new Button.defaults {Button.label = Just "Check now"}
            opening <- Button.new Button.defaults {Button.label = Just "Open the release", Button.variant = Button.Primary}
            GI.set opening [#visible := False]
            void $ GI.on now #clicked $ Button.busy now ([] :: [Gtk.Widget]) "Checking\8230" m.onCheckUpdates $ \case
                Left problem -> GI.set said [#label := "Could not check: " <> problem]
                Right Nothing -> GI.set said [#label := "\10003 This is the latest"]
                Right (Just (v, open)) -> do
                    GI.set said [#label := "usta " <> v <> " is out"]
                    GI.set opening [#visible := True]
                    void $ GI.on opening #clicked open
            versionRow <- setting "Version" [code ("usta " <> m.version)] =<< sequence [Gtk.toWidget said, Gtk.toWidget now, Gtk.toWidget opening]
            pure [checking, versionRow]
        Keyboard -> forM m.shortcuts $ \(keys, action) -> do
            keyLabel <- Shortcut.key keys >>= Gtk.toWidget
            setting (capitalize action) [] [keyLabel]
    forM_ rows $ #append card
    #append box card
    Gtk.toWidget box
    where
        -- A row for a choice the Nix configuration may fix.
        setting' key = locked (key `elem` m.fixed)
        capitalize t = T.toUpper (T.take 1 t) <> T.drop 1 t
        switch on set' = do
            w <- GI.new Gtk.Switch [#active := on]
            void $ GI.on w (GI.PropertyNotify #active) $ \_ -> do
                now <- GI.get w #active
                change (set' now)
            Gtk.toWidget w
        intervalName i = case i of
            EveryMinute -> "Every minute"
            EveryTwoMinutes -> "Every 2 minutes"
            EveryFiveMinutes -> "Every 5 minutes"
            EveryFifteenMinutes -> "Every 15 minutes"
            Manually -> "Manually"

-- | A piece of a row's description, in the normal or the code font.
type Piece :: Type
data Piece = Piece Text Bool

plain, code :: Text -> Piece
plain t = Piece t False
code t = Piece t True

-- | A row: a title with an optional description, controls on the right.
setting :: Text -> [Piece] -> [Gtk.Widget] -> IO Gtk.Widget
setting = locked False

{- | A row whose choice, when @fixed@, comes from the Nix configuration:
a "Set in Nix" pill, and its controls dimmed.
-}
locked :: Bool -> Text -> [Piece] -> [Gtk.Widget] -> IO Gtk.Widget
locked fixed' title pieces controls = do
    row <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationHorizontal
            , #spacing := 12
            , #cssClasses := ["setting"]
            ]
    texts <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationVertical
            , #spacing := 2
            , #hexpand := True
            , #valign := Gtk.AlignCenter
            ]
    heading <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    label title ["title"] >>= #append heading
    when fixed' $ do
        pill <- label "Set in Nix" ["nix-pill"]
        GI.set pill [#tooltipText := "programs.usta.settings in your Nix configuration"]
        #append heading pill
    #append texts heading
    when (not (null pieces)) $ description pieces >>= #append texts
    #append row texts
    forM_ controls $ \c -> do
        GI.set c [#valign := Gtk.AlignCenter]
        when fixed' $ GI.set c [#sensitive := False, #tooltipText := "Set in your Nix configuration"]
        #append row c
    Gtk.toWidget row

description :: [Piece] -> IO Gtk.Box
description pieces = do
    line <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    forM_ pieces $ \(Piece text mono) ->
        label text (["description"] <> ["mono" | mono]) >>= #append line
    pure line

accountRow :: Account -> IO Gtk.Widget
accountRow a = do
    tile <- Forge.tile a.forge
    status <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    if a.connected
        then do
            Badge.dot Success >>= #append status
            label "Connected" ["connected"] >>= #append status
        else do
            Badge.dot Danger >>= #append status
            label "Signed out" ["signed-out"] >>= #append status
    reauthenticate <- Button.new Button.defaults {Button.label = Just "Re-authenticate", Button.variant = Button.Secondary}
    void $ GI.on reauthenticate #clicked a.onReauthenticate
    remove <- Button.new Button.defaults {Button.label = Just "Remove", Button.variant = Button.Danger}
    void $ GI.on remove #clicked $ do
        anchor <- Gtk.toWidget remove
        confirm
            anchor
            ("Remove " <> a.user <> "@" <> a.host <> "?")
            "Its cached pull requests, issues and notifications are deleted, and its token is removed from the keyring."
            "Remove"
            a.onRemove
    controls <- sequence [Gtk.toWidget status, Gtk.toWidget reauthenticate, Gtk.toWidget remove]
    row <-
        setting
            a.host
            [plain "Signed in as ", code a.user, plain (" · " <> authText a.auth)]
            controls
    -- The tile goes before the texts.
    box <- GI.castTo Gtk.Box row
    forM_ box $ \b -> Gtk.boxPrepend b tile
    pure row
    where
        authText BrowserSignIn = "Browser sign-in"
        authText AccessToken = "Access token"

{- | System, Light or Dark; switches the app's colors right away.
| System, Light and Dark; @onTheme@ runs with the one chosen.
-}
themeChoice :: Theme -> (Theme -> IO ()) -> IO Gtk.Widget
themeChoice current onTheme = do
    box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 16]
    buttons <- forM [minBound .. maxBound] $ \theme -> do
        b <- GI.new Gtk.CheckButton [#label := T.pack (show theme), #active := theme == current]
        void $ GI.on b #toggled $ do
            active <- GI.get b #active
            when active (onTheme theme)
        #append box b
        pure b
    case buttons of
        first : rest -> forM_ rest $ \b -> Gtk.checkButtonSetGroup b (Just first)
        [] -> pure ()
    Gtk.toWidget box

{- | A small modal window over @anchor@'s window: @title@, @text@, Cancel
and a danger button that runs @onConfirm@.
-}
confirm :: Gtk.Widget -> Text -> Text -> Text -> IO () -> IO ()
confirm anchor title text action onConfirm = do
    (dialog, footer) <- modal anchor title text
    cancel <- Button.new Button.defaults {Button.label = Just "Cancel"}
    void $ GI.on cancel #clicked (Gtk.windowDestroy dialog)
    go <- Button.new Button.defaults {Button.label = Just action, Button.variant = Button.Danger}
    void $ GI.on go #clicked (Gtk.windowDestroy dialog >> onConfirm)
    #append footer cancel
    #append footer go
    #present dialog

-- | Tells the user something went wrong, with an OK button.
notice :: Gtk.Widget -> Text -> Text -> IO ()
notice anchor title text = do
    (dialog, footer) <- modal anchor title text
    ok <- Button.new Button.defaults {Button.label = Just "OK", Button.variant = Button.Primary}
    void $ GI.on ok #clicked (Gtk.windowDestroy dialog)
    #append footer ok
    #present dialog

-- | The window and its empty button row.
modal :: Gtk.Widget -> Text -> Text -> IO (Gtk.Window, Gtk.Box)
modal anchor title text = do
    root <- Gtk.widgetGetRoot anchor
    parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root
    dialog <-
        GI.new
            Gtk.Window
            [ #title := title
            , #modal := True
            , #resizable := False
            , #destroyWithParent := True
            , #defaultWidth := 420
            , #cssClasses := ["usta", "u-confirm"]
            ]
    Platform.dialogOver dialog parent
    body <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12, #cssClasses := ["body"]]
    label title ["title"] >>= #append body
    message <- label text ["muted"]
    GI.set message [#wrap := True]
    #append body message
    footer <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationHorizontal
            , #spacing := 8
            , #halign := Gtk.AlignEnd
            ]
    #append body footer
    GI.set dialog [#child := body]
    pure (dialog, footer)

-- | @onClick@ gets the button, for a dialog over its window.
button :: Text -> Button.Variant -> (Gtk.Widget -> IO ()) -> IO Gtk.Widget
button text variant onClick = do
    b <- Button.new Button.defaults {Button.label = Just text, Button.variant = variant}
    widget <- Gtk.toWidget b
    void $ GI.on b #clicked (onClick widget)
    pure widget

-- | Runs @act@ off the main thread, then @done@ with its result on it.
offThread :: IO a -> (a -> IO ()) -> IO ()
offThread act done = void $ forkIO $ do
    result <- act
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT (done result >> pure False)

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [(Text, Text)] -> Model
sample keys =
    Model
        { accounts =
            [ account Forge.GitHub "github.com" BrowserSignIn
            , account Forge.Forgejo "git.azizovich.uz" AccessToken
            ]
        , preferences = Preferences.defaults
        , onPreferences = const (pure ())
        , mirrorPath = "~/Library/Caches/usta/mirrors"
        , onShowMirrors = pure ()
        , onPrune = pure (Right 0)
        , onClearCache = pure (Right ())
        , shortcuts = keys
        , canNotify = True
        , fixed = []
        , version = "0.1.0"
        , onCheckUpdates = pure (Right Nothing)
        }
    where
        account kind host auth =
            Account
                { forge = kind
                , host = host
                , user = "abdivasiyev"
                , auth = auth
                , connected = True
                , onRemove = pure ()
                , onReauthenticate = pure ()
                }

css :: Text
css =
    mconcat
        [ "window.u-confirm box.body { padding: 20px; }"
        , "window.u-confirm label.title { font-size: 15px; font-weight: 600; }"
        , "box.u-settings box.settings-header {"
        , "  padding: 8px 16px; background: var(--surface); border-bottom: 1px solid var(--border);"
        , "}"
        , "box.u-settings label.page-title { font-size: 17px; font-weight: 600; }"
        , "box.u-settings box.settings-nav {"
        , "  padding: 16px 12px; background: var(--sidebar); border-right: 1px solid var(--border);"
        , "}"
        , "box.u-settings box.content { padding: 24px 32px 48px; }"
        , "box.u-settings label.section-title { font-size: 19px; font-weight: 600; }"
        , "box.u-settings box.setting > :disabled { opacity: 0.45; }"
        , "box.u-settings label.nix-pill { padding: 1px 8px; border-radius: 10px; background: var(--border-subtle); color: var(--text-soft); font-size: 12px; font-weight: 500; }"
        , "box.u-settings label.nix-note { padding: 10px 14px; border: 1px solid var(--border); border-radius: 8px; background: var(--window); color: var(--text-soft); font-size: 13px; }"
        , "box.u-settings box.answer { background: var(--accent-bg); }"
        , "box.u-settings box.answer image { color: var(--success); }"
        , "box.u-settings box.setting { padding: 14px 16px; border-bottom: 1px solid var(--border-subtle); }"
        , "box.u-settings box.setting:last-child { border-bottom: none; }"
        , "box.u-settings box.setting label.title { font-weight: 600; }"
        , "box.u-settings box.setting label.description { font-size: 13px; color: var(--text-muted); }"
        , "box.u-settings switch:checked { background: var(--accent); }"
        , "box.u-settings radio:checked { background: var(--accent); border-color: var(--accent); color: var(--on-accent); }"
        , "box.u-settings label.connected { color: var(--success); }"
        , "box.u-settings label.signed-out { color: var(--danger); }"
        , "box.u-settings dropdown > button {"
        , "  min-height: 34px; padding: 0 10px; border: 1px solid var(--input-border); border-radius: 6px;"
        , "  background: var(--surface); color: var(--text); box-shadow: none; background-image: none;"
        , "}"
        ]
