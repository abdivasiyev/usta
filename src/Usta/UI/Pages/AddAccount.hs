{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Connecting a forge account, in a modal window: forge type, server, and
-- how to sign in.
module Usta.UI.Pages.AddAccount
  ( Model (..)
  , sample
  , for
  , Server (..)
  , parseServer
  , SignIn
  , BrowserSignIn
  , open
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, join, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Browser as Browser
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.Icon as Icon
import qualified Usta.UI.Components.Segmented as Segmented
import qualified Usta.UI.Platform as Platform
import Usta.UI.Style (Tone (..))
import Usta.Types.Host (Host)
import Usta.Types.Parse (parse)
import Usta.Types.Token (Token)

type Model :: Type
data Model = Model
  { forge :: Forge.Kind
    -- ^ The forge selected at start.
  , server :: Text
  , reachable :: [Text]
    -- ^ Hosts known to answer, until the dialog can ask them itself.
  }

sample :: Model
sample = Model
  { forge = Forge.Forgejo
  , server = "https://git.azizovich.uz"
  , reachable = ["github.com", "gitlab.com", "git.azizovich.uz"]
  }

-- | The dialog with @kind@ selected and its usual server filled in;
-- Forgejo servers have none, so that one starts empty.
for :: Forge.Kind -> Model
for kind = Model
  { forge = kind
  , server = defaultServer kind
  , reachable = []
  }

-- | A forge server, parsed from what was typed.
type Server :: Type
newtype Server = Server { host :: Text }
  deriving (Eq, Show)

-- | Only https addresses; a path after the host is ignored.
parseServer :: Text -> Either Text Server
parseServer raw = case T.stripPrefix "https://" text of
  Just rest
    | validHost (hostOf rest) -> Right (Server (hostOf rest))
    | otherwise -> Left "That address has no valid host"
  Nothing
    | "http://" `T.isPrefixOf` text -> Left "Use an https:// address"
    | otherwise -> Left "Start the address with https://"
  where
    text = T.strip raw
    hostOf = T.toLower . T.takeWhile (/= '/')
    validHost h = not (T.null h) && T.all (\c -> c /= ' ' && c /= '@') h

defaultServer :: Forge.Kind -> Text
defaultServer kind = case kind of
  Forge.GitHub -> "https://github.com"
  Forge.GitLab -> "https://gitlab.com"
  Forge.Forgejo -> ""

reach :: Forge.Kind -> Text
reach kind = case kind of
  Forge.GitHub -> "github.com or Enterprise"
  Forge.GitLab -> "gitlab.com or self-hosted"
  Forge.Forgejo -> "Forgejo or Gitea"

-- | The token scopes each forge needs for what usta shows.
scopes :: Forge.Kind -> [Text]
scopes kind = case kind of
  Forge.GitHub -> ["repo", "read:org", "read:user", "notifications"]
  Forge.GitLab -> ["api", "read_user"]
  Forge.Forgejo -> ["read:user", "read:repository", "write:issue", "read:notification"]

-- | Signs in with a token: 'Left' says why it did not work. Runs off the
-- GTK main thread, since it asks the forge.
type SignIn :: Type
type SignIn = Forge.Kind -> Host -> Token -> IO (Either Text ())

-- | Starts a browser sign-in with an OAuth2 client ID, on the main thread:
-- the page to open, the wait for it to finish (off the main thread), and
-- how to stop waiting.
type BrowserSignIn :: Type
type BrowserSignIn = Forge.Kind -> Host -> Text -> IO (Either Text (Text, IO (Either Text ()), IO ()))

-- | Opens the dialog over the window that @anchor@ is in. "Add account"
-- runs @signInWith@; @onAdded@ runs once it worked, with the dialog closed.
open :: Gtk.Widget -> Model -> SignIn -> BrowserSignIn -> IO () -> IO ()
open anchor m signInWith browserSignIn onAdded = do
  root <- Gtk.widgetGetRoot anchor
  parent <- maybe (pure Nothing) (GI.castTo Gtk.Window) root

  dialog <- GI.new Gtk.Window [ #title := "Add a forge account"
                              , #modal := True
                              , #defaultWidth := 640
                              , #resizable := False
                              , #destroyWithParent := True
                              , #cssClasses := ["usta", "u-add-account"]
                              ]
  forM_ parent $ \p -> Gtk.windowSetTransientFor dialog (Just p)
  header <- GI.new Gtk.HeaderBar []
  Platform.configureHeaderBar Platform.current header
  GI.set dialog [#titlebar := header]

  body <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 10
                         , #cssClasses := ["form"]
                         ]

  -- 1. Forge type
  step "1. Forge type" >>= #append body
  cards <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 10
                          , #homogeneous := True
                          ]
  choices <- forM [minBound .. maxBound] $ \kind -> do
    card <- forgeCard kind
    GI.set card [#active := kind == m.forge]
    #append cards card
    pure (card, kind)
  case choices of
    (first, _) : rest -> forM_ rest $ \(c, _) -> Gtk.checkButtonSetGroup c (Just first)
    [] -> pure ()
  #append body cards

  -- 2. Server
  step "2. Server" >>= #append body
  caption "Server URL" >>= #append body
  url <- GI.new Gtk.Entry [ #text := m.server
                          , #placeholderText := "https://git.example.com"
                          , #cssClasses := ["mono"]
                          ]
  #append body url
  status <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  statusIcon <- Icon.toned 14 Success Icon.Check
  statusText <- GI.new Gtk.Label [#xalign := 0, #cssClasses := ["status"]]
  #append status statusIcon
  #append status statusText
  #append body status

  -- 3. Sign in
  step "3. Sign in" >>= #append body
  (methods, methodButtons) <- Segmented.new ["Access token", "Sign in with browser"] 0
  GI.set methods [#halign := Gtk.AlignStart]
  Gtk.widgetAddCssClass methods "methods"
  #append body methods

  -- Size to the visible page only: measuring the hidden one's wrapped
  -- hint against the visible one's height makes GTK warn.
  signIn <- GI.new Gtk.Stack [#vhomogeneous := False, #hhomogeneous := False]
  tokenPage <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10]
  caption "Personal access token" >>= #append tokenPage
  token <- GI.new Gtk.PasswordEntry [ #placeholderText := "Paste your token"
                                    , #showPeekIcon := True
                                    , #cssClasses := ["mono"]
                                    ]
  #append tokenPage token
  scopesHint <- GI.new Gtk.Label [ #xalign := 0
                                 , #wrap := True
                                 , #useMarkup := True
                                 , #cssClasses := ["hint"]
                                 ]
  #append tokenPage scopesHint
  void $ Gtk.stackAddNamed signIn tokenPage (Just "token")
  browserPage <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 10]
  browserHint <- GI.new Gtk.Label [#xalign := 0, #wrap := True, #cssClasses := ["hint"]]
  #append browserPage browserHint
  -- The three steps, shown once the browser is open.
  steps <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12, #visible := False, #cssClasses := ["steps"]]
  let stepRow n title detail = do
        row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
        number <- GI.new Gtk.Label [#label := n, #valign := Gtk.AlignStart, #cssClasses := ["step-number"]]
        #append row number
        texts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
        GI.new Gtk.Label [#label := title, #xalign := 0, #cssClasses := ["step-title"]] >>= #append texts
        forM_ detail $ \d -> GI.new Gtk.Label [#label := d, #xalign := 0, #wrap := True, #cssClasses := ["hint"]] >>= #append texts
        #append row texts
        #append steps row
        pure (number, texts)
  (opened, _) <- stepRow "1" "usta opened the server in your browser" (Just "Sign in there if it asks.")
  (approving, approveTexts) <- stepRow "2" "Approve \"usta\" on the forge" (Just "It asks for access to your repositories, issues, pull requests and notifications.")
  waitingRow <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  spinner <- GI.new Gtk.Spinner [#spinning := True]
  waitingText <- GI.new Gtk.Label [#label := "Waiting for the forge…", #cssClasses := ["hint"]]
  again <- Button.new Button.defaults {Button.label = Just "Open the page again", Button.variant = Button.Flat, Button.small = True}
  mapM_ (#append waitingRow) =<< sequence [Gtk.toWidget spinner, Gtk.toWidget waitingText, Gtk.toWidget again]
  #append approveTexts waitingRow
  (keeping, _) <- stepRow "3" "usta keeps the token in the system keyring" Nothing
  #append browserPage steps
  appBox <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8, #cssClasses := ["oauth-app"]]
  appHint <- GI.new Gtk.Label [ #xalign := 0, #wrap := True, #useMarkup := True, #cssClasses := ["hint"]
                              , #label := "<b>The server needs an OAuth app for usta, once.</b> An administrator, or you on your own account, creates an OAuth2 application: <tt>Settings → Applications</tt>, name <tt>usta</tt>, not confidential, redirect URI <tt>http://127.0.0.1/callback</tt>. Paste its client ID here:" ]
  clientId <- GI.new Gtk.Entry [#placeholderText := "Client ID", #cssClasses := ["mono"]]
  mapM_ (#append appBox) =<< sequence [Gtk.toWidget appHint, Gtk.toWidget clientId]
  #append browserPage appBox
  void $ Gtk.stackAddNamed signIn browserPage (Just "browser")
  -- A sign-in waiting for the browser, stopped by Cancel or closing.
  stopping <- newIORef (pure ())
  page <- newIORef ""
  void $ GI.on dialog #closeRequest $ join (readIORef stopping) >> pure False
  void $ GI.on again #clicked (readIORef page >>= openUri)
  #append body signIn

  footer <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                           , #spacing := 8
                           , #halign := Gtk.AlignFill
                           , #cssClasses := ["footer"]
                           ]
  spacer <- GI.new Gtk.Box [#hexpand := True]
  #append footer spacer
  cancel <- Button.new Button.defaults { Button.label = Just "Cancel" }
  add <- Button.new Button.defaults { Button.label = Just "Add account", Button.variant = Button.Primary }
  void $ GI.on cancel #clicked (join (readIORef stopping) >> Gtk.windowDestroy dialog)
  #append footer cancel
  #append footer add


  selected <- newIORef m.forge
  let showProblem problem = do
        GI.set status [#visible := True]
        GI.set statusIcon [#visible := False]
        GI.set statusText [#label := problem, #cssClasses := ["status", "problem"]]
      checking busy = GI.set add
        [ #sensitive := not busy
        , #label := (if busy then "Checking token…" else "Add account")
        ]
  void $ GI.on add #clicked $ do
    kind <- readIORef selected
    text <- Gtk.editableGetText url
    tokenText <- Gtk.editableGetText token
    browser <- GI.get (methodButtons !! 1) #active
    client <- T.strip <$> Gtk.editableGetText clientId
    if browser then case parseServer text >>= parse . (.host) of
      Left problem -> showProblem problem
      Right host -> browserSignIn kind host client >>= \case
        Left problem -> showProblem problem
        Right (link, wait, stop) -> do
          writeIORef stopping stop
          writeIORef page link
          openUri link
          GI.set steps [#visible := True]
          GI.set appBox [#visible := False]
          GI.set opened [#label := "✓", #cssClasses := ["step-number", "done"]]
          GI.set approving [#cssClasses := ["step-number", "now"]]
          GI.set add [#sensitive := False, #label := "Waiting…"]
          void $ forkIO $ do
            outcome <- try wait
            void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
              stop
              writeIORef stopping (pure ())
              case either (\(_ :: SomeException) -> Left "Signing in failed unexpectedly.") id outcome of
                Right () -> do
                  GI.set keeping [#label := "✓", #cssClasses := ["step-number", "done"]]
                  Gtk.windowDestroy dialog >> onAdded
                Left problem -> do
                  GI.set steps [#visible := False]
                  GI.set appBox [#visible := True]
                  GI.set add [#sensitive := True, #label := "Sign in"]
                  showProblem problem
              pure False
    else case (,) <$> (parseServer text >>= parse . (.host)) <*> parse tokenText of
      Left problem -> showProblem problem
      Right (host, parsedToken) -> do
        checking True
        void $ forkIO $ do
          outcome <- try (signInWith kind host parsedToken)
          let result = case outcome of
                Left (_ :: SomeException) -> Left "Signing in failed unexpectedly."
                Right r -> r
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            case result of
              Right () -> Gtk.windowDestroy dialog >> onAdded
              Left problem -> checking False >> showProblem problem
            pure False
  let refresh = do
        kind <- readIORef selected
        text <- Gtk.editableGetText url
        tokenText <- Gtk.editableGetText token
        browser <- GI.get (methodButtons !! 1) #active
        let parsed = parseServer text
        case parsed of
          _ | T.null (T.strip text) -> GI.set status [#visible := False]
          Right s | s.host `elem` m.reachable -> do
            GI.set status [#visible := True]
            GI.set statusIcon [#visible := True]
            GI.set statusText [ #label := Forge.name kind <> " API reachable"
                              , #cssClasses := ["status", "ok"]
                              ]
          Right _ -> do
            GI.set status [#visible := True]
            GI.set statusIcon [#visible := False]
            GI.set statusText [#label := "Checked when you add the account", #cssClasses := ["status"]]
          Left problem -> do
            GI.set status [#visible := True]
            GI.set statusIcon [#visible := False]
            GI.set statusText [#label := problem, #cssClasses := ["status", "problem"]]
        GI.set scopesHint [#label := scopesMarkup kind]
        client <- Gtk.editableGetText clientId
        GI.set browserHint
          [ #label := "usta opens " <> either (const "the server") (.host) parsed
              <> " in your browser and comes back here once you approve."
          ]
        -- Only Forgejo signs in with the browser so far.
        GI.set (methodButtons !! 1) [#sensitive := kind == Forge.Forgejo]
        when (kind /= Forge.Forgejo && browser) $ GI.set (methodButtons !! 0) [#active := True]
        Gtk.stackSetVisibleChildName signIn (if browser then "browser" else "token")
        GI.set add [ #label := if browser then "Sign in" else "Add account"
                   , #sensitive := either (const False) (const True) parsed
                                    && (if browser then not (T.null (T.strip client)) else not (T.null tokenText)) ]

  forM_ choices $ \(card, kind) ->
    void $ GI.on card #toggled $ do
      active <- GI.get card #active
      when active $ do
        writeIORef selected kind
        Gtk.editableSetText url (defaultServer kind)
        refresh
  void $ GI.on url #changed refresh
  void $ GI.on token #changed refresh
  void $ GI.on clientId #changed refresh
  forM_ methodButtons $ \b -> void $ GI.on b #toggled refresh
  refresh

  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #append content body
  #append content footer
  GI.set dialog [#child := content]
  #present dialog

openUri :: Text -> IO ()
openUri uri = do
  Browser.open (uri)

-- | "Needs scopes a, b and c. The token is kept …", scopes in code font.
scopesMarkup :: Forge.Kind -> Text
scopesMarkup kind = "Needs scopes " <> list (map code (scopes kind))
  <> ". The token is kept in your system keyring, never in a file."
  where
    code s = "<tt><b>" <> s <> "</b></tt>"
    list xs = case reverse xs of
      [] -> ""
      [x] -> x
      lastOne : rest -> T.intercalate ", " (reverse rest) <> " and " <> lastOne

forgeCard :: Forge.Kind -> IO Gtk.CheckButton
forgeCard kind = do
  texts <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  GI.new Gtk.Label [#label := Forge.name kind, #xalign := 0, #cssClasses := ["name"]] >>= #append texts
  GI.new Gtk.Label [#label := reach kind, #xalign := 0, #cssClasses := ["reach"]] >>= #append texts
  GI.new Gtk.CheckButton [#child := texts, #cssClasses := ["forge-card"]]

step :: Text -> IO Gtk.Label
step text = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := ["step"]]

caption :: Text -> IO Gtk.Label
caption text = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := ["caption"]]

css :: Text
css = mconcat
  [ "window.u-add-account headerbar {"
  , "  background: var(--surface); border-bottom: 1px solid var(--border); box-shadow: none;"
  , "}"
  , "window.u-add-account box.form { padding: 20px; background: var(--surface); }"
  , "window.u-add-account label.step { font-weight: 600; color: var(--text-soft); margin-top: 8px; }"
  , "window.u-add-account label.caption { color: var(--text-soft); }"
  , "window.u-add-account checkbutton.forge-card {"
  , "  padding: 14px; border: 1px solid var(--input-border); border-radius: 8px; background: var(--surface);"
  , "}"
  , "window.u-add-account checkbutton.forge-card:checked {"
  , "  border: 2px solid var(--accent); padding: 13px; background: var(--selected);"
  , "}"
  , "window.u-add-account checkbutton.forge-card radio:checked {"
  , "  background: var(--accent); border-color: var(--accent); color: var(--on-accent);"
  , "}"
  , "window.u-add-account checkbutton.forge-card label.name { font-weight: 600; font-size: 15px; }"
  , "window.u-add-account checkbutton.forge-card label.reach { font-size: 13px; color: var(--text-muted); }"
  , "window.u-add-account entry, window.u-add-account passwordentry {"
  , "  min-height: 40px; border: 1px solid var(--input-border); border-radius: 8px;"
  , "  background: var(--surface); color: var(--text); box-shadow: none;"
  , "}"
  , "window.u-add-account label.status { font-size: 13px; color: var(--text-muted); }"
  , "window.u-add-account label.status.ok { color: var(--success); }"
  , "window.u-add-account label.status.problem { color: var(--danger); }"
  , "window.u-add-account box.methods { background: var(--sidebar); border-radius: 8px; padding: 4px; }"
  , "window.u-add-account box.methods button:checked {"
  , "  background: var(--surface); color: var(--text); box-shadow: 0 1px 2px rgba(27, 29, 33, 0.12);"
  , "}"
  , "window.u-add-account label.hint { font-size: 13px; color: var(--text-muted); }"
  , "window.u-add-account label.step-number { min-width: 24px; min-height: 24px; border-radius: 12px; background: var(--neutral-bg); font-size: 12px; font-weight: 600; }"
  , "window.u-add-account label.step-number.done { background: var(--success); color: var(--on-accent); }"
  , "window.u-add-account label.step-number.now { background: var(--accent); color: var(--on-accent); }"
  , "window.u-add-account label.step-title { font-weight: 600; }"
  , "window.u-add-account box.oauth-app { padding: 14px; border-radius: 8px; background: var(--window); }"
  , "window.u-add-account box.footer {"
  , "  padding: 14px 20px; background: var(--window); border-top: 1px solid var(--border);"
  , "}"
  ]
