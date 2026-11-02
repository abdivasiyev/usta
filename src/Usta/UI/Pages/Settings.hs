{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Settings: a section list on the left, every section on one scrolling
-- page on the right.
module Usta.UI.Pages.Settings
  ( Auth (..)
  , Account (..)
  , Model (..)
  , sample
  , new
  , css
  ) where

import Control.Monad (forM, forM_, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Forge as Forge
import qualified Usta.UI.Components.NavItem as NavItem
import qualified Usta.UI.Components.Shortcut as Shortcut
import Usta.UI.Style (Tone (..))

-- | How usta signed in to the forge.
data Auth = BrowserSignIn | AccessToken

data Account = Account
  { forge :: Forge.Kind
  , host :: Text
  , user :: Text
  , auth :: Auth
  , connected :: Bool
  }

data Model = Model
  { accounts :: [Account]
  , syncIntervals :: [Text]
  , syncInterval :: Word32
    -- ^ Index into 'syncIntervals'.
  , mirrorPath :: Text
  , diffLayouts :: [Text]
  , diffLayout :: Word32
  , shortcuts :: [(Text, Text)]
    -- ^ Keys and what they do, as the shortcut strip shows them.
  }

data Section = Accounts | Sync | Notifications | Appearance | Keyboard
  deriving (Eq, Enum, Bounded)

sectionTitle :: Section -> Text
sectionTitle s = case s of
  Accounts -> "Accounts"
  Sync -> "Sync and storage"
  Notifications -> "Notifications"
  Appearance -> "Appearance"
  Keyboard -> "Keyboard"

-- | @onBack@ runs for Back; @onAddAccount@ for "Add account".
new :: IO () -> IO () -> Model -> IO Gtk.Widget
new onBack onAddAccount m = do
  page <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #hexpand := True
                         , #cssClasses := ["u-settings"]
                         ]

  bar <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 12
                        , #cssClasses := ["settings-header"]
                        ]
  back <- Button.new Button.defaults
    { Button.label = Just "Back"
    , Button.icon = Just "go-previous-symbolic"
    , Button.variant = Button.Flat
    }
  void $ GI.on back #clicked onBack
  #append bar back
  label "Settings" ["page-title"] >>= #append bar
  #append page bar

  content <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                            , #spacing := 32
                            , #widthRequest := 796
                            , #halign := Gtk.AlignStart
                            , #cssClasses := ["content"]
                            ]
  sections <- forM [minBound .. maxBound] $ \s -> do
    widget <- section s m onAddAccount
    #append content widget
    pure (s, widget)

  viewport <- GI.new Gtk.Viewport [#child := content]
  scroll <- GI.new Gtk.ScrolledWindow [ #child := viewport
                                      , #hexpand := True
                                      , #vexpand := True
                                      , #hscrollbarPolicy := Gtk.PolicyTypeNever
                                      ]

  nav <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                        , #spacing := 2
                        , #widthRequest := 240
                        , #hexpand := False
                        , #cssClasses := ["settings-nav"]
                        ]
  items <- forM sections $ \(s, widget) -> do
    item <- NavItem.new NavItem.defaults
      { NavItem.label = sectionTitle s, NavItem.active = s == minBound }
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

section :: Section -> Model -> IO () -> IO Gtk.Widget
section s m onAddAccount = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 12]
  heading <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  title <- label (sectionTitle s) ["section-title"]
  GI.set title [#hexpand := True, #valign := Gtk.AlignCenter]
  #append heading title
  when (s == Accounts) $ do
    add <- Button.new Button.defaults
      { Button.label = Just "Add account", Button.variant = Button.Primary }
    void $ GI.on add #clicked onAddAccount
    #append heading add
  #append box heading

  card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["u-card"]]
  rows <- case s of
    Accounts -> mapM accountRow (accounts m)
    Sync -> sequence
      [ do
          interval <- Gtk.dropDownNewFromStrings (syncIntervals m)
          Gtk.dropDownSetSelected interval (syncInterval m)
          setting "Check for updates" [plain "How often usta asks your forges for new activity"]
            =<< sequence [Gtk.toWidget interval]
      , setting "Local git mirrors"
          [plain "Diffs are computed from bare clones in ", code (mirrorPath m)]
          =<< sequence [button "Show in folder" Button.Secondary, button "Prune unused" Button.Secondary]
      , setting "Offline cache" [plain "Pull requests, issues and comments stored in a local database"]
          =<< sequence [button "Clear cache" Button.Danger]
      ]
    Notifications -> sequence
      [ setting "Desktop notifications" [plain "Review requests, mentions and failed checks"]
          =<< sequence [switch True]
      , setting "Only when usta is in the background" [plain "Stay quiet while you are looking at the app"]
          =<< sequence [switch False]
      ]
    Appearance -> sequence
      [ setting "Theme" [] =<< sequence [themeChoice]
      , do
          layout <- Gtk.dropDownNewFromStrings (diffLayouts m)
          Gtk.dropDownSetSelected layout (diffLayout m)
          setting "Diff layout" [] =<< sequence [Gtk.toWidget layout]
      ]
    Keyboard -> forM (shortcuts m) $ \(keys, action) -> do
      keyLabel <- Shortcut.key keys >>= Gtk.toWidget
      setting (capitalize action) [] [keyLabel]
  forM_ rows $ #append card
  #append box card
  Gtk.toWidget box
  where
    capitalize t = T.toUpper (T.take 1 t) <> T.drop 1 t

-- | A piece of a row's description, in the normal or the code font.
data Piece = Piece Text Bool

plain, code :: Text -> Piece
plain t = Piece t False
code t = Piece t True

-- | A row: a title with an optional description, controls on the right.
setting :: Text -> [Piece] -> [Gtk.Widget] -> IO Gtk.Widget
setting title pieces controls = do
  row <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 12
                        , #cssClasses := ["setting"]
                        ]
  texts <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                          , #spacing := 2
                          , #hexpand := True
                          , #valign := Gtk.AlignCenter
                          ]
  label title ["title"] >>= #append texts
  when (not (null pieces)) $ description pieces >>= #append texts
  #append row texts
  forM_ controls $ \c -> do
    GI.set c [#valign := Gtk.AlignCenter]
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
  tile <- Forge.tile (forge a)
  status <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  if connected a
    then do
      Badge.dot Success >>= #append status
      label "Connected" ["connected"] >>= #append status
    else do
      Badge.dot Danger >>= #append status
      label "Signed out" ["signed-out"] >>= #append status
  controls <- sequence
    [ Gtk.toWidget status
    , button "Re-authenticate" Button.Secondary
    , button "Remove" Button.Danger
    ]
  row <- setting (host a)
    [plain "Signed in as ", code (user a), plain (" · " <> authText (auth a))]
    controls
  -- The tile goes before the texts.
  box <- GI.castTo Gtk.Box row
  forM_ box $ \b -> Gtk.boxPrepend b tile
  pure row
  where
    authText BrowserSignIn = "Browser sign-in"
    authText AccessToken = "Access token"

-- | System, Light or Dark; switches the app's colors right away.
themeChoice :: IO Gtk.Widget
themeChoice = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 16]
  settings <- Gtk.settingsGetDefault
  reported <- maybe (pure Gtk.InterfaceColorSchemeDefault) (`GI.get` #gtkInterfaceColorScheme) settings
  -- Anything but an explicit light or dark choice follows the system.
  let current
        | reported `elem` [Gtk.InterfaceColorSchemeLight, Gtk.InterfaceColorSchemeDark] = reported
        | otherwise = Gtk.InterfaceColorSchemeDefault
  let choices = [ ("System", Gtk.InterfaceColorSchemeDefault)
                , ("Light", Gtk.InterfaceColorSchemeLight)
                , ("Dark", Gtk.InterfaceColorSchemeDark)
                ]
  buttons <- forM choices $ \(name, scheme) -> do
    b <- GI.new Gtk.CheckButton [#label := name, #active := scheme == current]
    void $ GI.on b #toggled $ do
      active <- GI.get b #active
      when active $ forM_ settings $ \s -> GI.set s [#gtkInterfaceColorScheme := scheme]
    #append box b
    pure b
  case buttons of
    first : rest -> forM_ rest $ \b -> Gtk.checkButtonSetGroup b (Just first)
    [] -> pure ()
  Gtk.toWidget box

switch :: Bool -> IO Gtk.Widget
switch on = GI.new Gtk.Switch [#active := on] >>= Gtk.toWidget

button :: Text -> Button.Variant -> IO Gtk.Widget
button text variant =
  Button.new Button.defaults { Button.label = Just text, Button.variant = variant } >>= Gtk.toWidget

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [(Text, Text)] -> Model
sample keys = Model
  { accounts =
      [ Account Forge.GitHub "github.com" "abdivasiyev" BrowserSignIn True
      , Account Forge.Forgejo "git.azizovich.uz" "abdivasiyev" AccessToken True
      ]
  , syncIntervals = ["Every minute", "Every 2 minutes", "Every 5 minutes", "Every 15 minutes", "Manually"]
  , syncInterval = 1
  , mirrorPath = "~/.cache/usta/repos"
  , diffLayouts = ["Unified", "Split"]
  , diffLayout = 0
  , shortcuts = keys
  }

css :: Text
css = mconcat
  [ "box.u-settings box.settings-header {"
  , "  padding: 8px 16px; background: var(--surface); border-bottom: 1px solid var(--border);"
  , "}"
  , "box.u-settings label.page-title { font-size: 17px; font-weight: 600; }"
  , "box.u-settings box.settings-nav {"
  , "  padding: 16px 12px; background: var(--sidebar); border-right: 1px solid var(--border);"
  , "}"
  , "box.u-settings box.content { padding: 24px 32px 48px; }"
  , "box.u-settings label.section-title { font-size: 19px; font-weight: 600; }"
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
