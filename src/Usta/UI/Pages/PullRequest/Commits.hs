{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Commits tab of a pull request: its commits on the left, the
-- selected commit's message and changes on the right.
module Usta.UI.Pages.PullRequest.Commits
  ( Commit (..)
  , fromCommit
  , LoadChanges
  , sample
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Diff as Diff
import Usta.UI.Components.Diff (Line (..))
import qualified Usta.UI.Components.Skeleton as Skeleton
import Usta.UI.Format (Clock, ago)
import qualified Usta.Types.Commit as C
import Usta.Types.Parse (unparse)

type Commit :: Type
data Commit = Commit
  { hash :: Text
    -- ^ Full hash; lists show the first seven characters.
  , subject :: Text
  , message :: Text
    -- ^ The body below the subject; may be empty.
  , author :: Text
  , initials :: Text
  , login :: Maybe Text
    -- ^ The author's account, for the picture.
  , age :: Text
  , changes :: [Diff.File]
  }

-- | A commit as the tab lists it; its changes are loaded when it is shown.
fromCommit :: Clock -> C.Commit -> Commit
fromCommit c commit = Commit
  { hash = unparse commit.sha
  , subject = commit.subject
  , message = commit.body
  , author = commit.author
  , initials = T.toUpper (T.take 2 commit.author)
  , login = commit.login
  , age = ago c commit.authoredAt
  , changes = []
  }

-- | A commit's changes, for when it is selected; 'Left' says why they
-- could not be loaded. Runs off the GTK main thread.
type LoadChanges :: Type
type LoadChanges = Commit -> IO (Either Text [Diff.File])

-- | @source@ says where the commits came from, as "History from the local
-- mirror".
new :: Maybe Avatar.Place -> Text -> LoadChanges -> [Commit] -> IO Gtk.Widget
new place source loadChanges commits = do
  columns <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #cssClasses := ["u-commits"]
                            ]
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #widthRequest := 380
                         , #hexpand := False
                         , #cssClasses := ["commit-list"]
                         ]
  count <- label (T.pack (show (length commits)) <> plural (length commits) <> " · " <> source) ["muted", "small", "count"]
  #append side count
  #append columns side

  detailScroll <- GI.new Gtk.ScrolledWindow [#hexpand := True, #vexpand := True]
  #append columns detailScroll

  rows <- forM commits $ \c -> do
    button <- commitRow place c
    #append side button
    pure (c, button)
  let select c = do
        forM_ rows $ \(other, button) ->
          if other.hash == c.hash
            then Gtk.widgetAddCssClass button "selected"
            else Gtk.widgetRemoveCssClass button "selected"
        content <- detail place loadChanges c
        GI.set detailScroll [#child := content]
  forM_ rows $ \(c, button) -> void $ GI.on button #clicked (select c)
  forM_ (take 1 commits) select
  Gtk.toWidget columns
  where
    plural 1 = " commit"
    plural _ = " commits"

commitRow :: Maybe Avatar.Place -> Commit -> IO Gtk.Button
commitRow place c = do
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  avatar <- pictureOf place 24 c
  GI.set avatar [#valign := Gtk.AlignStart]
  #append content avatar
  text <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #hexpand := True]
  title <- label c.subject ["strong"]
  GI.set title [#ellipsize := Pango.EllipsizeModeEnd]
  #append text title
  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  label c.author ["muted", "small"] >>= #append meta
  label age' ["muted", "small"] >>= #append meta
  #append text meta
  #append content text
  label (T.take 7 c.hash) ["mono", "small", "hash"] >>= #append content
  GI.new Gtk.Button [ #child := content
                    , #hasFrame := False
                    , #cssClasses := ["commit"]
                    ]
  where
    age' = "committed " <> c.age

detail :: Maybe Avatar.Place -> LoadChanges -> Commit -> IO Gtk.Box
detail place loadChanges c = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 16, #cssClasses := ["commit-detail"]]
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  title <- label c.subject ["commit-title"]
  GI.set title [#wrap := True]
  #append header title
  if T.null c.message
    then pure ()
    else do
      body <- label c.message ["soft"]
      GI.set body [#wrap := True]
      #append header body
  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  pictureOf place 20 c >>= #append meta
  label c.author ["small", "strong"] >>= #append meta
  label ("committed " <> c.age) ["muted", "small"] >>= #append meta
  fullHash <- label c.hash ["mono", "small", "muted"]
  GI.set fullHash [#hexpand := True, #halign := Gtk.AlignEnd, #selectable := True]
  #append meta fullHash
  #append header meta
  #append box header
  changes <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 16]
  Skeleton.block 160 >>= #append changes
  #append box changes
  void $ forkIO $ do
    loaded <- try (loadChanges c)
    let result = either (\(_ :: SomeException) -> Left "Loading the changes failed unexpectedly.") id loaded
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      clear changes
      case result of
        Right files -> forM_ files $ \f -> Diff.plain f >>= #append changes
        Left message -> label message ["muted"] >>= #append changes
      pure False
  pure box
  where
    clear box = Gtk.widgetGetFirstChild box >>= mapM_ (\w -> Gtk.boxRemove box w >> clear box)

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [Commit]
sample =
  [ Commit "e1c90fa3b7d24c61a9f05e8812c4d6b0f3a7e921" "feat: add gi-gtk4 dependency"
      "Adds the GTK4 bindings to the cabal file and the flake derivation."
      "abdivasiyev" "AA" (Just "abdivasiyev") "5 min ago"
      [ Diff.File "usta.cabal" False
          [ Hunk "@@ -12,6 +12,8 @@ executable usta"
          , Context 13 13 "  main-is: Main.hs"
          , Context 14 14 "  hs-source-dirs: app"
          , Removed 15 "  build-depends:    base >=4.18 && <5"
          , Added 15 "  build-depends:"
          , Added 16 "    base,"
          , Added 17 "    gi-gtk4,"
          ]
      , Diff.File "flake.nix" False
          [ Hunk "@@ -20,7 +20,7 @@ ustaFor = pkgs:"
          , Context 22 22 "          isExecutable = true;"
          , Removed 23 "          executableHaskellDepends = with hsPkgs; [ base ];"
          , Added 23 "          executableHaskellDepends = with hsPkgs; [ base gi-gtk4 ];"
          ]
      ]
  ]

css :: Text
css = mconcat
  [ "box.u-commits label.small { font-size: 12px; }"
  , "box.u-commits label.strong { font-weight: 600; }"
  , "box.u-commits label.hash { color: var(--accent); }"
  , "box.u-commits box.commit-list { background: var(--surface); border-right: 1px solid var(--border); }"
  , "box.u-commits label.count { margin: 12px 16px; }"
  , "box.u-commits button.commit {"
  , "  padding: 12px 16px; border-radius: 0; border: none; border-bottom: 1px solid var(--border-subtle);"
  , "  box-shadow: none; background: transparent; background-image: none; color: var(--text);"
  , "  font-weight: 400;"
  , "}"
  , "box.u-commits button.commit:hover { background: var(--window); }"
  , "box.u-commits button.commit.selected { background: var(--selected); box-shadow: inset 3px 0 0 var(--accent); }"
  , "box.u-commits box.commit-detail { padding: 20px 24px; }"
  , "box.u-commits label.commit-title { font-size: 18px; font-weight: 600; }"
  ]

-- | The author's picture, or initials without an account.
pictureOf :: Maybe Avatar.Place -> Int32 -> Commit -> IO Gtk.Widget
pictureOf place size c = maybe (Avatar.new size c.initials >>= Gtk.toWidget) (Avatar.person size place) c.login
