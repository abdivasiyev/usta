{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Commits tab of a pull request: its commits on the left, the
-- selected commit's message and changes on the right.
module Usta.UI.Pages.PullRequest.Commits
  ( Commit (..)
  , sample
  , new
  , css
  ) where

import Control.Monad (forM, forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Diff as Diff
import Usta.UI.Components.Diff (Line (..))

type Commit :: Type
data Commit = Commit
  { hash :: Text
    -- ^ Full hash; lists show the first seven characters.
  , subject :: Text
  , message :: Text
    -- ^ The body below the subject; may be empty.
  , author :: Text
  , initials :: Text
  , age :: Text
  , changes :: [Diff.File]
  }

new :: [Commit] -> IO Gtk.Widget
new commits = do
  columns <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #cssClasses := ["u-commits"]
                            ]
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #widthRequest := 380
                         , #hexpand := False
                         , #cssClasses := ["commit-list"]
                         ]
  count <- label (T.pack (show (length commits)) <> plural (length commits)) ["muted", "small", "count"]
  #append side count
  #append columns side

  detailScroll <- GI.new Gtk.ScrolledWindow [#hexpand := True, #vexpand := True]
  #append columns detailScroll

  rows <- forM commits $ \c -> do
    button <- commitRow c
    #append side button
    pure (c, button)
  let select c = do
        forM_ rows $ \(other, button) ->
          if hash other == hash c
            then Gtk.widgetAddCssClass button "selected"
            else Gtk.widgetRemoveCssClass button "selected"
        content <- detail c
        GI.set detailScroll [#child := content]
  forM_ rows $ \(c, button) -> void $ GI.on button #clicked (select c)
  forM_ (take 1 commits) select
  Gtk.toWidget columns
  where
    plural 1 = " commit"
    plural _ = " commits"

commitRow :: Commit -> IO Gtk.Button
commitRow c = do
  content <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
  avatar <- Avatar.new 24 (initials c)
  GI.set avatar [#valign := Gtk.AlignStart]
  #append content avatar
  text <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4, #hexpand := True]
  title <- label (subject c) ["strong"]
  GI.set title [#ellipsize := Pango.EllipsizeModeEnd]
  #append text title
  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  label (author c) ["muted", "small"] >>= #append meta
  label age' ["muted", "small"] >>= #append meta
  #append text meta
  #append content text
  label (T.take 7 (hash c)) ["mono", "small", "hash"] >>= #append content
  GI.new Gtk.Button [ #child := content
                    , #hasFrame := False
                    , #cssClasses := ["commit"]
                    ]
  where
    age' = "committed " <> age c

detail :: Commit -> IO Gtk.Box
detail c = do
  box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 16, #cssClasses := ["commit-detail"]]
  header <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  title <- label (subject c) ["commit-title"]
  GI.set title [#wrap := True]
  #append header title
  if T.null (message c)
    then pure ()
    else do
      body <- label (message c) ["soft"]
      GI.set body [#wrap := True]
      #append header body
  meta <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
  Avatar.new 20 (initials c) >>= #append meta
  label (author c) ["small", "strong"] >>= #append meta
  label ("committed " <> age c) ["muted", "small"] >>= #append meta
  fullHash <- label (hash c) ["mono", "small", "muted"]
  GI.set fullHash [#hexpand := True, #halign := Gtk.AlignEnd, #selectable := True]
  #append meta fullHash
  #append header meta
  #append box header
  forM_ (changes c) $ \f -> Diff.plain f >>= #append box
  pure box

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: [Commit]
sample =
  [ Commit "e1c90fa3b7d24c61a9f05e8812c4d6b0f3a7e921" "feat: add gi-gtk4 dependency"
      "Adds the GTK4 bindings to the cabal file and the flake derivation."
      "abdivasiyev" "AA" "5 min ago"
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
