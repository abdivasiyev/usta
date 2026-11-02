{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | The Files tab of a pull request: changed files on the left, their
-- diffs with review threads in the middle, and your review on the right.
module Usta.UI.Pages.PullRequest.Files
  ( Comment (..)
  , Model (..)
  , sample
  , new
  , css
  ) where

import Control.Monad (forM, forM_, void)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk

import qualified Usta.UI.Components.Avatar as Avatar
import qualified Usta.UI.Components.Badge as Badge
import qualified Usta.UI.Components.Button as Button
import qualified Usta.UI.Components.Card as Card
import qualified Usta.UI.Components.Diff as Diff
import Usta.UI.Components.Diff (Line (..))
import qualified Usta.UI.Components.Icon as Icon
import Usta.UI.Style (Tone (..))

-- | A review comment attached to a line of a file's diff.
data Comment = Comment
  { onLine :: Int
    -- ^ Index into the file's diff lines.
  , author :: Text
  , pending :: Bool
  , body :: Text
  }

data Model = Model
  { files :: [(Diff.File, [Comment])]
  , range :: Text
    -- ^ The commits the diff was computed between, e.g. "8427b36..e1c90fa".
  , checks :: [(Bool, Text, Text)]
    -- ^ Passed, name, duration.
  , reviewers :: [(Tone, Text, Text)]
    -- ^ Status, name, state.
  , approvalsNeeded :: Int
  }

new :: Model -> IO Gtk.Widget
new m = do
  columns <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                            , #cssClasses := ["u-files"]
                            ]
  fileList m >>= #append columns

  main <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 16
                         , #cssClasses := ["diffs"]
                         ]
  toolbar <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12]
  label "Diff computed locally" ["muted", "small"] >>= #append toolbar
  label (range m) ["muted", "small", "mono"] >>= #append toolbar
  #append main toolbar
  forM_ (files m) $ \(file, comments) -> do
    attached <- forM comments $ \c -> do
      w <- thread c >>= Gtk.toWidget
      pure (onLine c, w)
    Diff.new file attached >>= #append main
  scroll <- GI.new Gtk.ScrolledWindow [ #child := main
                                      , #hexpand := True
                                      , #vexpand := True
                                      ]
  #append columns scroll

  review m >>= #append columns
  Gtk.toWidget columns

fileList :: Model -> IO Gtk.Box
fileList m = do
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 6
                         , #widthRequest := 240
                         , #hexpand := False
                         , #cssClasses := ["file-list"]
                         ]
  GI.new Gtk.SearchEntry [#placeholderText := "Filter files"] >>= #append side
  -- The first file is the one in view until scrolling is tracked.
  forM_ (zip [0 :: Int ..] (files m)) $ \(index, (file, _)) -> do
    row <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 8
                          , #cssClasses := ["file-row"] <> ["current" | index == 0]
                          ]
    mark <- GI.new Gtk.Box [ #cssClasses := ["viewed-mark"] <> ["done" | Diff.viewed file]
                           , #valign := Gtk.AlignCenter
                           ]
    if Diff.viewed file
      then Icon.toned 10 Success Icon.Check >>= #append mark
      else pure ()
    #append row mark
    name <- label (Diff.path file) ["mono", "small"]
    GI.set name [#hexpand := True]
    #append row name
    let (plus, minus) = counts file
    label ("+" <> plus) ["mono", "small", "added"] >>= #append row
    label ("−" <> minus) ["mono", "small", "removed"] >>= #append row
    #append side row
  let total = length (files m)
      seen = length (filter (Diff.viewed . fst) (files m))
  progress <- label (T.pack (show seen) <> " of " <> T.pack (show total) <> " files viewed") ["muted", "small"]
  GI.set progress [#vexpand := True, #valign := Gtk.AlignEnd]
  #append side progress
  pure side
  where
    counts file =
      ( T.pack (show (length [() | Added _ _ <- Diff.diffLines file]))
      , T.pack (show (length [() | Removed _ _ <- Diff.diffLines file]))
      )

thread :: Comment -> IO Gtk.Box
thread c = do
  card <- Card.new
  Gtk.widgetAddCssClass card "thread"
  top <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                        , #spacing := 10
                        , #cssClasses := ["thread-body"]
                        ]
  avatar <- Avatar.new 28 "AA"
  GI.set avatar [#valign := Gtk.AlignStart]
  #append top avatar
  text <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 4]
  who <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
  label (author c) ["strong"] >>= #append who
  if pending c then label "· pending" ["muted"] >>= #append who else pure ()
  #append text who
  bodyLabel <- label (body c) []
  GI.set bodyLabel [#wrap := True]
  #append text bodyLabel
  #append top text
  #append card top

  reply <- GI.new Gtk.Box [ #orientation := Gtk.OrientationHorizontal
                          , #spacing := 8
                          , #cssClasses := ["thread-reply"]
                          ]
  entry <- GI.new Gtk.Entry [#placeholderText := "Reply…", #hexpand := True]
  #append reply entry
  Button.new Button.defaults { Button.label = Just "Resolve" } >>= #append reply
  #append card reply
  pure card

review :: Model -> IO Gtk.Box
review m = do
  side <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                         , #spacing := 20
                         , #widthRequest := 280
                         , #hexpand := False
                         , #cssClasses := ["review"]
                         ]

  verdict <- section "Your review"
  radios <- forM ["Comment", "Approve", "Request changes"] $ \text ->
    GI.new Gtk.CheckButton [#label := text]
  case radios of
    first : rest -> do
      forM_ rest $ \r -> GI.set r [#group := first]
      GI.set first [#active := True]
    [] -> pure ()
  forM_ radios (#append verdict)
  label "Summary" ["soft", "small"] >>= #append verdict
  GI.new Gtk.TextView [ #wrapMode := Gtk.WrapModeWordChar
                      , #heightRequest := 72
                      , #topMargin := 8
                      , #bottomMargin := 8
                      , #leftMargin := 10
                      , #rightMargin := 10
                      , #cssClasses := ["editor"]
                      ] >>= #append verdict
  let pendingCount = length [() | (_, cs) <- files m, c <- cs, pending c]
  label (T.pack (show pendingCount) <> " pending comment" <> (if pendingCount == 1 then "" else "s"))
    ["muted", "small"] >>= #append verdict
  #append side verdict

  checksBox <- section "Checks"
  forM_ (checks m) $ \(passed, name, took) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    Icon.toned 14 (if passed then Success else Danger) (if passed then Icon.Check else Icon.Cross)
      >>= #append row
    nameLabel <- label name ["mono", "small"]
    GI.set nameLabel [#hexpand := True]
    #append row nameLabel
    label took ["muted", "small"] >>= #append row
    #append checksBox row
  #append side checksBox

  reviewersBox <- section "Reviewers"
  forM_ (reviewers m) $ \(tone, name, state) -> do
    row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 8]
    Badge.dot tone >>= #append row
    label name ["small"] >>= #append row
    label ("· " <> state) ["muted", "small"] >>= #append row
    #append reviewersBox row
  #append side reviewersBox

  bottom <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical
                           , #spacing := 8
                           , #vexpand := True
                           , #valign := Gtk.AlignEnd
                           ]
  mergeButton <- Button.new Button.defaults
    { Button.label = Just "Squash and merge", Button.variant = Button.Primary }
  GI.set mergeButton [#sensitive := approvalsNeeded m == 0]
  #append bottom mergeButton
  void $ if approvalsNeeded m == 0
    then pure ()
    else do
      needs <- label ("Needs " <> T.pack (show (approvalsNeeded m)) <> " approval") ["muted", "small"]
      GI.set needs [#xalign := 0.5]
      #append bottom needs
  #append side bottom
  pure side
  where
    section title = do
      box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8]
      Card.sectionTitle title >>= #append box
      pure box

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

sample :: Model
sample = Model
  { files =
      [ ( Diff.File "usta.cabal" True
            [ Hunk "@@ -12,6 +12,8 @@ executable usta"
            , Context 13 13 "  main-is: Main.hs"
            , Context 14 14 "  hs-source-dirs: app"
            , Removed 15 "  build-depends:    base >=4.18 && <5"
            , Added 15 "  build-depends:"
            , Added 16 "    base,"
            , Added 17 "    gi-gtk4,"
            , Context 16 18 ""
            , Context 17 19 "  default-language: GHC2021"
            ]
        , [] )
      , ( Diff.File "flake.nix" False
            [ Hunk "@@ -20,7 +20,7 @@ ustaFor = pkgs:"
            , Context 21 21 "          isLibrary = false;"
            , Context 22 22 "          isExecutable = true;"
            , Removed 23 "          executableHaskellDepends = with hsPkgs; [ base ];"
            , Added 23 "          executableHaskellDepends = with hsPkgs; [ base gi-gtk4 ];"
            , Context 24 24 "          license = pkgs.lib.licenses.asl20;"
            ]
        , [ Comment 4 "abdivasiyev" True
              "Add wrapGAppsHook4 here once Main opens a window, otherwise the app can't find the GSettings schemas."
          ] )
      ]
  , range = "8427b36..e1c90fa"
  , checks = [(True, "nix build", "3m 24s")]
  , reviewers = [(Warning, "abdivasiyev", "awaiting")]
  , approvalsNeeded = 1
  }

css :: Text
css = mconcat
  [ "box.u-files label.small { font-size: 12px; }"
  , "box.u-files label.strong { font-weight: 600; }"
  , "box.u-files label.added { color: var(--success); }"
  , "box.u-files label.removed { color: var(--danger); }"
  , "box.u-files box.file-list {"
  , "  padding: 12px; background: var(--sidebar); border-right: 1px solid var(--border);"
  , "}"
  , "box.u-files box.file-row { padding: 8px 8px; border-radius: 6px; }"
  , "box.u-files box.file-row.current { background: var(--nav-active); }"
  , "box.u-files box.viewed-mark {"
  , "  min-width: 14px; min-height: 14px; border-radius: 3px; border: 1.5px solid var(--text-muted);"
  , "}"
  , "box.u-files box.viewed-mark.done { border-color: var(--success); }"
  , "box.u-files box.diffs { padding: 16px; }"
  , "box.u-files box.thread-body { padding: 12px; }"
  , "box.u-files box.thread-reply { padding: 10px 12px; border-top: 1px solid var(--border-subtle); }"
  , "box.u-files box.review {"
  , "  padding: 16px; background: var(--surface); border-left: 1px solid var(--border);"
  , "}"
  , "box.u-files textview.editor { border: 1px solid var(--input-border); border-radius: 6px; }"
  , "box.u-files textview.editor text { background: var(--surface); color: var(--text); }"
  ]
