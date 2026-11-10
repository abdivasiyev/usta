{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A window showing every component, for building and checking them in
isolation. New components get a section here.
-}
module Usta.UI.Gallery
    ( build
    , styles
    )
where

import Control.Monad (forM_, void)
import Data.GI.Base (AttrOp ((:=)))
import Data.Text (Text)

import Data.GI.Base qualified as GI
import GI.Gtk qualified as Gtk

import Usta.UI.Style (Tone (..))

import Usta.UI.Components.Avatar qualified as Avatar
import Usta.UI.Components.Badge qualified as Badge
import Usta.UI.Components.Button qualified as Button
import Usta.UI.Components.Card qualified as Card
import Usta.UI.Components.Icon qualified as Icon
import Usta.UI.Components.NavItem qualified as NavItem
import Usta.UI.Components.Progress qualified as Progress
import Usta.UI.Components.SearchField qualified as SearchField
import Usta.UI.Components.Segmented qualified as Segmented
import Usta.UI.Components.Shortcut qualified as Shortcut
import Usta.UI.Components.Skeleton qualified as Skeleton
import Usta.UI.Components.SyncStatus qualified as SyncStatus
import Usta.UI.Style qualified as Style

-- | The CSS of every component, in one list for 'Style.install'.
styles :: [Text]
styles =
    [ Avatar.css
    , Badge.css
    , Button.css
    , Card.css
    , Icon.css
    , NavItem.css
    , SearchField.css
    , Segmented.css
    , Shortcut.css
    , Skeleton.css
    , Progress.css
    , SyncStatus.css
    , galleryCss
    ]

build :: Gtk.Application -> IO Gtk.ApplicationWindow
build app = do
    page <- vbox 28
    Gtk.widgetAddCssClass page "gallery"

    section page "Brand" $ \row -> do
        Icon.new 32 Icon.Logo >>= #append row
        GI.new Gtk.Label [#label := "usta", #cssClasses := ["brand"]] >>= #append row

    section page "Buttons" $ \row -> do
        let
            btn cfg = Button.new cfg >>= #append row
        btn Button.defaults {Button.label = Just "Open review", Button.variant = Button.Primary}
        btn Button.defaults {Button.label = Just "Mark as done"}
        btn Button.defaults {Button.label = Just "Remove", Button.variant = Button.Danger}
        btn
            Button.defaults
                { Button.icon = Just "view-refresh-symbolic"
                , Button.variant = Button.Flat
                , Button.tooltip = Just "Sync now"
                }
        disabled <- Button.new Button.defaults {Button.label = Just "Squash and merge", Button.variant = Button.Primary}
        Gtk.widgetSetSensitive disabled False
        #append row disabled

    section page "Badges and dots" $ \row -> do
        forM_
            [ (Success, "Open")
            , (Warning, "Review requested")
            , (Danger, "Checks failed")
            , (Accent, "Mentioned")
            , (Neutral, "Assigned")
            , (Merged, "Merged")
            ]
            $ \(tone, text) -> Badge.new tone text >>= #append row
        forM_ [minBound .. maxBound] $ \tone -> Badge.dot tone >>= #append row

    section page "Icons" $ \row -> do
        Icon.toned 16 Success Icon.PullRequest >>= #append row
        Icon.toned 16 Merged Icon.PullRequest >>= #append row
        Icon.toned 16 Neutral Icon.Issue >>= #append row
        Icon.toned 16 Success Icon.Check >>= #append row
        Icon.toned 16 Danger Icon.Cross >>= #append row
        Icon.toned 24 Accent Icon.PullRequest >>= #append row

    section page "Avatars" $ \row -> do
        Avatar.new 32 "AA" >>= #append row
        Avatar.new 24 "AA" >>= #append row

    section page "Search and filters" $ \row -> do
        (search, _) <- SearchField.new SearchField.defaults
        #append row search
        (filters, _) <- Segmented.new ["All", "Unread", "Participating"] 0
        #append row filters

    section page "Navigation" $ \row -> do
        nav <-
            GI.new
                Gtk.Box
                [ #orientation := Gtk.OrientationVertical
                , #spacing := 2
                , #widthRequest := 240
                , #halign := Gtk.AlignStart
                , #cssClasses := ["gallery-sidebar"]
                ]
        let
            item cfg = NavItem.new cfg >>= #append nav
        item NavItem.defaults {NavItem.label = "Inbox", NavItem.count = Just 7, NavItem.active = True}
        item NavItem.defaults {NavItem.label = "Review requests", NavItem.count = Just 2}
        Card.sectionTitle "Forges" >>= #append nav
        item NavItem.defaults {NavItem.label = "github.com", NavItem.dot = Just Neutral}
        item NavItem.defaults {NavItem.label = "git.azizovich.uz", NavItem.dot = Just Warning}
        item NavItem.defaults {NavItem.label = "+ Add forge", NavItem.link = True}
        Card.sectionTitle "Pinned repositories" >>= #append nav
        item NavItem.defaults {NavItem.label = "abdivasiyev/usta", NavItem.mono = True}
        #append row nav

    section page "Cards" $ \row -> do
        Card.stat Success "Checks" "1 of 1 passed" >>= #append row
        Card.stat Neutral "Changes" "+4 −2 in 2 files" >>= #append row
        Card.stat Warning "Your review" "Requested" >>= #append row

    section page "Skeletons" $ \row -> do
        rows <- Card.new
        GI.set rows [#widthRequest := 560, #valign := Gtk.AlignStart]
        forM_ [300, 240, 280] $ \w -> Skeleton.listRow w >>= #append rows
        #append row rows
        detail <- Card.new
        GI.set detail [#widthRequest := 360, #valign := Gtk.AlignStart]
        Gtk.widgetAddCssClass detail "gallery-padded"
        Skeleton.detail >>= #append detail
        #append row detail

    section page "Progress" $ \row -> do
        column <- vbox 16
        GI.set column [#widthRequest := 360, #hexpand := False]
        Progress.new [Progress.Plain "Fetching ", Progress.Code "feat/big-refactor"] (Progress.Percent 64)
            >>= #append column
        Progress.new [Progress.Plain "Talking to git.azizovich.uz"] Progress.Unknown >>= #append column
        #append row column

    section page "Sync status" $ \row -> do
        column <- vbox 12
        forM_
            [ SyncStatus.Synced "1 min ago"
            , SyncStatus.Syncing "3 of 5 repositories"
            , SyncStatus.Offline
            , SyncStatus.Failed
            ]
            $ \state ->
                SyncStatus.new state >>= #append column
        #append row column

    scroller <- GI.new Gtk.ScrolledWindow [#child := page, #vexpand := True]
    footer <- Shortcut.bar [("j k", "move"), ("Enter", "open"), ("e", "done")] (Just "Component gallery")
    root <- vbox 0
    #append root scroller
    #append root footer

    window <-
        GI.new
            Gtk.ApplicationWindow
            [ #application := app
            , #title := "usta components"
            , #defaultWidth := 1280
            , #defaultHeight := 800
            , #child := root
            , #cssClasses := ["usta"]
            ]
    Gtk.toWidget window >>= \w -> Style.install w styles
    #present window
    pure window

section :: Gtk.Box -> Text -> (Gtk.Box -> IO ()) -> IO ()
section page title fill = do
    heading <- Card.sectionTitle title
    row <-
        GI.new
            Gtk.Box
            [ #orientation := Gtk.OrientationHorizontal
            , #spacing := 12
            , #valign := Gtk.AlignStart
            ]
    fill row
    group <- vbox 10
    #append group heading
    #append group row
    void $ #append page group

vbox :: Int -> IO Gtk.Box
vbox gap = GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := fromIntegral gap]

galleryCss :: Text
galleryCss =
    mconcat
        [ "box.gallery { padding: 32px 40px; }"
        , "box.gallery-sidebar { background: var(--sidebar); padding: 12px; border-radius: 8px; }"
        , "box.gallery-sidebar label.u-section-title { margin: 12px 10px 4px; }"
        , "label.brand { font-weight: 700; font-size: 15px; }"
        , "box.gallery-padded { padding: 16px; }"
        ]
