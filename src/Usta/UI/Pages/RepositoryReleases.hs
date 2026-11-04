{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A repository's Releases tab: its releases, Latest and Pre-release
-- marked, on the left, or its tags; the chosen release on the right with
-- its notes rendered and its assets to download.
module Usta.UI.Pages.RepositoryReleases
  ( Actions (..)
  , new
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int64)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import qualified Usta.UI.Components.Button as Button
import Usta.UI.Components.CommandPalette (shortcut)
import qualified Usta.UI.Components.Markdown as Markdown
import qualified Usta.UI.Components.Segmented as Segmented
import Usta.UI.Format (Clock, ago, date)
import Usta.Types.Branch (Tag (..))
import Usta.Types.Release (Asset (..), Release (..))

type Actions :: Type
data Actions = Actions
  { releases :: IO (Either Text [Release])
  , tags :: IO (Either Text [Tag])
    -- ^ Both off the main thread.
  , tagUrl :: Text -> Text
    -- ^ A tag's page on the forge.
  }

new :: Clock -> Actions -> IO Gtk.Widget
new c a = do
  page <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 16, #cssClasses := ["u-releases"]]
  left <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 8, #widthRequest := 280]
  (kinds, kindButtons) <- Segmented.new ["Releases", "Tags"] 0
  GI.set kinds [#halign := Gtk.AlignStart]
  #append left kinds
  list <- GI.new Gtk.ListBox [#selectionMode := Gtk.SelectionModeSingle, #cssClasses := ["picks"]]
  listScroll <- GI.new Gtk.ScrolledWindow [#child := list, #hscrollbarPolicy := Gtk.PolicyTypeNever, #vexpand := True]
  #append left listScroll
  #append page left
  right <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 14, #hexpand := True, #cssClasses := ["u-card", "release"]]
  rightScroll <- GI.new Gtk.ScrolledWindow [#child := right, #hscrollbarPolicy := Gtk.PolicyTypeNever, #hexpand := True, #vexpand := True]
  #append page rightScroll
  label "Loading the releases…" ["muted"] >>= #append right

  loaded <- newIORef Nothing
  showingTags <- newIORef False
  let clearBox b = Gtk.widgetGetFirstChild b >>= mapM_ (\w -> Gtk.boxRemove b w >> clearBox b)
      say text = clearBox right >> label text ["muted"] >>= \l -> GI.set l [#wrap := True] >> #append right l
      latestOf rs = case [r | r <- rs, not r.draft, not r.prerelease] of
        r : _ -> Just r.tag
        [] -> Nothing
      fill = do
        Gtk.listBoxRemoveAll list
        tagsOn <- readIORef showingTags
        readIORef loaded >>= \case
          Nothing -> pure ()
          Just (rs, ts) -> if tagsOn
            then case ts of
              Left problem -> say problem
              Right [] -> say "No tags yet."
              Right all' -> do
                forM_ all' $ \t -> pick t.name (maybe "" (date c) t.at) [] >>= #append list
                selectFirst
            else case rs of
              Left problem -> say problem
              Right [] -> say "No releases yet. Tags are under Tags."
              Right all' -> do
                let latest = latestOf all'
                forM_ all' $ \r -> pick r.name (r.tag <> maybe "" ((" · " <>) . date c) r.published)
                  ([("Latest", "latest") | Just r.tag == latest] <> [("Pre-release", "pre") | r.prerelease] <> [("Draft", "draft") | r.draft])
                  >>= #append list
                selectFirst
      selectFirst = Gtk.listBoxGetRowAtIndex list 0 >>= mapM_ (\r -> Gtk.listBoxSelectRow list (Just r))
      showRelease latest r = do
        clearBox right
        top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 10]
        label r.name ["title"] >>= #append top
        when (latest == Just r.tag) $ label "Latest" ["pill", "latest"] >>= #append top
        when r.prerelease $ label "Pre-release" ["pill", "pre"] >>= #append top
        by <- label (r.author <> " released this " <> maybe "" (ago c) r.published <> " · " <> r.tag) ["muted", "small"]
        GI.set by [#hexpand := True, #xalign := 1]
        #append top by
        #append right top
        notes <- if T.null (T.strip r.notes) then label "No notes." ["muted"] >>= Gtk.toWidget else Markdown.new r.notes
        #append right notes
        unless (null r.assets) $ do
          label ("Assets · " <> T.pack (show (length r.assets))) ["section"] >>= #append right
          card <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #cssClasses := ["assets"]]
          forM_ r.assets $ \x -> do
            row <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 12, #cssClasses := ["asset"]]
            n <- label x.name ["mono"]
            GI.set n [#hexpand := True, #ellipsize := Pango.EllipsizeModeMiddle]
            #append row n
            label (sizeOf x.size) ["muted", "small"] >>= #append row
            d <- label (if x.downloads > 0 then T.pack (show x.downloads) <> " downloads" else "") ["muted", "small"]
            GI.set d [#widthRequest := 100, #xalign := 1]
            #append row d
            get <- Button.new Button.defaults {Button.label = Just "Download", Button.small = True, Button.tooltip = Just "Opens the download in the browser"}
            void $ GI.on get #clicked (openUri x.url)
            #append row get
            #append card row
          #append right card
        browser <- Button.new Button.defaults {Button.label = Just "Open in browser", Button.small = True}
        GI.set browser [#halign := Gtk.AlignStart]
        void $ GI.on browser #clicked (openUri r.url)
        #append right browser
      showTag t = do
        clearBox right
        label ("Tag " <> t.name) ["title"] >>= #append right
        label (maybe "" (\w -> "Made " <> ago c w) t.at) ["muted", "small"] >>= #append right
        browser <- Button.new Button.defaults {Button.label = Just "Open in browser", Button.small = True}
        GI.set browser [#halign := Gtk.AlignStart]
        void $ GI.on browser #clicked (openUri (a.tagUrl t.name))
        #append right browser
  void $ GI.on list #rowSelected $ \row -> forM_ row $ \r -> do
    i <- fromIntegral <$> Gtk.listBoxRowGetIndex r
    tagsOn <- readIORef showingTags
    readIORef loaded >>= mapM_ (\(rs, ts) -> if tagsOn
      then either (const (pure ())) (\all' -> when (i < length all') (showTag (all' !! i))) ts
      else either (const (pure ())) (\all' -> when (i < length all') (showRelease (latestOf all') (all' !! i))) rs)
  forM_ (zip [False, True] kindButtons) $ \(isTags, b) -> void $ GI.on b #toggled $ do
    active <- GI.get b #active
    when active (writeIORef showingTags isTags >> fill)
  -- j k move, t shows the tags.
  keys <- GI.new Gtk.ShortcutController []
  let move by = do
        current <- Gtk.listBoxGetSelectedRow list >>= maybe (pure (-1)) (fmap fromIntegral . Gtk.listBoxRowGetIndex)
        Gtk.listBoxGetRowAtIndex list (fromIntegral (current + by :: Int)) >>= mapM_ (\r -> Gtk.listBoxSelectRow list (Just r))
  shortcut keys "j" (move 1)
  shortcut keys "k" (move (-1))
  shortcut keys "t" (forM_ (drop 1 kindButtons) (\b -> GI.set b [#active := True]))
  Gtk.widgetAddController page keys

  void $ forkIO $ do
    rs <- try a.releases
    ts <- try a.tags
    let flat r = either (\(e :: SomeException) -> Left (T.pack (show e))) id r
    void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
      writeIORef loaded (Just (flat rs, flat ts))
      fill
      pure False
  Gtk.toWidget page
  where
    pick name meta pills = do
      box <- GI.new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 2, #cssClasses := ["pick"]]
      top <- GI.new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 6]
      n <- label name ["strong"]
      GI.set n [#ellipsize := Pango.EllipsizeModeEnd]
      #append top n
      forM_ pills $ \(text, kind) -> label text ["pill", kind] >>= #append top
      #append box top
      label meta ["muted", "small", "mono"] >>= #append box
      pure box

-- | "18.4 MB", "6 B".
sizeOf :: Int64 -> Text
sizeOf n
  | n < 1024 = T.pack (show n) <> " B"
  | n < 1024 * 1024 = one (fromIntegral n / 1024) <> " KB"
  | otherwise = one (fromIntegral n / (1024 * 1024)) <> " MB"
  where
    one :: Double -> Text
    one x = T.pack (show (fromIntegral (round (x * 10) :: Int) / 10 :: Double))

openUri :: Text -> IO ()
openUri uri = do
  launcher <- Gtk.uriLauncherNew (Just uri)
  Gtk.uriLauncherLaunch launcher (Nothing :: Maybe Gtk.Window) (Nothing :: Maybe Gio.Cancellable) Nothing

label :: Text -> [Text] -> IO Gtk.Label
label text classes = GI.new Gtk.Label [#label := text, #xalign := 0, #cssClasses := classes]

css :: Text
css = mconcat
  [ "box.u-releases { padding: 14px 20px; }"
  , "box.u-releases list.picks { background: transparent; }"
  , "box.u-releases list.picks row { padding: 8px 10px; border-radius: 8px; background: transparent; }"
  , "box.u-releases list.picks row:selected { background: var(--accent-bg); }"
  , "box.u-releases list.picks row:selected label.strong { color: var(--text); }"
  , "box.u-releases label.strong { font-weight: 600; }"
  , "box.u-releases label.small { font-size: 12px; }"
  , "box.u-releases label.pill { padding: 1px 8px; border-radius: 10px; font-size: 11px; font-weight: 600; }"
  , "box.u-releases label.pill.latest { background: var(--success-bg); color: var(--success); }"
  , "box.u-releases label.pill.pre { background: var(--warning-bg); color: var(--warning); }"
  , "box.u-releases label.pill.draft { background: var(--neutral-bg); color: var(--text-muted); }"
  , "box.u-releases box.release { padding: 18px; }"
  , "box.u-releases label.title { font-size: 20px; font-weight: 600; }"
  , "box.u-releases label.section { font-size: 11px; font-weight: 600; color: var(--text-muted); }"
  , "box.u-releases box.assets { border: 1px solid var(--border); border-radius: 8px; }"
  , "box.u-releases box.asset { padding: 8px 12px; border-bottom: 1px solid var(--border-subtle); }"
  , "box.u-releases box.asset:last-child { border-bottom: none; }"
  ]
