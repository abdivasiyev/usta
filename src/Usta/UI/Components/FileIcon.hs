{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Each file's and folder's icon by its name, from the Material Icon
-- Theme (MIT, in @data/file-icons@): a known name first (README,
-- Dockerfile), then the longest known extension, else the plain file. The
-- light theme takes the theme's light variants where it has them.
module Usta.UI.Components.FileIcon
  ( file
  , folder
  , fileIcon
  , folderIcon
  ) where

import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.:))
import qualified Data.ByteString as B
import Data.FileEmbed (embedDir)
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import System.FilePath (takeBaseName, takeExtension)
import System.IO.Unsafe (unsafePerformIO)

-- | A file's icon at @size@ pixels; @path@ may be the whole path.
file :: Int32 -> Text -> IO Gtk.Image
file size path = do
  light <- lightTheme
  image size (fileIcon light path)

-- | A folder's icon, open or closed.
folder :: Int32 -> Bool -> Text -> IO Gtk.Image
folder size open path = do
  light <- lightTheme
  image size (folderIcon light open path)

baseName :: Text -> Text
baseName = T.toLower . T.takeWhileEnd (/= '/') . T.dropWhileEnd (== '/')

-- * Which icon

-- | The theme's names for icons, as its map has them.
type Names :: Type
data Names = Names
  { extensions :: Map.Map Text Text
  , files :: Map.Map Text Text
  , folders :: Map.Map Text Text
  , openFolders :: Map.Map Text Text
  }

-- | Keys lowercase, as the names they are looked up by: the map has
-- ".Rhistory" and "Dockerfile".
instance FromJSON Names where
  parseJSON = withObject "names" $ \o ->
    Names <$> keys o "fileExtensions" <*> keys o "fileNames" <*> keys o "folderNames" <*> keys o "folderNamesExpanded"
    where
      keys o k = Map.mapKeys T.toLower <$> o .: k

type Theme :: Type
data Theme = Theme {dark :: Names, light :: Names}

instance FromJSON Theme where
  parseJSON v = Theme <$> parseJSON v <*> withObject "theme" (.: "light") v

-- | The map, read once; a broken one leaves every file plain.
theme :: Theme
theme = either (const (Theme none none)) id (eitherDecodeStrict (fromMaybe "{}" (lookup "map.json" embedded)))
  where
    none = Names Map.empty Map.empty Map.empty Map.empty
{-# NOINLINE theme #-}

-- | The names to look in: the light ones first in the light theme.
lookIn :: Bool -> [Names]
lookIn isLight = if isLight then [theme.light, theme.dark] else [theme.dark]

-- | The icon's name for a file, by its path: its name, else its
-- extensions, longest first; "file" when the map has neither.
fileIcon :: Bool -> Text -> Text
fileIcon isLight path = fromMaybe "file" $ listToMaybe $
  mapMaybe (Map.lookup name . (.files)) names
    <> [icon | ext <- extensionsOf name, Just icon <- map (Map.lookup ext . (.extensions)) names]
  where
    names = lookIn isLight
    name = baseName path

-- | "a.test.ts" has "test.ts" and then "ts".
extensionsOf :: Text -> [Text]
extensionsOf name = map (T.intercalate ".") (drop 1 (tailsOf (T.splitOn "." name)))
  where
    tailsOf xs = case xs of
      [] -> []
      _ : rest -> xs : tailsOf rest

-- | The icon's name for a folder, open or closed, by its path.
folderIcon :: Bool -> Bool -> Text -> Text
folderIcon isLight open path = fromMaybe (if open then "folder-open" else "folder") $ listToMaybe $
  mapMaybe (Map.lookup (baseName path) . (if open then (.openFolders) else (.folders))) (lookIn isLight)

lightTheme :: IO Bool
lightTheme = Gtk.settingsGetDefault >>= \case
  Nothing -> pure True
  Just s -> (/= Gtk.InterfaceColorSchemeDark) <$> GI.get s #gtkInterfaceColorScheme

-- * Drawing

embedded :: [(FilePath, B.ByteString)]
embedded = $(embedDir "data/file-icons")

svgs :: Map.Map Text B.ByteString
svgs = Map.fromList [(T.pack (takeBaseName p), b) | (p, b) <- embedded, takeExtension p == ".svg"]
{-# NOINLINE svgs #-}

-- | Each icon is parsed once and shared by every row that shows it.
loaded :: IORef (Map.Map Text Gdk.Paintable)
loaded = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE loaded #-}

image :: Int32 -> Text -> IO Gtk.Image
image size name = do
  paintable <- paintableOf name
  GI.new Gtk.Image [ #paintable := paintable, #pixelSize := size
                   , #halign := Gtk.AlignCenter, #valign := Gtk.AlignCenter, #cssClasses := ["u-file-icon"] ]

paintableOf :: Text -> IO Gdk.Paintable
paintableOf name = do
  have <- Map.lookup name <$> atomicModifyIORef' loaded (\m -> (m, m))
  case have of
    Just p -> pure p
    Nothing -> do
      let bytes = fromMaybe (fromMaybe "" (Map.lookup "file" svgs)) (Map.lookup name svgs)
      p <- GLib.bytesNew (Just bytes) >>= Gtk.svgNewFromBytes >>= Gdk.toPaintable
      atomicModifyIORef' loaded (\m -> (Map.insert name p m, ()))
      pure p
