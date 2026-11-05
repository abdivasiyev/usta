{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | A person's avatar: their initials at once, and their picture from the
-- forge once it has loaded. Each picture is fetched once while usta runs,
-- for every place that shows the person, and kept on disk for a week.
module Usta.UI.Components.Avatar
  ( Place (..)
  , new
  , person
  , css
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.GI.Base as GI
import Data.GI.Base (AttrOp ((:=)))
import Data.Int (Int32)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime, nominalDay)
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (statusCode)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesFileExist, getHomeDirectory, getModificationTime, getXdgDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Usta.UI.Platform as Platform

-- | Where a person's account is: the forge's host, and whether it is
-- GitHub (github.com or a server), whose pictures are found otherwise.
type Place :: Type
data Place = Place {github :: Bool, host :: Text}
  deriving stock (Eq, Ord, Show)

-- | @new 32 "AA"@: initials only, for who has no account, as a commit's
-- author from a mirror.
new :: Int32 -> Text -> IO Gtk.Label
new size initials =
  GI.new Gtk.Label [ #label := initials
                   , #widthRequest := size
                   , #heightRequest := size
                   , #halign := Gtk.AlignCenter
                   , #valign := Gtk.AlignCenter
                   , #cssClasses := ["u-avatar"]
                   ]

-- | The avatar of @login@ on @place@'s forge: initials until the picture
-- comes; without a place, or when the forge has none, they stay.
person :: Int32 -> Maybe Place -> Text -> IO Gtk.Widget
person size place login = do
  box <- GI.new Gtk.Box [ #widthRequest := size, #heightRequest := size
                        , #halign := Gtk.AlignCenter, #valign := Gtk.AlignCenter, #cssClasses := ["u-avatar-box"] ]
  initials <- new size (T.toUpper (T.take 2 login))
  #append box initials
  forM_ place $ \p -> unless (T.null login) $
    picture (urlOf p login (size * 2)) $ \texture -> do
      -- An image keeps to its pixel size; the picture is twice it, for
      -- sharp screens.
      shown <- GI.new Gtk.Image [ #paintable := texture, #pixelSize := size, #cssClasses := ["u-avatar-picture"] ]
      Gtk.widgetSetOverflow shown Gtk.OverflowHidden
      Gtk.boxRemove box initials
      #append box shown
  Gtk.toWidget box

-- | GitHub serves "/<login>.png" and redirects to the picture; Forgejo
-- "/user/avatar/<login>/<size>".
urlOf :: Place -> Text -> Int32 -> Text
urlOf p login size
  | p.github = "https://" <> p.host <> "/" <> login <> ".png?size=" <> T.pack (show size)
  | otherwise = "https://" <> p.host <> "/user/avatar/" <> login <> "/" <> T.pack (show size)

-- | What is known of each picture: loading (with who waits for it), here,
-- or missing.
type Known :: Type
data Known = Loading [Gdk.Texture -> IO ()] | Here Gdk.Texture | Missing

known :: IORef (Map.Map Text Known)
known = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE known #-}

-- | Runs @use@ on the main thread with the picture at @url@, now if it is
-- here, else once it loads; not at all when it cannot.
picture :: Text -> (Gdk.Texture -> IO ()) -> IO ()
picture url use = do
  step <- atomicModifyIORef' known $ \m -> case Map.lookup url m of
    Just (Here t) -> (m, Right t)
    Just Missing -> (m, Left False)
    Just (Loading waiting) -> (Map.insert url (Loading (use : waiting)) m, Left False)
    Nothing -> (Map.insert url (Loading [use]) m, Left True)
  case step of
    Right t -> use t
    Left False -> pure ()
    Left True -> void $ forkIO $ do
      bytes <- either (\(_ :: SomeException) -> Nothing) id <$> try (fetch url)
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
        texture <- case bytes of
          Just b -> either (\(_ :: SomeException) -> Nothing) Just <$> try (GLib.bytesNew (Just b) >>= Gdk.textureNewFromBytes)
          Nothing -> pure Nothing
        waiting <- atomicModifyIORef' known $ \m ->
          let before = case Map.lookup url m of
                Just (Loading w) -> w
                _ -> []
           in (Map.insert url (maybe Missing Here texture) m, before)
        forM_ texture $ \t -> mapM_ ($ t) (reverse waiting)
        pure False

-- | The picture's bytes, from the disk when fetched in the last week.
fetch :: Text -> IO (Maybe B.ByteString)
fetch url = do
  folder <- cacheFolder
  createDirectoryIfMissing True folder
  name <- fromMaybe "avatar" <$> GLib.computeChecksumForString GLib.ChecksumTypeSha1 url (-1)
  let file = folder </> T.unpack name
  cached <- doesFileExist file
  fresh <- if cached
    then (\at now -> diffUTCTime now at < 7 * nominalDay) <$> getModificationTime file <*> getCurrentTime
    else pure False
  if fresh
    then Just <$> B.readFile file
    else do
      manager <- getGlobalManager
      request <- HTTP.parseRequest (T.unpack url)
      response <- HTTP.httpLbs request manager
      if statusCode (HTTP.responseStatus response) /= 200
        then pure Nothing
        else do
          let b = BL.toStrict (HTTP.responseBody response)
          when (B.length b > 0) $ B.writeFile file b
          pure (Just b)

cacheFolder :: IO FilePath
cacheFolder = case Platform.current of
  Platform.MacOS -> (</> "Library" </> "Caches" </> "usta" </> "avatars") <$> getHomeDirectory
  _ -> (</> "avatars") <$> getXdgDirectory XdgCache "usta"

css :: Text
css = mconcat
  [ "label.u-avatar {"
  , "  border-radius: 999px; background: var(--accent-bg); color: var(--accent-soft);"
  , "  font-weight: 600; font-size: 12px;"
  , "}"
  , "image.u-avatar-picture { border-radius: 999px; }"
  ]
