{- | Desktop notifications: on macOS through the system's notification
center (GLib's way there no longer shows), elsewhere through GLib.
-}
module Usta.UI.Notify
    ( available
    , start
    , send
    )
where

import Data.Text (Text)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))

import Data.Text qualified as T
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

import Usta.UI.Platform qualified as Platform

{- | Whether the desktop shows usta's notifications: on macOS only for
usta.app.
-}
available :: IO Bool
available
    | Platform.current == Platform.MacOS = (/= 0) <$> c_inBundle
    | otherwise = pure True

-- | Asks macOS for permission, once, before the first one.
start :: IO ()
start
    | Platform.current == Platform.MacOS = c_start
    | otherwise = pure ()

-- | A notification; a newer one with the same @key@ replaces it.
send :: Gtk.Application -> Text -> Text -> Text -> IO ()
send app key title body
    | Platform.current == Platform.MacOS =
        withCString (T.unpack key) $ \k -> withCString (T.unpack title) $ \t -> withCString (T.unpack body) $ \b -> c_notify k t b
    | otherwise = do
        note <- Gio.notificationNew title
        Gio.notificationSetBody note (Just body)
        Gio.applicationSendNotification app (Just key) note

foreign import ccall unsafe "usta_in_bundle" c_inBundle :: IO CInt

foreign import ccall safe "usta_notifications_start" c_start :: IO ()

foreign import ccall safe "usta_notify" c_notify :: CString -> CString -> CString -> IO ()
