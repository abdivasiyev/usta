{- | The links macOS opens with usta (its @usta://@ scheme), delivered as
Apple Events; elsewhere links come as arguments and this does nothing.
-}
module Usta.UrlEvents
    ( listen
    )
where

import Data.Text (Text)
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (FunPtr)

import Data.Text qualified as T

{- | @each@ runs on the main thread for every link; call it once, before
the app runs. The callback lives as long as usta.
-}
listen :: (Text -> IO ()) -> IO ()
listen each = wrap (\c -> peekCString c >>= each . T.pack) >>= c_listen

foreign import ccall "wrapper"
    wrap :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall unsafe "usta_listen_urls"
    c_listen :: FunPtr (CString -> IO ()) -> IO ()
