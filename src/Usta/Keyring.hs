{-# LANGUAGE OverloadedStrings #-}

{- | Where account tokens live: the system keyring, never the cache. One
token per account, under the service "usta", keyed by login and host.

'system' calls each platform's own keyring API (cbits/keyring.c): on
macOS the item then belongs to usta, and other programs reading it get
the system's prompt. The Secret Service on Linux has no such check: any
program in the session can read an unlocked keyring.
-}
module Usta.Keyring
    ( Keyring (..)
    , KeyringError (..)
    , system
    , memory
    )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (void, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)

import Data.ByteString qualified as BS
import Data.Text qualified as T

import Usta.Types.Host (Host)
import Usta.Types.Login (Login)
import Usta.Types.Parse (parse, unparse)
import Usta.Types.Token (Token)

type Keyring :: Type
data Keyring = Keyring
    { save :: Host -> Login -> Token -> IO ()
    -- ^ Replaces the account's token if it has one.
    , load :: Host -> Login -> IO (Maybe Token)
    , forget :: Host -> Login -> IO ()
    -- ^ Nothing happens when the account has no token.
    }

{- | The keyring refused, or is missing or locked. The message is the
system's, and never holds the token.
-}
type KeyringError :: Type
newtype KeyringError = KeyringError Text
    deriving stock (Show)

instance Exception KeyringError

system :: Keyring
system =
    Keyring
        { save = \host login token -> do
            let
                secret = encodeUtf8 (unparse token)
            when (BS.elem 0 secret) $ throwIO (KeyringError "a token cannot hold a NUL character")
            withText (account host login) $ \name ->
                withText ("usta: " <> account host login) $ \label ->
                    BS.useAsCString secret $ \value ->
                        void (call (c_save name label value))
        , load = \host login ->
            withText (account host login) $ \name ->
                alloca $ \out -> do
                    poke out nullPtr
                    found <- call (c_load name out)
                    if not found
                        then pure Nothing
                        else do
                            value <- peek out
                            bytes <- BS.packCString value
                            c_free value
                            case decodeUtf8' bytes of
                                Right text | Right token <- parse text -> pure (Just token)
                                _ -> throwIO (KeyringError "the keyring holds something that is not a token")
        , forget = \host login ->
            withText (account host login) $ \name -> void (call (c_forget name))
        }

-- | A keyring that lasts as long as the process, for tests and samples.
memory :: IO Keyring
memory = do
    ref <- newIORef []
    pure
        Keyring
            { save = \host login token ->
                atomicModifyIORef' ref (\ts -> ((account host login, token) : filter ((/= account host login) . fst) ts, ()))
            , load = \host login -> lookup (account host login) <$> readIORef ref
            , forget = \host login ->
                atomicModifyIORef' ref (\ts -> (filter ((/= account host login) . fst) ts, ()))
            }

-- | "abdivasiyev@github.com": the item's account name.
account :: Host -> Login -> Text
account host login = unparse login <> "@" <> unparse host

withText :: Text -> (CString -> IO a) -> IO a
withText = BS.useAsCString . encodeUtf8

{- | Runs a keyring call: 'True' when it found or changed the item, 'False'
when there was none, 'KeyringError' when it failed.
-}
call :: (Out -> IO CInt) -> IO Bool
call action = alloca $ \errorOut -> do
    poke errorOut nullPtr
    status <- action errorOut
    case status of
        0 -> pure True
        1 -> pure False
        _ -> do
            message <- peek errorOut
            text <-
                if message == nullPtr
                    then pure "keyring error"
                    else do
                        bytes <- BS.packCString message
                        c_free message
                        pure (either (const "keyring error") id (decodeUtf8' bytes))
            throwIO (KeyringError (T.strip text))

-- | Where a call writes a string it allocated: the secret, or an error.
type Out :: Type
type Out = Ptr CString

foreign import ccall safe "usta_keyring_save"
    c_save :: CString -> CString -> CString -> Out -> IO CInt

foreign import ccall safe "usta_keyring_load"
    c_load :: CString -> Out -> Out -> IO CInt

foreign import ccall safe "usta_keyring_forget"
    c_forget :: CString -> Out -> IO CInt

foreign import ccall safe "usta_keyring_free"
    c_free :: CString -> IO ()
