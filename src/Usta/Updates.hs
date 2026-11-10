{-# LANGUAGE OverloadedStrings #-}

{- | Whether a newer usta is out: GitHub's latest release of
abdivasiyev/usta, read through its API. Nothing about the user is sent;
the request carries only usta's name and version.
-}
module Usta.Updates
    ( Release (..)
    , latest
    , newer
    , current
    )
where

import Control.Exception (SomeException, try)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Version (Version, parseVersion, showVersion)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (statusCode)
import Servant.API (Get, Header', JSON, NamedRoutes, Required, Strict, (:-), (:>))
import Servant.Client (AsClientT, BaseUrl (..), ClientError (..), ClientM, ResponseF (..), Scheme (Https), client, mkClientEnv, runClientM)
import Text.ParserCombinators.ReadP (eof, readP_to_S)

import Data.Text qualified as T

import Paths_usta (version)

type Release :: Type
data Release = Release
    { version :: Version
    , page :: Text
    -- ^ The release on GitHub: what changed, and the downloads.
    }

-- | A release whose tag is not "v" and a version is not one usta reads.
instance FromJSON Release where
    parseJSON = withObject "Release" $ \o -> do
        tag <- o .: "tag_name"
        url <- o .: "html_url"
        case [v | (v, "") <- readP_to_S (parseVersion <* eof) (T.unpack (T.dropWhile (== 'v') tag))] of
            v : _ -> pure Release {version = v, page = url}
            [] -> fail ("Not a version: " <> T.unpack tag)

-- | The one route of GitHub's API usta asks; GitHub wants a User-Agent.
type Routes :: Type -> Type
data Routes mode = Routes
    { latestRelease ::
        mode
            :- "repos"
                :> "abdivasiyev"
                :> "usta"
                :> "releases"
                :> "latest"
                :> Header' '[Required, Strict] "User-Agent" Text
                :> Get '[JSON] Release
    }
    deriving stock (Generic)

api :: Routes (AsClientT ClientM)
api = client (Proxy @(NamedRoutes Routes))

{- | The latest release, 'Nothing' before the first one, or why usta
could not tell.
-}
latest :: IO (Either Text (Maybe Release))
latest = do
    asked <- try $ do
        manager <- newTlsManager
        runClientM
            (api.latestRelease ("usta/" <> T.pack (showVersion version)))
            (mkClientEnv manager (BaseUrl Https "api.github.com" 443 ""))
    pure $ case asked of
        Left (e :: SomeException) -> Left (T.pack (takeWhile (/= '\n') (show e)))
        Right (Left (FailureResponse _ r)) | statusCode r.responseStatusCode == 404 -> Right Nothing
        Right (Left _) -> Left "GitHub did not say which release is the latest."
        Right (Right r) -> Right (Just r)

-- | Whether @r@ is newer than the usta running.
newer :: Release -> Bool
newer r = r.version > version

current :: Text
current = T.pack (showVersion version)
