{-# LANGUAGE OverloadedStrings #-}

-- | What the user chose in Settings, kept in a small JSON file next to
-- the database. A file that is missing, or that usta cannot read, gives
-- the defaults; a value usta does not know is the default for that one.
module Usta.Preferences
  ( Preferences (..)
  , Interval (..)
  , Theme (..)
  , defaults
  , seconds
  , file
  , load
  , save
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecodeFileStrict, encodeFile, object, withObject, (.!=), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

import Usta.DB (databaseFile)

-- | How often sync asks the forges.
type Interval :: Type
data Interval
  = EveryMinute
  | EveryTwoMinutes
  | EveryFiveMinutes
  | EveryFifteenMinutes
  | Manually
  deriving stock (Eq, Show, Enum, Bounded)

-- | 'Nothing' for 'Manually'.
seconds :: Interval -> Maybe NominalDiffTime
seconds interval = case interval of
  EveryMinute -> Just 60
  EveryTwoMinutes -> Just 120
  EveryFiveMinutes -> Just 300
  EveryFifteenMinutes -> Just 900
  Manually -> Nothing

type Theme :: Type
data Theme = System | Light | Dark
  deriving stock (Eq, Show, Enum, Bounded)

type Preferences :: Type
data Preferences = Preferences
  { interval :: Interval
  , theme :: Theme
  , notify :: Bool
    -- ^ Desktop notifications for review requests, mentions and
    -- failed checks.
  , onlyInBackground :: Bool
    -- ^ Only while the window is not the active one.
  , openLinks :: Bool
    -- ^ Forge links in comments, the palette or the command line open in
    -- usta; off, they open in the browser.
  , checkUpdates :: Bool
    -- ^ Once a day, whether a newer usta is out.
  }
  deriving stock (Eq, Show)

defaults :: Preferences
defaults = Preferences
  { interval = EveryMinute
  , theme = System
  , notify = True
  , onlyInBackground = False
  , openLinks = True
  , checkUpdates = True
  }

-- | Each choice is stored by a name, so reordering constructors changes
-- nothing on disk.
names :: (Show a, Enum a, Bounded a) => [(Text, a)]
names = [(T.pack (show x), x) | x <- [minBound .. maxBound]]

named :: (Show a, Enum a, Bounded a) => a -> Maybe Value -> Parser a
named fallback value = pure $ case value of
  Just (String t) | Just x <- lookup t names -> x
  _ -> fallback

instance ToJSON Preferences where
  toJSON p = object
    [ "interval" .= show p.interval
    , "theme" .= show p.theme
    , "notify" .= p.notify
    , "onlyInBackground" .= p.onlyInBackground
    , "openLinks" .= p.openLinks
    , "checkUpdates" .= p.checkUpdates
    ]

instance FromJSON Preferences where
  parseJSON = withObject "Preferences" $ \o -> do
    interval <- o .:? "interval" >>= named defaults.interval
    theme <- o .:? "theme" >>= named defaults.theme
    notify <- o .:? "notify" .!= defaults.notify
    onlyInBackground <- o .:? "onlyInBackground" .!= defaults.onlyInBackground
    openLinks <- o .:? "openLinks" .!= defaults.openLinks
    checkUpdates <- o .:? "checkUpdates" .!= defaults.checkUpdates
    pure Preferences {interval = interval, theme = theme, notify = notify, onlyInBackground = onlyInBackground, openLinks = openLinks, checkUpdates = checkUpdates}

-- | @preferences.json@ in the folder that has the database.
file :: IO FilePath
file = (</> "preferences.json") . takeDirectory <$> databaseFile

load :: FilePath -> IO Preferences
load path = do
  found <- try (eitherDecodeFileStrict path)
  pure $ case found of
    Right (Right p) -> p
    Right (Left _) -> defaults
    Left (_ :: IOException) -> defaults

save :: FilePath -> Preferences -> IO ()
save path p = do
  createDirectoryIfMissing True (takeDirectory path)
  encodeFile path p
