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
  , Managed
  , managedFile
  , loadManaged
  , fixedKeys
  , effective
  , keeping
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecodeFileStrict, encodeFile, object, withObject, (.!=), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Control.Applicative ((<|>))
import System.Directory (XdgDirectory (XdgConfig), XdgDirectoryList (XdgConfigDirs), createDirectoryIfMissing, getXdgDirectory, getXdgDirectoryList)
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

-- | Choices fixed outside usta, in @~/.config/usta/config.json@ (Home
-- Manager's @programs.usta.settings@ writes it) or, for every user, in
-- @/etc/xdg/usta/config.json@ (the NixOS and nix-darwin modules'): the
-- same names as @preferences.json@, each one that is there winning over
-- the user's, and the user's file over the system's. usta never writes
-- them.
type Managed :: Type
data Managed = Managed
  { interval :: Maybe Interval
  , theme :: Maybe Theme
  , notify :: Maybe Bool
  , onlyInBackground :: Maybe Bool
  , openLinks :: Maybe Bool
  , checkUpdates :: Maybe Bool
  }

instance FromJSON Managed where
  parseJSON = withObject "Managed" $ \o -> do
    interval <- o .:? "interval" >>= traverse (fixedAs "interval")
    theme <- o .:? "theme" >>= traverse (fixedAs "theme")
    notify <- o .:? "notify"
    onlyInBackground <- o .:? "onlyInBackground"
    openLinks <- o .:? "openLinks"
    checkUpdates <- o .:? "checkUpdates"
    pure Managed {interval, theme, notify, onlyInBackground, openLinks, checkUpdates}
    where
      -- A name usta does not know is an error here: the user wrote it in
      -- their configuration and should hear it is wrong.
      fixedAs :: (Show a, Enum a, Bounded a) => Text -> Text -> Parser a
      fixedAs key t = maybe (fail (T.unpack ("Unknown " <> key <> ": " <> t))) pure (lookup t names)

-- | The first one's choices, and the second's where the first has none.
instance Semigroup Managed where
  a <> b = Managed
    { interval = a.interval <|> b.interval
    , theme = a.theme <|> b.theme
    , notify = a.notify <|> b.notify
    , onlyInBackground = a.onlyInBackground <|> b.onlyInBackground
    , openLinks = a.openLinks <|> b.openLinks
    , checkUpdates = a.checkUpdates <|> b.checkUpdates
    }

instance Monoid Managed where
  mempty = Managed Nothing Nothing Nothing Nothing Nothing Nothing

-- | The user's file, which Home Manager writes.
managedFile :: IO FilePath
managedFile = (</> "config.json") <$> getXdgDirectory XdgConfig "usta"

-- | The user's file over the system's ones; a missing file fixes
-- nothing. A wrong one is a 'Left', for the caller to log, and then
-- nothing is fixed.
loadManaged :: IO (Either Text Managed)
loadManaged = do
  own <- managedFile
  system <- map (</> "usta" </> "config.json") <$> getXdgDirectoryList XdgConfigDirs
  fmap mconcat . sequence <$> mapM readOne (own : system)
  where
    readOne path = do
      found <- try (eitherDecodeFileStrict path)
      pure $ case found of
        Right (Right m) -> Right m
        Right (Left e) -> Left (T.pack path <> ": " <> T.pack e)
        Left (_ :: IOException) -> Right mempty

-- | The names of the choices fixed, as Settings marks them.
fixedKeys :: Managed -> [Text]
fixedKeys m =
  [k | (k, True) <- [ ("interval", has m.interval), ("theme", has m.theme), ("notify", has m.notify)
                    , ("onlyInBackground", has m.onlyInBackground), ("openLinks", has m.openLinks)
                    , ("checkUpdates", has m.checkUpdates) ]]
  where
    has :: Maybe a -> Bool
    has = maybe False (const True)

-- | The user's choices with the fixed ones over them.
over :: Managed -> Preferences -> Preferences
over m p = Preferences
  { interval = fromMaybe p.interval m.interval
  , theme = fromMaybe p.theme m.theme
  , notify = fromMaybe p.notify m.notify
  , onlyInBackground = fromMaybe p.onlyInBackground m.onlyInBackground
  , openLinks = fromMaybe p.openLinks m.openLinks
  , checkUpdates = fromMaybe p.checkUpdates m.checkUpdates
  }

-- | What usta goes by: @preferences.json@ under @config.json@.
effective :: IO Preferences
effective = do
  own <- file >>= load
  either (const own) (`over` own) <$> loadManaged

-- | What to save when Settings changed @new@: the fixed choices keep the
-- user's own from @before@, so dropping one from the configuration
-- brings back what the user had.
keeping :: Managed -> Preferences -> Preferences -> Preferences
keeping m before new = Preferences
  { interval = maybe new.interval (const before.interval) m.interval
  , theme = maybe new.theme (const before.theme) m.theme
  , notify = maybe new.notify (const before.notify) m.notify
  , onlyInBackground = maybe new.onlyInBackground (const before.onlyInBackground) m.onlyInBackground
  , openLinks = maybe new.openLinks (const before.openLinks) m.openLinks
  , checkUpdates = maybe new.checkUpdates (const before.checkUpdates) m.checkUpdates
  }
