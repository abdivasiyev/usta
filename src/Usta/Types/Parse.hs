{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Parsing domain types out of their raw representations: what a forge
sends, what a column holds, what the user types.

A domain type is built by a parser that can fail, and by nothing else.
Holding a value of it is then evidence that the raw data was checked, not
a promise that someone remembered to check it.

== Writing a domain type

Give the type a 'Parse' instance and derive its wire instances through
'Parsed':

@
newtype Host = Host Text
  deriving stock (Eq, Ord, Show)
  deriving (FromJSON, ToJSON, PersistField, PersistFieldSql) via Parsed Host

instance Parse Host where
  type Raw Host = Text
  parse raw = if validHost raw then Right (Host raw) else Left "a host name such as github.com"
  unparse (Host raw) = raw
@

and export it abstractly: @Host@, never @Host (..)@. A public constructor
makes the parser bypassable, and a bypassable parser proves nothing.

== Two rules

* @'parse' . 'unparse' ≡ Right@ for every instance. A type whose parser
  rejects what its own 'unparse' produced cannot round-trip through the
  cache.

* Never use @derivePersistField@. It stores the 'Show' output, so a column
  would hold @Host "github.com"@ rather than the host. Deriving through
  'Parsed' stores the raw value.
-}
module Usta.Types.Parse
    ( Parse (..)
    , Parsed (..)
    , parseM
    , isPathSegment
    )
where

import Control.Monad ((>=>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import Data.Text qualified as Text

{- | A type parsed out of a simpler representation. 'Raw' is what crosses
the wire and sits in the column, normally 'Text'. 'parse' is the only way
in; 'unparse' is the only way out.
-}
type Parse :: Type -> Constraint
class Parse a where
    type Raw a :: Type

    {- | Rejects with a message describing what was expected, not what was
    given: echoing input back is how tokens end up in logs.
    -}
    parse :: Raw a -> Either Text a

    unparse :: a -> Raw a

{- | 'parse' in any 'MonadFail', for the aeson and http-api-data instances
and for @do@ blocks.
-}
parseM :: (MonadFail m, Parse a) => Raw a -> m a
parseM = either (fail . Text.unpack) pure . parse

{- | What forges allow in one path segment: owners, repository names and
logins. Shared by the parsers of those types.
-}
isPathSegment :: Text -> Bool
isPathSegment segment =
    not (Text.null segment)
        && segment /= "."
        && segment /= ".."
        && Text.all (\c -> isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("._-" :: String)) segment

{- | Deriving-via carrier: supplies every wire instance from the one 'Parse'
instance. It is representationally equal to @a@, so
@deriving ... via Parsed a@ coerces these instances onto @a@ itself. It is
not meant to appear in a signature.
-}
type Parsed :: Type -> Type
newtype Parsed a = Parsed a

-- | The column holds the bare 'Raw' value.
instance (Parse a, PersistField (Raw a)) => PersistField (Parsed a) where
    toPersistValue (Parsed a) = toPersistValue (unparse a)
    fromPersistValue = fromPersistValue >=> fmap Parsed . parse

instance (Parse a, PersistFieldSql (Raw a)) => PersistFieldSql (Parsed a) where
    sqlType _ = sqlType (Proxy @(Raw a))

instance (FromJSON (Raw a), Parse a) => FromJSON (Parsed a) where
    parseJSON = parseJSON >=> fmap Parsed . parseM

instance (Parse a, ToJSON (Raw a)) => ToJSON (Parsed a) where
    toJSON (Parsed a) = toJSON (unparse a)

-- | Path pieces, query parameters and headers in servant APIs.
instance (FromHttpApiData (Raw a), Parse a) => FromHttpApiData (Parsed a) where
    parseUrlPiece = parseUrlPiece >=> fmap Parsed . parse

instance (Parse a, ToHttpApiData (Raw a)) => ToHttpApiData (Parsed a) where
    toUrlPiece (Parsed a) = toUrlPiece (unparse a)
