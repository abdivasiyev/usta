{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Query parameters as one record: @RecordQueryParam Query :> …@ sends
each field of @Query@ as the parameter of the same name. The client side
of metanchi-api's module of the same name.

A @Maybe@ field is left out when 'Nothing'; @Named "wire" a@ sends the
field under another name, for names Haskell cannot use (@type@) or
spells differently (@review_requested@).
-}
module Usta.Types.Servant.NamedQueryParam
    ( ToQuery (..)
    , Named (..)
    , RecordQueryParam
    )
where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API (ToHttpApiData (..), (:>))
import Servant.Client.Core (HasClient (..), appendToQueryString)

import Data.Text qualified as Text

{- | Names a field's wire parameter when it differs from the Haskell field
name; the value is in 'named'.
-}
type Named :: Symbol -> Type -> Type
newtype Named name a = Named {named :: a}

type ToQuery :: Type -> Constraint
class ToQuery a where
    -- | The parameters, in field order.
    toQuery :: a -> [(Text, Text)]
    default toQuery :: (GToQuery (Rep a), Generic a) => a -> [(Text, Text)]
    toQuery = gtoQuery . from

fieldName :: forall (name :: Symbol). (KnownSymbol name) => Text
fieldName = Text.pack (symbolVal (Proxy @name))

type GToQuery :: (Type -> Type) -> Constraint
class GToQuery f where
    gtoQuery :: f p -> [(Text, Text)]

instance (GToQuery f) => GToQuery (D1 meta f) where
    gtoQuery (M1 x) = gtoQuery x

instance (GToQuery f) => GToQuery (C1 meta f) where
    gtoQuery (M1 x) = gtoQuery x

instance (GToQuery f, GToQuery g) => GToQuery (f :*: g) where
    gtoQuery (f :*: g) = gtoQuery f <> gtoQuery g

-- One selector: the record field name is the wire name unless the field's
-- type says otherwise.
instance
    (GField t, KnownSymbol name) =>
    GToQuery (S1 ('MetaSel ('Just name) su ss ds) (Rec0 t))
    where
    gtoQuery (M1 (K1 x)) = gfield (fieldName @name) x

-- | Encodes one field under the fallback name (the Haskell field name).
type GField :: Type -> Constraint
class GField t where
    gfield :: Text -> t -> [(Text, Text)]

instance {-# OVERLAPPING #-} (ToHttpApiData a) => GField (Maybe a) where
    gfield key = maybe [] (\v -> [(key, toQueryParam v)])

instance {-# OVERLAPPING #-} (KnownSymbol wname, ToHttpApiData a) => GField (Named wname (Maybe a)) where
    gfield _ (Named v) = gfield (fieldName @wname) v

instance {-# OVERLAPPING #-} (KnownSymbol wname, ToHttpApiData a) => GField (Named wname a) where
    gfield _ (Named v) = [(fieldName @wname, toQueryParam v)]

instance {-# OVERLAPPABLE #-} (ToHttpApiData a) => GField a where
    gfield key v = [(key, toQueryParam v)]

type RecordQueryParam :: Type -> Type
data RecordQueryParam a

instance (HasClient m api, ToQuery a) => HasClient m (RecordQueryParam a :> api) where
    type Client m (RecordQueryParam a :> api) = a -> Client m api

    clientWithRoute pm _ request value =
        clientWithRoute pm (Proxy @api) (foldl' add request (toQuery value))
        where
            add r (key, v) = appendToQueryString key (Just (encodeUtf8 v)) r

    hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy @api) f . cl
