-- | A repository's pull requests or issues a page at a time, as its tabs
-- ask for them: which ones, in what order, and the page the forge gave.
module Usta.Types.Listing
  ( Which (..)
  , Order (..)
  , Listing (..)
  , Paged (..)
  ) where

import Data.Kind (Type)
import Data.Text (Text)

-- | Issues take only 'Opened', 'Shut' and 'Every'.
type Which :: Type
data Which = Opened | Drafts | Merged | Shut | Every
  deriving stock (Eq, Show, Enum, Bounded)

type Order :: Type
data Order = RecentlyUpdated | Newest | Oldest
  deriving stock (Eq, Show, Enum, Bounded)

type Listing :: Type
data Listing = Listing {which :: Which, order :: Order}
  deriving stock (Eq, Show)

-- | One page: its items, how many there are in all when the forge says,
-- and where the next page starts, if there is one.
type Paged :: Type -> Type
data Paged a = Paged
  { items :: [a]
  , total :: Maybe Int
  , next :: Maybe Text
  }
  deriving stock (Eq, Show)

instance Functor Paged where
  fmap f p = Paged {items = map f p.items, total = p.total, next = p.next}
