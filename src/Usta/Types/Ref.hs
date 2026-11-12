-- | Where a pull request or issue lives.
module Usta.Types.Ref
    ( Ref (..)
    )
where

import Data.Kind (Type)

import Usta.Types.Host (Host)
import Usta.Types.Number (Number)
import Usta.Types.RepoName (RepoName)

-- | One pull request or issue, wherever it lives.
type Ref :: Type
data Ref = Ref
    { host :: Host
    , repo :: RepoName
    , number :: Number
    }
    deriving stock (Eq, Ord, Show)
