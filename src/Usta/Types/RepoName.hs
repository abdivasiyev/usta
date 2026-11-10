{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | A repository's full name on its forge.
module Usta.Types.RepoName
    ( RepoName
    , owner
    , shortName
    )
where

import Data.Aeson (FromJSON)
import Data.Kind (Type)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Data.Text qualified as Text

import Usta.Types.Parse (Parse (..), Parsed (..), isPathSegment)

{- | A repository's full name, @owner/name@, as the forge spells it.

The fields stay private: exporting them would let a record update skip
the parser. Read them with 'owner' and 'shortName'.
-}
type RepoName :: Type
data RepoName = RepoName
    { repoOwner :: Text
    -- ^ The user or organization: @abdivasiyev@.
    , repoShortName :: Text
    -- ^ The repository within it: @usta@.
    }
    deriving stock (Eq, Ord, Show)
    deriving (FromJSON, PersistField, PersistFieldSql) via Parsed RepoName

instance Parse RepoName where
    type Raw RepoName = Text
    parse raw = case Text.splitOn "/" raw of
        [o, n] | isPathSegment o && isPathSegment n -> Right RepoName {repoOwner = o, repoShortName = n}
        _ -> Left "a repository as owner/name, such as abdivasiyev/usta"
    unparse r = r.repoOwner <> "/" <> r.repoShortName

-- | The user or organization that owns the repository.
owner :: RepoName -> Text
owner = (.repoOwner)

-- | The name without the owner, as lists show it: @usta@.
shortName :: RepoName -> Text
shortName = (.repoShortName)
