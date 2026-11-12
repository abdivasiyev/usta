{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Which forge software a server runs.
module Usta.Types.Forge
    ( Kind (..)
    )
where

import Data.Aeson (FromJSON)
import Data.Kind (Type)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Types.Parse (Parse (..), Parsed (..))

-- | Which forge software a server runs; decides how usta talks to it.
type Kind :: Type
data Kind = GitHub | GitLab | Forgejo
    deriving stock (Bounded, Enum, Eq, Show)
    deriving (FromJSON, PersistField, PersistFieldSql) via Parsed Kind

instance Parse Kind where
    type Raw Kind = Text
    parse raw = case raw of
        "github" -> Right GitHub
        "gitlab" -> Right GitLab
        "forgejo" -> Right Forgejo
        _ -> Left "a forge: github, gitlab or forgejo"
    unparse kind = case kind of
        GitHub -> "github"
        GitLab -> "gitlab"
        Forgejo -> "forgejo"
