{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The lists sync fetches from each account.
module Usta.Types.Resource
    ( Resource (..)
    )
where

import Data.Kind (Type)
import Data.Text (Text)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)

import Usta.Types.Parse (Parse (..), Parsed (..))

-- | One list sync fetches; each has its own cursor per account.
type Resource :: Type
data Resource = Notifications | PullRequests | Issues | Repositories
    deriving stock (Bounded, Enum, Eq, Show)
    deriving (PersistField, PersistFieldSql) via Parsed Resource

instance Parse Resource where
    type Raw Resource = Text
    parse raw = case raw of
        "notifications" -> Right Notifications
        "pull_requests" -> Right PullRequests
        "issues" -> Right Issues
        "repositories" -> Right Repositories
        _ -> Left "a resource: notifications, pull_requests, issues or repositories"
    unparse resource = case resource of
        Notifications -> "notifications"
        PullRequests -> "pull_requests"
        Issues -> "issues"
        Repositories -> "repositories"
