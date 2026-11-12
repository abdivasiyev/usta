{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The @pull_request_involvements@ table: how the account a pull request
was synced for is involved in it, one row per way. Deleted with the pull
request.
-}
module Usta.DB.Schema.PullRequestInvolvement
    ( PullRequestInvolvement (..)
    , PullRequestInvolvementId
    , EntityField (..)
    , Unique (..)
    )
where

import Data.Kind (Type)
import Database.Persist (EntityField, Unique)
import Database.Persist.TH

import Usta.DB.Schema.PullRequest (PullRequestId)
import Usta.Types.Involvement (Involvement)

-- 'discoverEntities' passes the entities already in scope (the
-- pull_requests table), so the reference becomes a foreign key.
share
    [mkPersistWith sqlSettings {mpsFieldLabelModifier = \_ field -> field} $(discoverEntities)]
    [persistLowerCase|
PullRequestInvolvement sql=pull_request_involvements
    pullRequestId  PullRequestId OnDeleteCascade
    involvement    Involvement

    UniquePullRequestInvolvement pullRequestId involvement
    deriving Eq Show
|]

type PullRequestInvolvement :: Type

type PullRequestInvolvementId :: Type
