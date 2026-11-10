-- | The repositories each account can see, as last synced.
module Usta.DB.Repository
    ( saveRepositories
    , keepOnly
    , repositories
    )
where

import Data.Foldable (for_)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
    ( Entity (..)
    , InnerJoin (..)
    , desc
    , from
    , on
    , orderBy
    , select
    , table
    , (:&) (..)
    , (==.)
    )

import Database.Persist qualified as Persist

import Usta.DB (Result, requireHost)
import Usta.DB.Schema.Account (AccountId)
import Usta.DB.Schema.Repository (Unique (..))
import Usta.Types.Host (Host)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Repository (Repository (..))

import Usta.DB.Schema.Account qualified as Account
import Usta.DB.Schema.Repository qualified as Schema

{- | Adds each repository, or replaces the one already synced with the
same name. Throws 'Usta.DB.WrongHost' if any is not on the account's
server.
-}
saveRepositories :: AccountId -> UTCTime -> [Repository] -> Result ()
saveRepositories account now found = do
    requireHost account (map (\repository -> repository.host) found)
    for_ found $ \repository -> do
        let
            row = toRow account now repository
        existing <- Persist.getBy (UniqueRepository account repository.name)
        case existing of
            Nothing -> () <$ Persist.insert row
            Just (Entity key _) -> Persist.replace key row

{- | Forgets the account's repositories not named: the forge listed all
it has, and these were not among them.
-}
keepOnly :: AccountId -> [RepoName] -> Result ()
keepOnly account names =
    Persist.deleteWhere [Schema.RepositoryAccountId Persist.==. account, Schema.RepositoryName Persist./<-. names]

{- | Every synced repository with the account that sees it, most recently
updated first.
-}
repositories :: Result [(AccountId, Repository)]
repositories = do
    rows <- select $ do
        (repository :& account) <-
            from
                $ table @Schema.Repository
                    `InnerJoin` table @Account.Account
                        `on` (\(repository :& account) -> repository.accountId ==. account.id)
        orderBy [desc repository.updatedAt]
        pure (repository, account)
    pure [(row.accountId, fromRow account.host row) | (Entity _ row, Entity _ account) <- rows]

toRow :: AccountId -> UTCTime -> Repository -> Schema.Repository
toRow account now repository =
    Schema.Repository
        { accountId = account
        , name = repository.name
        , description = repository.description
        , defaultBranch = repository.defaultBranch
        , private = repository.private
        , openPullRequests = repository.openPullRequests
        , openIssues = repository.openIssues
        , updatedAt = repository.updatedAt
        , syncedAt = now
        }

fromRow :: Host -> Schema.Repository -> Repository
fromRow host row =
    Repository
        { host
        , name = row.name
        , description = row.description
        , defaultBranch = row.defaultBranch
        , private = row.private
        , openPullRequests = row.openPullRequests
        , openIssues = row.openIssues
        , updatedAt = row.updatedAt
        }
