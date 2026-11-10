{- | The pull requests each account has synced, with how it is involved in
them.
-}
module Usta.DB.PullRequest
    ( savePullRequests
    , pullRequests
    , fromEntities
    )
where

import Control.Applicative ((<|>))
import Data.Foldable (for_)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
    ( Entity (..)
    , InnerJoin (..)
    , desc
    , from
    , in_
    , on
    , orderBy
    , select
    , table
    , valList
    , where_
    , (:&) (..)
    , (==.)
    )

import Data.Map.Strict qualified as Map
import Database.Persist qualified as Persist

import Usta.DB (Result, requireHost)
import Usta.DB.Schema.Account (AccountId)
import Usta.DB.Schema.PullRequest (PullRequestId, Unique (..))
import Usta.DB.Schema.PullRequestInvolvement (EntityField (..))
import Usta.Types.Host (Host)
import Usta.Types.Involvement (Involvement)
import Usta.Types.PullRequest (PullRequest (..))
import Usta.Types.Ref (Ref (..))

import Usta.DB.Schema.Account qualified as Account
import Usta.DB.Schema.PullRequest qualified as Schema
import Usta.DB.Schema.PullRequestInvolvement qualified as Involvement

{- | Adds each pull request, or replaces the one already synced with the
same repository and number, along with its involvements. A list sync
doesn't carry the changed files and commits, so a missing count keeps
the one fetched before. Throws 'Usta.DB.WrongHost' if any pull request
is not on the account's server.
-}
savePullRequests :: AccountId -> UTCTime -> [PullRequest] -> Result ()
savePullRequests account now prs = do
    requireHost account (map (\pr -> pr.ref.host) prs)
    for_ prs $ \pr -> do
        let
            row = toRow account now pr
        existing <- Persist.getBy (UniquePullRequest account pr.ref.repo pr.ref.number)
        key <- case existing of
            Nothing -> Persist.insert row
            Just (Entity key old) -> do
                Persist.replace key (keepDetails old row)
                Persist.deleteWhere [PullRequestInvolvementPullRequestId Persist.==. key]
                pure key
        Persist.insertMany_ [Involvement.PullRequestInvolvement {pullRequestId = key, involvement} | involvement <- nub pr.involvement]

{- | Every synced pull request with the account it was synced for, most
recently updated first.
-}
pullRequests :: Result [(AccountId, PullRequest)]
pullRequests = do
    rows <- select $ do
        (pr :& account) <-
            from
                $ table @Schema.PullRequest
                    `InnerJoin` table @Account.Account
                        `on` (\(pr :& account) -> pr.accountId ==. account.id)
        orderBy [desc pr.updatedAt]
        pure (pr, account)
    converted <- fromEntities [((entityVal account).host, pr) | (pr, account) <- rows]
    pure (zip [(entityVal pr).accountId | (pr, _) <- rows] converted)

{- | Converts rows read elsewhere (the inbox joins them), with their
account's host, loading their involvements in one query.
-}
fromEntities :: [(Host, Entity Schema.PullRequest)] -> Result [PullRequest]
fromEntities rows = do
    found <- involvements (map (entityKey . snd) rows)
    pure [fromRow host (Map.findWithDefault [] key found) row | (host, Entity key row) <- rows]

involvements :: [PullRequestId] -> Result (Map PullRequestId [Involvement])
involvements keys = do
    rows <- select $ do
        i <- from (table @Involvement.PullRequestInvolvement)
        where_ (i.pullRequestId `in_` valList keys)
        pure i
    pure (Map.fromListWith (flip (<>)) [(row.pullRequestId, [row.involvement]) | Entity _ row <- rows])

toRow :: AccountId -> UTCTime -> PullRequest -> Schema.PullRequest
toRow account now pr =
    Schema.PullRequest
        { accountId = account
        , repo = pr.ref.repo
        , number = pr.ref.number
        , title = pr.title
        , author = pr.author
        , headBranch = pr.headBranch
        , headOwner = pr.headOwner
        , baseBranch = pr.baseBranch
        , state = pr.state
        , checks = pr.checks
        , review = pr.review
        , comments = pr.comments
        , changedFiles = pr.changedFiles
        , commits = pr.commits
        , createdAt = pr.createdAt
        , updatedAt = pr.updatedAt
        , syncedAt = now
        }

keepDetails :: Schema.PullRequest -> Schema.PullRequest -> Schema.PullRequest
keepDetails old new =
    new
        { Schema.changedFiles = new.changedFiles <|> old.changedFiles
        , Schema.commits = new.commits <|> old.commits
        }

fromRow :: Host -> [Involvement] -> Schema.PullRequest -> PullRequest
fromRow host found row =
    PullRequest
        { ref = Ref {host, repo = row.repo, number = row.number}
        , title = row.title
        , author = row.author
        , headBranch = row.headBranch
        , headOwner = row.headOwner
        , baseBranch = row.baseBranch
        , state = row.state
        , checks = row.checks
        , review = row.review
        , comments = row.comments
        , changedFiles = row.changedFiles
        , commits = row.commits
        , involvement = found
        , createdAt = row.createdAt
        , updatedAt = row.updatedAt
        }
