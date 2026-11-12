{- | The issues each account has synced, with their labels and how the
account is involved in them.
-}
module Usta.DB.Issue
    ( saveIssues
    , issues
    )
where

import Data.Foldable (for_)
import Data.List (nub, nubBy)
import Data.Map.Strict (Map)
import Data.Time (UTCTime)
import Database.Esqueleto.Experimental
    ( Entity (..)
    , InnerJoin (..)
    , asc
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
import Usta.DB.Schema.Issue (IssueId, Unique (..))
import Usta.DB.Schema.IssueInvolvement (EntityField (..))
import Usta.DB.Schema.IssueLabel (EntityField (..))
import Usta.Types.Host (Host)
import Usta.Types.Involvement (Involvement)
import Usta.Types.Issue (Issue (..))
import Usta.Types.Label (Label (..))
import Usta.Types.Ref (Ref (..))

import Usta.DB.Schema.Account qualified as Account
import Usta.DB.Schema.Issue qualified as Schema
import Usta.DB.Schema.IssueInvolvement qualified as Involvement
import Usta.DB.Schema.IssueLabel qualified as IssueLabel

{- | Adds each issue, or replaces the one already synced with the same
repository and number, along with its labels and involvements. Throws
'Usta.DB.WrongHost' if any issue is not on the account's server.
-}
saveIssues :: AccountId -> UTCTime -> [Issue] -> Result ()
saveIssues account now found = do
    requireHost account (map (\issue -> issue.ref.host) found)
    for_ found $ \issue -> do
        let
            row = toRow account now issue
        existing <- Persist.getBy (UniqueIssue account issue.ref.repo issue.ref.number)
        key <- case existing of
            Nothing -> Persist.insert row
            Just (Entity key _) -> do
                Persist.replace key row
                Persist.deleteWhere [IssueLabelIssueId Persist.==. key]
                Persist.deleteWhere [IssueInvolvementIssueId Persist.==. key]
                pure key
        Persist.insertMany_
            [ IssueLabel.IssueLabel {issueId = key, name = label.name, color = label.color}
            | label <- nubBy (\x y -> x.name == y.name) issue.labels
            ]
        Persist.insertMany_ [Involvement.IssueInvolvement {issueId = key, involvement} | involvement <- nub issue.involvement]

{- | Every synced issue with the account it was synced for, most recently
updated first.
-}
issues :: Result [(AccountId, Issue)]
issues = do
    rows <- select $ do
        (issue :& account) <-
            from
                $ table @Schema.Issue
                    `InnerJoin` table @Account.Account
                        `on` (\(issue :& account) -> issue.accountId ==. account.id)
        orderBy [desc issue.updatedAt]
        pure (issue, account)
    let
        keys = map (entityKey . fst) rows
    foundLabels <- labelsOf keys
    foundInvolvements <- involvementsOf keys
    pure
        [ (row.accountId, fromRow account.host (Map.findWithDefault [] key foundLabels) (Map.findWithDefault [] key foundInvolvements) row)
        | (Entity key row, Entity _ account) <- rows
        ]

-- | In the order the forge listed them.
labelsOf :: [IssueId] -> Result (Map IssueId [Label])
labelsOf keys = do
    rows <- select $ do
        label <- from (table @IssueLabel.IssueLabel)
        where_ (label.issueId `in_` valList keys)
        orderBy [asc label.id]
        pure label
    pure (Map.fromListWith (flip (<>)) [(row.issueId, [Label {name = row.name, color = row.color}]) | Entity _ row <- rows])

involvementsOf :: [IssueId] -> Result (Map IssueId [Involvement])
involvementsOf keys = do
    rows <- select $ do
        i <- from (table @Involvement.IssueInvolvement)
        where_ (i.issueId `in_` valList keys)
        pure i
    pure (Map.fromListWith (flip (<>)) [(row.issueId, [row.involvement]) | Entity _ row <- rows])

toRow :: AccountId -> UTCTime -> Issue -> Schema.Issue
toRow account now issue =
    Schema.Issue
        { accountId = account
        , repo = issue.ref.repo
        , number = issue.ref.number
        , title = issue.title
        , author = issue.author
        , comments = issue.comments
        , createdAt = issue.createdAt
        , updatedAt = issue.updatedAt
        , closedAt = issue.closedAt
        , syncedAt = now
        }

fromRow :: Host -> [Label] -> [Involvement] -> Schema.Issue -> Issue
fromRow host foundLabels found row =
    Issue
        { ref = Ref {host, repo = row.repo, number = row.number}
        , title = row.title
        , author = row.author
        , labels = foundLabels
        , comments = row.comments
        , involvement = found
        , createdAt = row.createdAt
        , updatedAt = row.updatedAt
        , closedAt = row.closedAt
        }
