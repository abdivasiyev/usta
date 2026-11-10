{- | Forgetting what sync saved, for "Clear cache". Accounts and the
user's mirror choices stay; the next sync fetches the rest again.
-}
module Usta.DB.Cache
    ( clear
    )
where

import Database.Persist (Filter, deleteWhere)

import Usta.DB (Result)

import Usta.DB.Schema.Issue qualified as Issue
import Usta.DB.Schema.IssueInvolvement qualified as IssueInvolvement
import Usta.DB.Schema.IssueLabel qualified as IssueLabel
import Usta.DB.Schema.Notification qualified as Notification
import Usta.DB.Schema.PullRequest qualified as PullRequest
import Usta.DB.Schema.PullRequestInvolvement qualified as PullRequestInvolvement
import Usta.DB.Schema.Repository qualified as Repository
import Usta.DB.Schema.SyncCursor qualified as SyncCursor

{- | Every synced list, and the cursors (ETags, times) that would let sync
skip fetching them. Rows that point at others go first.
-}
clear :: Result ()
clear = do
    deleteWhere ([] :: [Filter IssueLabel.IssueLabel])
    deleteWhere ([] :: [Filter IssueInvolvement.IssueInvolvement])
    deleteWhere ([] :: [Filter Issue.Issue])
    deleteWhere ([] :: [Filter PullRequestInvolvement.PullRequestInvolvement])
    deleteWhere ([] :: [Filter PullRequest.PullRequest])
    deleteWhere ([] :: [Filter Notification.Notification])
    deleteWhere ([] :: [Filter Repository.Repository])
    deleteWhere ([] :: [Filter SyncCursor.SyncCursor])
