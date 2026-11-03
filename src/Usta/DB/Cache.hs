-- | Forgetting what sync saved, for "Clear cache". Accounts and the
-- user's mirror choices stay; the next sync fetches the rest again.
module Usta.DB.Cache
  ( clear
  ) where

import Database.Persist (Filter, deleteWhere)

import Usta.DB (Result)
import qualified Usta.DB.Schema.Issue as Issue
import qualified Usta.DB.Schema.IssueInvolvement as IssueInvolvement
import qualified Usta.DB.Schema.IssueLabel as IssueLabel
import qualified Usta.DB.Schema.Notification as Notification
import qualified Usta.DB.Schema.PullRequest as PullRequest
import qualified Usta.DB.Schema.PullRequestInvolvement as PullRequestInvolvement
import qualified Usta.DB.Schema.Repository as Repository
import qualified Usta.DB.Schema.SyncCursor as SyncCursor

-- | Every synced list, and the cursors (ETags, times) that would let sync
-- skip fetching them. Rows that point at others go first.
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
