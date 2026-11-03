-- | What sync asks of a forge, the same for every kind: one 'Forge' per
-- signed-in account. Adapters (GitHub, Forgejo, the sample) build it.
module Usta.Forge
  ( Forge (..)
  , Fetched (..)
  , CloneUrls (..)
  , Failure (..)
  ) where

import Control.Exception (Exception)
import Data.Kind (Type)
import Data.Text (Text)

import Usta.Types.Commit (Commit)
import Usta.Types.Diff (FileDiff)
import Usta.Types.ETag (ETag)
import Usta.Types.Issue (Issue)
import Usta.Types.Notification (Notification)
import Usta.Types.Number (Number)
import Usta.Types.PullRequest (PullRequest)
import Usta.Types.RepoName (RepoName)
import Usta.Types.Repository (Repository)
import Usta.Types.ThreadId (ThreadId)

-- | Each list takes the ETag of the copy sync already has, and answers
-- 'NotModified' when it is still current. Calls throw 'Failure'.
type Forge :: Type
data Forge = Forge
  { notifications :: Maybe ETag -> IO (Fetched [Notification])
  , pullRequests :: Maybe ETag -> IO (Fetched [PullRequest])
    -- ^ The ones the account is involved in.
  , issues :: Maybe ETag -> IO (Fetched [Issue])
  , repositories :: Maybe ETag -> IO (Fetched [Repository])
  , markRead :: ThreadId -> IO ()
  , cloneUrls :: RepoName -> IO CloneUrls
    -- ^ Where a mirror of the repository fetches from, as the forge says.
  , pullDiff :: RepoName -> Number -> IO [FileDiff]
    -- ^ What a pull request changes, for when the repository has no mirror.
  , pullCommits :: RepoName -> Number -> IO [Commit]
    -- ^ Its commits, oldest first.
  , recentCommits :: RepoName -> Text -> IO [Commit]
    -- ^ The last commits on a branch, newest first.
  , readme :: RepoName -> Text -> IO (Maybe (Text, Text))
    -- ^ The README at the top of a branch, with its file name.
  }

-- | A repository's clone URLs. The forge gives them because only it knows
-- its address (it may sit under a path) and its ssh port.
type CloneUrls :: Type
data CloneUrls = CloneUrls
  { https :: Text
    -- ^ The forge's own URL; plain http on a forge without TLS.
  , ssh :: Maybe Text
    -- ^ 'Nothing' when the forge has ssh turned off.
  }
  deriving stock (Eq, Show)

type Fetched :: Type -> Type
data Fetched a
  = NotModified
  | Fetched
      { etag :: Maybe ETag
        -- ^ 'Nothing' when the forge sends none; the next call fetches
        -- the whole list again.
      , items :: a
      }
  deriving stock (Eq, Show)

-- | Why a call failed, as sync tells the user.
type Failure :: Type
data Failure
  = Offline
    -- ^ The forge could not be reached.
  | SignedOut
    -- ^ The forge refused the token: sign in again.
  | Refused Text
    -- ^ Anything else the forge answered with, described for the user.
  deriving stock (Eq, Show)

instance Exception Failure
