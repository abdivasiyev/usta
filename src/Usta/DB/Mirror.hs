-- | Which repositories the user chose to mirror, and how.
module Usta.DB.Mirror
  ( Wanted (..)
  , choose
  , choiceFor
  , mirrored
  , markFetched
  ) where

import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist (Entity (..), Filter, get, getBy, selectList, updateWhere, upsertBy, (=.), (==.))

import Usta.DB (Result)
import Usta.DB.Schema.Account (Account (..), AccountId)
import Usta.DB.Schema.Mirror (EntityField (..), Unique (..))
import qualified Usta.DB.Schema.Mirror as Schema
import Usta.Git (Kind)
import Usta.Types.Host (Host)
import Usta.Types.Mirror (Choice (..))
import Usta.Types.RepoName (RepoName)

-- | A repository to keep a mirror of.
type Wanted :: Type
data Wanted = Wanted
  { accountId :: AccountId
  , host :: Host
  , repo :: RepoName
  , kind :: Kind
  , url :: Text
  }
  deriving stock (Eq, Show)

-- | Records the user's answer for a repository, replacing an earlier one.
-- @url@ is where a mirror fetches from; 'Nothing' for 'Declined'.
choose :: UTCTime -> AccountId -> RepoName -> Choice -> Maybe Text -> Result ()
choose now account repo choice url = do
  _ <-
    upsertBy
      (UniqueMirror account repo)
      Schema.Mirror {accountId = account, repo, choice, url, fetchedAt = Nothing, decidedAt = now}
      [MirrorChoice =. choice, MirrorUrl =. url, MirrorDecidedAt =. now]
  pure ()

-- | 'Nothing' until the user answered for this repository.
choiceFor :: AccountId -> RepoName -> Result (Maybe Choice)
choiceFor account repo = fmap ((.choice) . entityVal) <$> getBy (UniqueMirror account repo)

-- | Every repository chosen for a mirror, with its account's host.
mirrored :: Result [Wanted]
mirrored = do
  rows <- selectList ([] :: [Filter Schema.Mirror]) []
  catMaybes <$> traverse (wanted . entityVal) rows
  where
    wanted row = case (row.choice, row.url) of
      (Mirrored kind _, Just url) -> do
        account <- get row.accountId
        pure $ fmap (\a -> Wanted {accountId = row.accountId, host = a.host, repo = row.repo, kind = kind, url = url}) account
      _ -> pure Nothing

markFetched :: UTCTime -> AccountId -> RepoName -> Result ()
markFetched now account repo =
  updateWhere [MirrorAccountId ==. account, MirrorRepo ==. repo] [MirrorFetchedAt =. Just now]
