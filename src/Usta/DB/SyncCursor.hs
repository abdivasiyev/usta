-- | Where each account's sync of each list stopped.
module Usta.DB.SyncCursor
    ( Cursor (..)
    , cursor
    , saveCursor
    )
where

import Data.Kind (Type)
import Data.Time (UTCTime)
import Database.Persist (Entity (..), getBy, upsertBy, (=.))

import Usta.DB (Result)
import Usta.DB.Schema.Account (AccountId)
import Usta.DB.Schema.SyncCursor (EntityField (..), Unique (..))
import Usta.Types.ETag (ETag)
import Usta.Types.Resource (Resource)

import Usta.DB.Schema.SyncCursor qualified as Schema

-- | What the last sync of a list left behind.
type Cursor :: Type
data Cursor = Cursor
    { etag :: Maybe ETag
    {- ^ 'Nothing' when the forge sent none; the next sync fetches the
    whole list.
    -}
    , syncedAt :: UTCTime
    }
    deriving stock (Eq, Show)

-- | 'Nothing' until the list has been synced once.
cursor :: AccountId -> Resource -> Result (Maybe Cursor)
cursor account resource = fmap (fromRow . entityVal) <$> getBy (UniqueSyncCursor account resource)

-- | Records a finished sync, replacing the cursor it started from.
saveCursor :: AccountId -> Resource -> Cursor -> Result ()
saveCursor account resource synced = do
    _ <-
        upsertBy
            (UniqueSyncCursor account resource)
            Schema.SyncCursor {accountId = account, resource, etag = synced.etag, syncedAt = synced.syncedAt}
            [SyncCursorEtag =. synced.etag, SyncCursorSyncedAt =. synced.syncedAt]
    pure ()

fromRow :: Schema.SyncCursor -> Cursor
fromRow row = Cursor {etag = row.etag, syncedAt = row.syncedAt}
