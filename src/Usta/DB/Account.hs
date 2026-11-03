-- | The accounts usta is signed in to, as the cache keeps them.
module Usta.DB.Account
  ( listAccounts
  , saveAccount
  , deleteAccount
  ) where

import Data.Time (UTCTime)
import Database.Persist (Entity (..), SelectOpt (Asc), delete, selectList, upsertBy, (=.))

import Usta.DB (Result)
import Usta.DB.Schema.Account (AccountId, EntityField (..), Unique (..))
import qualified Usta.DB.Schema.Account as Schema
import Usta.Types.Account (Account (..))

-- | Every account, in the order they were added.
listAccounts :: Result [(AccountId, Account)]
listAccounts = map fromEntity <$> selectList [] [Asc AccountCreatedAt]
  where
    fromEntity (Entity key row) = (key, fromRow row)

-- | Adds the account, or updates it when the same login on the same host
-- signs in again; its id and the time it was first added stay.
saveAccount :: UTCTime -> Account -> Result AccountId
saveAccount now account =
  entityKey
    <$> upsertBy
      (UniqueAccount account.host account.login)
      (toRow now account)
      [AccountForge =. account.forge, AccountAuthMethod =. account.authMethod]

-- | Removes the account and, through the foreign keys, everything synced
-- for it.
deleteAccount :: AccountId -> Result ()
deleteAccount = delete

toRow :: UTCTime -> Account -> Schema.Account
toRow now account =
  Schema.Account
    { forge = account.forge
    , host = account.host
    , login = account.login
    , authMethod = account.authMethod
    , createdAt = now
    }

fromRow :: Schema.Account -> Account
fromRow row =
  Account
    { forge = row.forge
    , host = row.host
    , login = row.login
    , authMethod = row.authMethod
    }
