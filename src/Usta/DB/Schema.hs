{-# LANGUAGE TemplateHaskell #-}

-- | The whole cache schema: every table under "Usta.DB.Schema", migrated
-- together. Code that reads or writes a table imports that table's module.
module Usta.DB.Schema
  ( migrateAll
  ) where

import Database.Persist.Sql (Migration)
import Database.Persist.TH (discoverEntities, migrateModels)

-- Imported for their instances only: 'discoverEntities' finds every entity
-- whose instances are in scope here.
import Usta.DB.Schema.Account ()
import Usta.DB.Schema.Issue ()
import Usta.DB.Schema.IssueInvolvement ()
import Usta.DB.Schema.IssueLabel ()
import Usta.DB.Schema.Mirror ()
import Usta.DB.Schema.Notification ()
import Usta.DB.Schema.PullRequest ()
import Usta.DB.Schema.PullRequestInvolvement ()
import Usta.DB.Schema.Repository ()
import Usta.DB.Schema.SyncCursor ()

-- | Creates missing tables and columns; leaves existing data alone.
migrateAll :: Migration
migrateAll = migrateModels $(discoverEntities)
