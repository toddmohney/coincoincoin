module Helpers.DatabaseHelpers
    ( setupTestDatabase
    , truncateDatabase
    ) where

import qualified Database.Persist.Postgresql  as DB
import           CoinCoinCoin.Database.Models (runMigrations)

setupTestDatabase :: DB.ConnectionPool -> IO ()
setupTestDatabase pool = do
    runMigrations pool
    truncateDatabase pool

truncateDatabase :: DB.ConnectionPool -> IO ()
truncateDatabase = DB.runSqlPool truncateQuery
    where
        truncateQuery = do
            DB.rawExecute "TRUNCATE TABLE congress_memberships RESTART IDENTITY CASCADE;" []
            DB.rawExecute "TRUNCATE TABLE contracts RESTART IDENTITY CASCADE;" []
            DB.rawExecute "TRUNCATE TABLE kafka_offsets RESTART IDENTITY CASCADE;" []
