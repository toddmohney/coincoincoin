module Helpers.DatabaseHelpers
    ( prepDb
    , setupTestDatabase
    , truncateDatabase
    ) where

import           Control.Monad (void)
import           Database.Persist.Postgresql (ConnectionPool)
import qualified Database.Persist.Postgresql as DB

import CoinCoinCoin.Database.Config
import CoinCoinCoin.Database.Models (runMigrations')

prepDb :: IO ConnectionPool
prepDb = do
    dbPool <- mkPool' "postgres://coincoincoin:coincoincoin@localhost:5432/coincoincoin_test"
    setupTestDatabase dbPool
    return dbPool

setupTestDatabase :: DB.ConnectionPool -> IO ()
setupTestDatabase pool = do
    void $ runMigrations' pool
    truncateDatabase pool

truncateDatabase :: DB.ConnectionPool -> IO ()
truncateDatabase = DB.runSqlPool truncateQuery
    where
        truncateQuery = do
            DB.rawExecute "TRUNCATE TABLE congress_memberships RESTART IDENTITY CASCADE;" []
            DB.rawExecute "TRUNCATE TABLE contracts RESTART IDENTITY CASCADE;" []
            DB.rawExecute "TRUNCATE TABLE kafka_offsets RESTART IDENTITY CASCADE;" []
