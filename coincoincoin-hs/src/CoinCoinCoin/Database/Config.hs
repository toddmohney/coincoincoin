module CoinCoinCoin.Database.Config
  ( ConnectionPool
  , dbConnectionString
  , dbConnectionString'
  , mkPool
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql
    ( ConnectionPool
    , ConnectionString
    , createPostgresqlPool
    )
import System.Environment (getEnv, lookupEnv)

import CoinCoinCoin.Environment
import CoinCoinCoin.Logging (runLogging)

mkPool :: Environment -> IO ConnectionPool
mkPool Test = do
    connStr <- dbConnectionString
    runNoLoggingT $ createPostgresqlPool connStr 1
mkPool env = do
    connStr  <- dbConnectionString
    poolSize <- read <$> getEnv "DB_POOL_SIZE"
    runLogging False env $ createPostgresqlPool connStr poolSize

dbConnectionString :: IO ConnectionString
dbConnectionString = pack <$> getEnv "DATABASE_URL"

dbConnectionString' :: IO (Maybe ConnectionString)
dbConnectionString' = fmap pack <$> lookupEnv "DATABASE_URL"
