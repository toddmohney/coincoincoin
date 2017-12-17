module CoinCoinCoin.Database.Config
  ( makePool
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.ByteString.Char8 (pack)
import Database.Persist.Postgresql
    (ConnectionPool, ConnectionString, createPostgresqlPool)
import System.Environment (getEnv)

import CoinCoinCoin.Environment
import CoinCoinCoin.Logging (runLogging)

data DbConnectionString =
  DbConnectionString { dbname   :: String
                     , user     :: String
                     , password :: String
                     , host     :: String
                     , port     :: Int
                     } deriving (Show)

makePool :: Environment -> IO ConnectionPool
makePool Test = do
    connStr <- dbConnectionString
    runNoLoggingT $ createPostgresqlPool connStr 1
makePool env = do
    connStr  <- dbConnectionString
    poolSize <- read <$> getEnv "DB_POOL_SIZE"
    runLogging False env $ createPostgresqlPool connStr poolSize

dbConnectionString :: IO ConnectionString
dbConnectionString =
    (pack . toStr) <$> buildDBConnectionFromEnv

toStr :: DbConnectionString -> String
toStr cStr =
    unwords
        [ "dbname=" ++ dbname cStr
        , "user=" ++ user cStr
        , "password=" ++ password cStr
        , "host=" ++ host cStr
        , "port=" ++ show (port cStr)
        ]

buildDBConnectionFromEnv :: IO DbConnectionString
buildDBConnectionFromEnv =
    DbConnectionString
        <$> getEnv "DB_NAME"
        <*> getEnv "DB_USER"
        <*> getEnv "DB_PASS"
        <*> getEnv "DB_HOST"
        <*> (read <$> getEnv "DB_PORT")

