{-# LANGUAGE InstanceSigs #-}

module CoinCoinCoin.Class
    ( MonadFileReader(..)
    , MonadTime(..)
    , UTCTime
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time
import qualified System.Directory as D

class (Monad m) => MonadTime m where
    getCurrentTime :: m UTCTime

instance MonadTime IO where
    getCurrentTime = Time.getCurrentTime

class (Monad m) => MonadFileReader m where
    doesDirectoryExist :: FilePath -> m Bool

    listDirectory :: FilePath -> m [FilePath]

    readFile :: FilePath -> m ByteString

instance MonadFileReader IO where
    doesDirectoryExist :: FilePath -> IO Bool
    doesDirectoryExist = D.doesDirectoryExist

    listDirectory :: FilePath -> IO [FilePath]
    listDirectory = D.listDirectory

    readFile :: FilePath -> IO ByteString
    readFile = BS.readFile
