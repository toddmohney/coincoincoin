{-# LANGUAGE InstanceSigs #-}

module CoinCoinCoin.Class
    ( MonadFileReader(..)
    , MonadTime(..)
    , UTCTime
    ) where

import qualified Control.Concurrent.Async as A
import qualified Control.Monad as M
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time
import           Prelude hiding (readFile)
import qualified System.Directory as D
import qualified System.FilePath as FP

class (Monad m) => MonadTime m where
    getCurrentTime :: m UTCTime

instance MonadTime IO where
    getCurrentTime = Time.getCurrentTime

class (MonadIO m) => MonadFileReader m where
    doesDirectoryExist :: FilePath -> m Bool

    listDirectory :: FilePath -> m [FilePath]

    readFile :: FilePath -> m ByteString

    readFilesRecursively :: FilePath -> m [ByteString]
    readFilesRecursively path = do
        isDir <- doesDirectoryExist path
        if isDir
        then do
            files <- fmap (path FP.</>) <$> listDirectory path
            M.join <$> liftIO (A.mapConcurrently readFilesRecursively files)
        else
            (:[]) <$> readFile path

instance MonadFileReader IO where
    doesDirectoryExist :: FilePath -> IO Bool
    doesDirectoryExist = D.doesDirectoryExist

    listDirectory :: FilePath -> IO [FilePath]
    listDirectory = D.listDirectory

    readFile :: FilePath -> IO ByteString
    readFile = BS.readFile
