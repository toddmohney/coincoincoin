{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module CoinCoinCoin.Class
    ( MonadFileReader(..)
    , MonadTime(..)
    , UTCTime
    ) where

import qualified Control.Concurrent.Async as A
import qualified Control.Monad as M
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader
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

    -- | default implementation provided to reduce boilerplate
    default doesDirectoryExist :: (MonadTrans t, MonadFileReader m1, m ~ t m1) => FilePath -> m Bool
    doesDirectoryExist = liftIO . doesDirectoryExist

    listDirectory :: FilePath -> m [FilePath]

    -- | default implementation provided to reduce boilerplate
    default listDirectory :: (MonadTrans t, MonadFileReader m1, m ~ t m1) => FilePath -> m [FilePath]
    listDirectory = liftIO . listDirectory

    readFile :: FilePath -> m ByteString

    -- | default implementation provided to reduce boilerplate
    default readFile :: (MonadTrans t, MonadFileReader m1, m ~ t m1) => FilePath -> m ByteString
    readFile = liftIO . readFile

    readFilesRecursively :: FilePath -> m [ByteString]
    readFilesRecursively path = do
        isDir <- doesDirectoryExist path
        if isDir
        then do
            files <- fmap (path FP.</>) <$> listDirectory path
            M.join <$> liftIO (A.mapConcurrently readFilesRecursively files)
        else
            (:[]) <$> readFile path

instance (MonadIO m, MonadFileReader m) => MonadFileReader (ReaderT s m)
instance (MonadIO m, MonadFileReader m) => MonadFileReader (LoggingT m)

instance MonadFileReader IO where
    doesDirectoryExist :: FilePath -> IO Bool
    doesDirectoryExist = D.doesDirectoryExist

    listDirectory :: FilePath -> IO [FilePath]
    listDirectory = D.listDirectory

    readFile :: FilePath -> IO ByteString
    readFile = BS.readFile
