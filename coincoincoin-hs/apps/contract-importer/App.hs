{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module App
    ( AppT(..)
    , runAppT
    ) where
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT(..), MonadLogger)
import           Control.Monad.Reader
    ( MonadReader
    , ReaderT(..)
    , runReaderT
    )
import           Data.ByteString (ByteString)
import           Prelude hiding (readFile)

import           CoinCoinCoin.Class
    ( MonadFileReader(..)
    , MonadTime(..)
    )
import           CoinCoinCoin.Logging (runLogging')

import           AppConfig (AppConfig)

newtype AppT m a = AppT { unAppT :: ReaderT AppConfig (LoggingT m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadLogger
             , MonadReader AppConfig
             , MonadCatch
             , MonadThrow
             )

runAppT :: (MonadIO m) => AppConfig -> AppT m a -> m a
runAppT cfg appT =
    runLogging' $
        runReaderT (unAppT appT) cfg

instance MonadIO m => MonadTime (AppT m) where
    getCurrentTime = liftIO getCurrentTime

instance MonadIO m => MonadFileReader (AppT m) where
    doesDirectoryExist :: FilePath -> AppT m Bool
    doesDirectoryExist = liftIO . doesDirectoryExist

    listDirectory :: FilePath -> AppT m [FilePath]
    listDirectory = liftIO . listDirectory

    readFile :: FilePath -> AppT m ByteString
    readFile = liftIO . readFile
