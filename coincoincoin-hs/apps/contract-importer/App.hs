{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module App
    ( AppT(..)
    , MonadDb
    , MonadDbReader(..)
    , MonadDbWriter(..)
    , runAppT
    ) where
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT(..), MonadLogger)
import           Control.Monad.Reader
    ( MonadReader
    , ReaderT(..)
    , runReaderT
    , asks
    )
import           Data.ByteString (ByteString)
import qualified Database.Persist.Postgresql as Sql
import           Prelude hiding (readFile)

import           CoinCoinCoin.Class
    ( MonadFileReader(..)
    , MonadTime(..)
    )
import           CoinCoinCoin.Logging (runLogging')
import           CoinCoinCoin.Database.Models
    ( SqlPersistT
    )

import           AppConfig (AppConfig(..))

type MonadDb m = (MonadDbReader m, MonadDbWriter m)

class (Monad m) => MonadDbReader m where
    type DbReaderType m :: * -> *

    runDbReader :: (DbReaderType m) a -> m a

class (Monad m) => MonadDbWriter m where
    type DbWriterType m :: * -> *

    runDbWriter :: (DbWriterType m) a -> m a

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

instance (MonadIO m) => MonadDbReader (AppT m) where
    type DbReaderType (AppT m) = SqlPersistT IO

    runDbReader :: SqlPersistT IO a -> AppT m a
    runDbReader query = do
        conn <- asks appDbConn
        liftIO $ Sql.runSqlPool query conn

instance (MonadIO m) => MonadDbWriter (AppT m) where
    type DbWriterType (AppT m) = SqlPersistT IO

    runDbWriter :: SqlPersistT IO a -> AppT m a
    runDbWriter query = do
        conn <- asks appDbConn
        liftIO $ Sql.runSqlPool query conn
