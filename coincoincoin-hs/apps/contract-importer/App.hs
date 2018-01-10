{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module App
    ( AppT(..)
    , MonadDbWriter(..)
    , runAppT
    ) where
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT(..), MonadLogger)
import           Control.Monad.Reader
    ( MonadReader
    , ReaderT(..)
    , asks
    , runReaderT
    )
import qualified Database.Persist.Postgresql as Sql

import           CoinCoinCoin.Class (MonadFileReader(..), MonadTime(..))
import qualified CoinCoinCoin.Database.Contracts.Query as Q
import           CoinCoinCoin.Database.Models
    ( Contract(..)
    , Entity
    , SqlPersistT
    )
import           CoinCoinCoin.Logging (runLogging')

import AppConfig (AppConfig(..))

class (Monad m) => MonadDbWriter m where
    type DbWriterType m :: * -> *

    runDbWriter :: (DbWriterType m) a -> m a

    upsertContract :: Contract -> m (Entity Contract)

newtype AppT m a = AppT { unAppT :: ReaderT AppConfig (LoggingT m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadLogger
             , MonadFileReader
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

instance (MonadIO m) => MonadDbWriter (AppT m) where
    type DbWriterType (AppT m) = SqlPersistT IO

    runDbWriter :: SqlPersistT IO a -> AppT m a
    runDbWriter query = do
        conn <- asks appDbConn
        liftIO $ Sql.runSqlPool query conn

    upsertContract :: Contract -> AppT m (Entity Contract)
    upsertContract = runDbWriter . Q.upsertContract
