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
    , Contract(..)
    , ContractId
    , NetworkBuilder
    )
import qualified CoinCoinCoin.Database.Contracts.Query as Q

import           AppConfig (AppConfig(..))

class (Monad m) => MonadDbWriter m where
    type DbWriterType m :: * -> *

    runDbWriter :: (DbWriterType m) a -> m a

    upsertContract :: Contract -> m ContractId

    upsertContract' :: (Contract, [NetworkBuilder]) -> m ContractId

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

instance (MonadIO m) => MonadDbWriter (AppT m) where
    type DbWriterType (AppT m) = SqlPersistT IO

    runDbWriter :: SqlPersistT IO a -> AppT m a
    runDbWriter query = do
        conn <- asks appDbConn
        liftIO $ Sql.runSqlPool query conn

    upsertContract :: Contract -> AppT m ContractId
    upsertContract = runDbWriter . Q.upsertContract

    upsertContract' :: (Contract, [NetworkBuilder]) -> AppT m ContractId
    upsertContract' (c, nbs) = do
        cId <- runDbWriter $ Q.upsertContract c
        mapM_ ((runDbWriter . Q.upsertNetwork) . (\nb -> nb cId)) nbs
        return cId
