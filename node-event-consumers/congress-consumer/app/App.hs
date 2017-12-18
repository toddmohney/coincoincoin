module App
    ( AppT(..)
    , runAppT
    , module AppConfig
    ) where

import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.Except   (ExceptT (..), MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger   (LoggingT (..), MonadLogger)
import           Control.Monad.Reader   (MonadReader, ReaderT (..), asks, runReaderT)
import           Network.Kafka          (KafkaClientError)

import           AppConfig (AppConfig(..), mkAppConfig)
import           CoinCoinCoin.MessageQueue (MonadMessageQueue(..))
import           CoinCoinCoin.MessageQueue.Adapters.Kafka (runKafkaT)
import           CoinCoinCoin.Logging (runLogging)

newtype AppT m a = AppT { unAppT :: ReaderT AppConfig (LoggingT (ExceptT KafkaClientError m)) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadLogger
             , MonadError KafkaClientError
             , MonadReader AppConfig
             , MonadCatch
             , MonadThrow
             )

runAppT :: (MonadIO m) => AppConfig -> AppT m a -> m (Either KafkaClientError a)
runAppT cfg appT =
    runExceptT . runLogging False (appEnv cfg) $
        runReaderT (unAppT appT) cfg

instance MonadIO m => MonadMessageQueue (AppT m) where
    produceMessages jobs = do
        kState <- asks kafkaState
        result <- runKafkaT kState (produceMessages jobs)
        case result of
            Left err -> throwError err
            Right resps -> return resps

    getEarliestOffset kTopic kPartition = do
        kState <- asks kafkaState
        result <- runKafkaT kState (getEarliestOffset kTopic kPartition)
        case result of
            Left err -> throwError err
            Right offset -> return offset

    consumeMessages kTopic kPartition offset = do
        kState <- asks kafkaState
        result <- runKafkaT kState (consumeMessages kTopic kPartition offset)
        case result of
            Left err -> throwError err
            Right resp -> return resp
