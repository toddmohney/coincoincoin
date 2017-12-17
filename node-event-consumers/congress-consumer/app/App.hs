module App
    ( AppT(..)
    , runAppT
    , module AppConfig
    ) where

import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.Except   (ExceptT (..), MonadError, runExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger   (LoggingT (..), MonadLogger)
import           Control.Monad.Reader   (MonadReader, ReaderT (..), runReaderT)
import           Network.Kafka          (KafkaClientError)

import           AppConfig (AppConfig(..), mkAppConfig)
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
