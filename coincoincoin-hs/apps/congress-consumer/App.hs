{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module App
    ( AppT(..)
    , MonadDb
    , MonadDbReader(..)
    , MonadDbWriter(..)
    , runAppT
    , module AppConfig
    ) where
import           Control.Monad (void)
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.Except
    ( ExceptT(..)
    , MonadError
    , runExceptT
    , throwError
    )
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT(..), MonadLogger)
import           Control.Monad.Reader
    ( MonadReader
    , ReaderT(..)
    , asks
    , runReaderT
    )
import qualified Database.Persist.Postgresql as Sql
import           Network.Kafka (KafkaClientError, TopicAndMessage)
import           Network.Kafka.Protocol (Offset, ProduceResponse)

import           AppConfig (AppConfig(..), mkAppConfig)
import           CoinCoinCoin.Class
    ( MonadTime(..)
    )
import qualified CoinCoinCoin.Database.CongressMemberships.Query as CMQ
import qualified CoinCoinCoin.Database.KafkaOffsets.Query as KQ
import           CoinCoinCoin.Database.Models
    ( Address(..)
    , Entity(..)
    , CongressMembership(..)
    , CongressMembershipId
    , KafkaClientId
    , KafkaOffset(..)
    , KafkaOffsetId
    , Partition
    , SqlPersistT
    , TopicName
    )
import           CoinCoinCoin.Logging (runLogging)
import           CoinCoinCoin.MessageQueue
    ( Enqueueable
    , Job
    , MonadMessageConsumer(..)
    , MonadMessageProducer(..)
    )
import           CoinCoinCoin.MessageQueue.Adapters.Kafka (runKafkaT)

type MonadDb m = (MonadDbReader m, MonadDbWriter m)

class (Monad m) => MonadDbReader m where
    type DbReaderType m :: * -> *

    runDbReader :: (DbReaderType m) a -> m a

    getCongressMembership :: Address -> m (Maybe (Entity CongressMembership))

    getAllKafkaOffsets :: m [Entity KafkaOffset]

    getKafkaOffset :: KafkaClientId -> TopicName -> Partition -> m (Maybe (Entity KafkaOffset))

class (Monad m) => MonadDbWriter m where
    type DbWriterType m :: * -> *

    runDbWriter :: (DbWriterType m) a -> m a

    upsertCongressMembership :: CongressMembership -> m CongressMembershipId

    createKafkaOffset :: KafkaOffset -> m KafkaOffsetId

    updateKafkaOffset :: KafkaOffsetId -> KafkaOffset -> m ()

    incrementKafkaOffset :: KafkaOffset -> m ()

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

instance MonadIO m => MonadTime (AppT m) where
    getCurrentTime = liftIO getCurrentTime

instance MonadIO m => MonadMessageProducer (AppT m) where
    produceMessages :: (Enqueueable a) => [Job a] -> AppT m [ProduceResponse]
    produceMessages jobs = do
        kState <- asks kafkaState
        result <- runKafkaT kState (produceMessages jobs)
        case result of
            Left err    -> throwError err
            Right resps -> return resps

instance (MonadIO m) => MonadMessageConsumer (AppT m) where
    getEarliestOffset :: TopicName -> Partition -> AppT m Offset
    getEarliestOffset kTopic kPartition = do
        kState <- asks kafkaState
        result <- runKafkaT kState (getEarliestOffset kTopic kPartition)
        case result of
            Left err     -> throwError err
            Right offset -> return offset

    consumeMessages :: TopicName -> Partition -> Offset -> AppT m [TopicAndMessage]
    consumeMessages kTopic kPartition offset = do
        kState <- asks kafkaState
        result <- runKafkaT kState (consumeMessages kTopic kPartition offset)
        case result of
            Left err   -> throwError err
            Right resp -> return resp

instance (MonadIO m) => MonadDbReader (AppT m) where
    type DbReaderType (AppT m) = SqlPersistT IO

    runDbReader :: SqlPersistT IO a -> AppT m a
    runDbReader query = do
        conn <- asks appDbConn
        liftIO $ Sql.runSqlPool query conn

    getCongressMembership :: Address -> AppT m (Maybe (Entity CongressMembership))
    getCongressMembership =
        runDbReader . CMQ.getCongressMembership

    getAllKafkaOffsets :: AppT m [Entity KafkaOffset]
    getAllKafkaOffsets =
        runDbReader KQ.getAllKafkaOffsets

    getKafkaOffset :: KafkaClientId -> TopicName -> Partition -> AppT m (Maybe (Entity KafkaOffset))
    getKafkaOffset kId tName part =
        runDbReader $ KQ.getKafkaOffset kId tName part

instance (MonadIO m) => MonadDbWriter (AppT m) where
    type DbWriterType (AppT m) = SqlPersistT IO

    runDbWriter :: SqlPersistT IO a -> AppT m a
    runDbWriter query = do
        conn <- asks appDbConn
        liftIO $ Sql.runSqlPool query conn

    upsertCongressMembership :: CongressMembership -> AppT m CongressMembershipId
    upsertCongressMembership mem = do
        mResult <- getCongressMembership $ congressMembershipMember mem
        case mResult of
            Nothing ->
                runDbWriter $ CMQ.createCongressMembership mem
            (Just (Entity memberId _)) -> do
                runDbWriter $ CMQ.updateCongressMembership memberId mem
                return memberId

    createKafkaOffset :: KafkaOffset -> AppT m KafkaOffsetId
    createKafkaOffset =
        runDbWriter . KQ.createKafkaOffset

    updateKafkaOffset :: KafkaOffsetId -> KafkaOffset -> AppT m ()
    updateKafkaOffset kId offset =
        runDbWriter $ KQ.updateKafkaOffset kId offset

    incrementKafkaOffset :: KafkaOffset -> AppT m ()
    incrementKafkaOffset kOffset = do
        mOffset <- getKafkaOffset (kafkaOffsetClientId kOffset) (kafkaOffsetTopic kOffset) (kafkaOffsetPartition kOffset)
        case mOffset of
            Nothing ->
                let nextOffset = kafkaOffsetOffset kOffset + 1
                in void $ createKafkaOffset (kOffset { kafkaOffsetOffset = nextOffset })
            (Just (Entity offsetId _)) ->
                runDbWriter $ KQ.incrementKafkaOffset offsetId
