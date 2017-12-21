{-# LANGUAGE ConstraintKinds #-}

module CoinCoinCoin.Class
    ( MonadDb
    , MonadDbReader(..)
    , MonadDbWriter(..)
    , MonadTime(..)
    , UTCTime
    ) where

import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import CoinCoinCoin.Database.Models
    ( Entity
    , KafkaClientId
    , KafkaOffset
    , KafkaOffsetId
    , Partition
    , SqlPersistT
    , TopicName
    )

class (Monad m) => MonadTime m where
    getCurrentTime :: m UTCTime

instance MonadTime IO where
    getCurrentTime = Time.getCurrentTime

type MonadDb m = (MonadDbReader m, MonadDbWriter m)

class (Monad m) => MonadDbReader m where
    runDbReader :: SqlPersistT IO a -> m a

    getAllKafkaOffsets :: m [Entity KafkaOffset]

    getKafkaOffset :: KafkaClientId -> TopicName -> Partition -> m (Maybe (Entity KafkaOffset))

class (Monad m) => MonadDbWriter m where
    runDbWriter :: SqlPersistT IO a -> m a

    createKafkaOffset :: KafkaOffset -> m KafkaOffsetId

    updateKafkaOffset :: KafkaOffsetId -> KafkaOffset -> m ()

    incrementKafkaOffset :: KafkaOffsetId -> m ()
