{-# LANGUAGE ConstraintKinds #-}

module CoinCoinCoin.MessageQueue.Class
    ( MonadMessageQueue
    , MonadMessageConsumer(..)
    , MonadMessageProducer(..)
    ) where

import Network.Kafka (TopicAndMessage)
import Network.Kafka.Protocol (Offset, Partition, ProduceResponse, TopicName)

import CoinCoinCoin.MessageQueue.Job (Enqueueable(..), Job(..))

type MonadMessageQueue m = (MonadMessageProducer m, MonadMessageConsumer m)

class (Monad m) => MonadMessageProducer m where
    -- | Produces a batch of messages
    produceMessages :: (Enqueueable a) => [Job a] -> m [ProduceResponse]
    -- | Convienience method for producing a single message
    produceMessage :: (Enqueueable a) => Job a -> m [ProduceResponse]
    produceMessage job = produceMessages [job]

class (Monad m) => MonadMessageConsumer m where
    -- | Returns the earliest relevant offset
    getEarliestOffset :: TopicName -> Partition -> m Offset

    -- | Consumes messages for the given topic starting from the offset
    consumeMessages :: TopicName -> Partition -> Offset -> m [TopicAndMessage]
