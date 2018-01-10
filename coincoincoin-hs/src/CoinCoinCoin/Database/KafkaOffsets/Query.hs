module CoinCoinCoin.Database.KafkaOffsets.Query
    ( getAllKafkaOffsets
    , getKafkaOffset
    , incrementKafkaOffset
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Database.Persist.Postgresql
import Network.Kafka.Protocol (Partition, TopicName)

import CoinCoinCoin.Database.Models

getAllKafkaOffsets :: SqlPersistT IO [Entity KafkaOffset]
getAllKafkaOffsets =
    selectList [] []

getKafkaOffset :: KafkaClientId -> TopicName -> Partition -> SqlPersistT IO (Maybe (Entity KafkaOffset))
getKafkaOffset clientId topic partition =
    getBy (UniqueKafkaOffsetsTopicPartition topic partition clientId)

incrementKafkaOffset :: KafkaOffset -> SqlPersistT IO (Entity KafkaOffset)
incrementKafkaOffset offset@KafkaOffset{..} = do
    now <- liftIO getCurrentTime
    let uniqueKey = UniqueKafkaOffsetsTopicPartition kafkaOffsetTopic kafkaOffsetPartition kafkaOffsetClientId

    upsertBy uniqueKey offset
        [ Update KafkaOffsetOffset 1 Add
        , Update KafkaOffsetUpdated now Assign
        ]
