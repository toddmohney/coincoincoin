module CoinCoinCoin.Database.KafkaOffsets.Query
    ( getAllKafkaOffsets
    , getKafkaOffset
    , createKafkaOffset
    , updateKafkaOffset
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

createKafkaOffset :: KafkaOffset -> SqlPersistT IO (Key KafkaOffset)
createKafkaOffset = insert

updateKafkaOffset :: KafkaOffsetId -> KafkaOffset -> SqlPersistT IO ()
updateKafkaOffset = replace

incrementKafkaOffset :: KafkaOffsetId -> SqlPersistT IO ()
incrementKafkaOffset offsetId = do
    now <- liftIO getCurrentTime
    updateWhere [ KafkaOffsetId ==. offsetId ]
        [ KafkaOffsetOffset +=. 1
        , KafkaOffsetUpdated =. now
        ]

