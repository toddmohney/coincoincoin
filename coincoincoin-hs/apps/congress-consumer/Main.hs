module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Network.Kafka (TopicAndMessage)

import           App (AppConfig(..))
import qualified App
import           CoinCoinCoin.Class (MonadDbReader(..), MonadTime(..))
import           CoinCoinCoin.Database.Models
    ( Entity(..)
    , KafkaClientId
    , KafkaOffset(..)
    , Partition
    , TopicName
    )
import qualified CoinCoinCoin.Database.Models as M
import           CoinCoinCoin.MessageQueue
    ( MonadMessageQueue(..)
    , Topic(..)
    , mkTopic
    )

main :: IO ()
main = do
    cfg <- App.mkAppConfig
    forever $ do
        void $ App.runAppT cfg doIt
        threadDelay (pollInterval cfg)

doIt :: ( MonadIO m
        , MonadDbReader m
        , MonadMessageQueue m
        , MonadReader AppConfig m
        , MonadTime m
        ) => m ()
doIt = do
    partition <- asks appKafkaPartition
    clientId <- asks appKafkaClientId
    offset <- getLatestOffset clientId topic partition
    events <- consumeMessages topic partition (M.kafkaOffsetOffset offset)
    mapM_ processEvent events
    where
        topic = mkTopic CongressContractEventReceived

processEvent :: (MonadIO m) => TopicAndMessage -> m ()
processEvent = undefined

getLatestOffset :: ( MonadIO m
                   , MonadDbReader m
                   , MonadMessageQueue m
                   , MonadTime m
                   ) => KafkaClientId -> TopicName -> Partition -> m KafkaOffset
getLatestOffset clientId topic partition =
    getKafkaOffset clientId topic partition >>= \case
        (Just offset) -> return $ entityVal offset
        Nothing -> do
            offset <- getEarliestOffset topic partition
            now <- getCurrentTime
            return $ KafkaOffset topic partition offset clientId now now
