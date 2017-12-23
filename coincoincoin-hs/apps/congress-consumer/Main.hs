module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever, void)
import Control.Monad.Catch    (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.Logger (MonadLogger, logInfoN, logErrorN)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8  as C8
import Data.Monoid ((<>))
import qualified Data.List              as L
import qualified Data.Text              as T
import Network.Kafka (TopicAndMessage)
import qualified Network.Kafka          as K

import App (AppConfig(..))
import qualified App
import CoinCoinCoin.Class
    ( MonadDb
    , MonadDbReader(..)
    , MonadDbWriter(..)
    , MonadTime(..)
    )
import CoinCoinCoin.Congress.Events.Types (CongressEvent(..))
import CoinCoinCoin.Database.Models
    ( Entity(..)
    , KafkaClientId
    , KafkaOffset(..)
    , Partition
    , TopicName
    )
import qualified CoinCoinCoin.Database.Models as M
import CoinCoinCoin.Errors (ParseError(..))
import CoinCoinCoin.Logging as Log
import CoinCoinCoin.MessageQueue
    ( MonadMessageConsumer(..)
    , Topic(..)
    , mkTopic
    )

import qualified Congress.Events.Processor as P

main :: IO ()
main = do
    cfg <- App.mkAppConfig
    M.runMigrations $ App.appDbConn cfg
    forever $ do
        App.runAppT cfg doIt >>= \case
            Left err -> Log.runLogging' (logErrorN . T.pack $ show err)
            Right _ -> Log.runLogging' (logInfoN . T.pack $ "Success!")
        threadDelay (pollInterval cfg)

doIt :: ( MonadDb m
        , MonadLogger m
        , MonadMessageConsumer m
        , MonadReader AppConfig m
        , MonadTime m
        , MonadCatch m
        , MonadThrow m
        ) => m ()
doIt = do
    partition <- asks appKafkaPartition
    clientId <- asks appKafkaClientId
    offset <- getLatestOffset clientId topic partition
    logConsumerDetails offset
    events <- consumeMessages topic partition (M.kafkaOffsetOffset offset)
    processEvents offset events
    where
        topic = mkTopic CongressContractEventReceived
        logConsumerDetails offset = do
            logInfoN "Fetching messages..."
            logInfoN . T.pack $ "Topic: " <> show (kafkaOffsetTopic offset)
            logInfoN . T.pack $ "Partition: " <> show (kafkaOffsetPartition offset)
            logInfoN . T.pack $ "Offset: " <> show (kafkaOffsetOffset offset)

processEvents :: ( MonadDbWriter m
                 , MonadLogger m
                 , MonadCatch m
                 , MonadThrow m
                 , MonadTime m
                 ) => KafkaOffset -> [TopicAndMessage] -> m ()
processEvents kOffset msgs =
    forM_ (L.sort (map K.tamPayload msgs)) $ \msg -> do
        evt <- catch (parseMessage msg) logAndReThrowParseFailure
        P.processEvent evt
        void $ incrementKafkaOffset kOffset


parseMessage :: (MonadThrow m) => C8.ByteString -> m CongressEvent
parseMessage msg =
    case AE.eitherDecodeStrict msg :: Either String CongressEvent of
        (Left err) -> throwM . JSONParseError $ T.pack err
        (Right evt) -> pure evt

logAndReThrowParseFailure :: ( MonadLogger m
                             , MonadThrow m
                             ) => ParseError -> m a
logAndReThrowParseFailure err = do
    logErrorN . T.pack $ show err
    throwM err

getLatestOffset :: ( MonadDbReader m
                   , MonadMessageConsumer m
                   , MonadTime m
                   ) => KafkaClientId -> TopicName -> Partition -> m KafkaOffset
getLatestOffset clientId topic partition =
    getKafkaOffset clientId topic partition >>= \case
        (Just offset) -> return $ entityVal offset
        Nothing -> do
            offset <- getEarliestOffset topic partition
            now <- getCurrentTime
            return $ KafkaOffset topic partition offset clientId now now
