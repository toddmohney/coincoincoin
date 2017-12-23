module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever, void)
import Control.Monad.Catch    (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.Logger (MonadLogger, logErrorN)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8  as C8
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
import CoinCoinCoin.MessageQueue
    ( MonadMessageQueue(..)
    , Topic(..)
    , mkTopic
    )

import qualified Congress.Events.Processor as P

main :: IO ()
main = do
    cfg <- App.mkAppConfig
    M.runMigrations $ App.appDbConn cfg
    forever $ do
        void $ App.runAppT cfg doIt
        threadDelay (pollInterval cfg)

doIt :: ( MonadDb m
        , MonadLogger m
        , MonadMessageQueue m
        , MonadReader AppConfig m
        , MonadTime m
        , MonadCatch m
        , MonadThrow m
        ) => m ()
doIt = do
    partition <- asks appKafkaPartition
    clientId <- asks appKafkaClientId
    offset <- getLatestOffset clientId topic partition
    events <- consumeMessages topic partition (M.kafkaOffsetOffset offset)
    processEvents offset events
    where
        topic = mkTopic CongressContractEventReceived

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
