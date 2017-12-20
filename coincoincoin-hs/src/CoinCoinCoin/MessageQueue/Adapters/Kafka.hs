module CoinCoinCoin.MessageQueue.Adapters.Kafka
    ( KafkaT(..)
    , runKafkaT
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Kafka (KafkaClientError, KafkaState, TopicAndMessage(..))
import qualified Network.Kafka as K
import qualified Network.Kafka.Consumer as K
import qualified Network.Kafka.Producer as K
import Network.Kafka.Protocol
    (KafkaString(..), Message(..), ProduceResponse, TopicName(..))

import CoinCoinCoin.MessageQueue.Class
import CoinCoinCoin.MessageQueue.Job
    (Enqueueable(..), Job(..), PartitionKey(..), Topic)

newtype KafkaT m a = KafkaT { unKafkaT :: ReaderT KafkaState (ExceptT KafkaClientError m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError KafkaClientError
             , MonadReader KafkaState
             )

runKafkaT :: (Monad m) => KafkaState -> KafkaT m a -> m (Either KafkaClientError a)
runKafkaT kState f = runExceptT $ runReaderT (unKafkaT f) kState

instance (MonadIO m) => MonadMessageQueue (KafkaT m) where
    produceMessages jobs =
        ask >>= \kState -> do
            result <- liftIO $ produceMessages' jobs kState
            case result of
                Left err    -> throwError err
                Right resps -> return resps

    getEarliestOffset kTopic kPartition =
        ask >>= \kState -> do
            result <- liftIO $ K.runKafka kState (K.getLastOffset K.EarliestTime kPartition kTopic)
            case result of
                Left err     -> throwError err
                Right offset -> return offset

    consumeMessages kTopic kPartition offset = do
        kState <- ask
        result <- liftIO $ K.runKafka kState $
            K.withAnyHandle (\handle ->
                K.fetch' handle =<< K.fetchRequest offset kPartition kTopic)
        case result of
            Left err   -> throwError err
            Right resp -> return $ K.fetchMessages resp

produceMessages' :: (Enqueueable a)
                 => [Job a] -> KafkaState -> IO (Either KafkaClientError [ProduceResponse])
produceMessages' jobs kState =
    let messages = map mkTopicAndMessage jobs
    in K.runKafka kState $ K.produceMessages messages

mkTopicAndMessage :: (Enqueueable a) => Job a -> TopicAndMessage
mkTopicAndMessage job =
    let t = mkTopic (topic job)
        m = mkMessage job
    in TopicAndMessage t m

mkTopic :: Topic -> TopicName
mkTopic = TName . KString . BS.pack . show

mkMessage :: (Enqueueable a) => Job a -> Message
mkMessage job =
    case partitionKey job of
        Nothing -> K.makeMessage . LBS.toStrict . AE.encode $ job
        (Just (PartitionKey pKey)) -> K.makeKeyedMessage pKey . LBS.toStrict . AE.encode $ job

