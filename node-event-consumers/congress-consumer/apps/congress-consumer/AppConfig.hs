module AppConfig
    ( AppConfig(..)
    , mkAppConfig
    ) where

import qualified Data.ByteString.Char8  as BS
import qualified Data.Text          as T
import           Network.Kafka          (KafkaState)
import qualified Network.Kafka          as K
import           Network.Kafka.Protocol (Host (..), KafkaString (..),
                                         Partition (..), Port (..))
import           System.Environment (getEnv)

import CoinCoinCoin.Database.Config (ConnectionPool, mkPool)
import CoinCoinCoin.Database.Models (KafkaClientId(..))
import CoinCoinCoin.Environment     (Environment (..))

data AppConfig = AppConfig
    { appEnv        :: Environment
    , appDbConn     :: ConnectionPool
    , appKafkaClientId  :: KafkaClientId
    , appKafkaPartition :: Partition
    , kafkaState        :: KafkaState
    , pollInterval      :: Int -- ^ microsecs
    }
    deriving (Show)

mkAppConfig :: IO AppConfig
mkAppConfig =  do
    env <- read <$> getEnv "ENV"
    kState <- mkKafkaState
    pInterval <- read <$> getEnv "POLL_INTERVAL"
    kClientId <- KafkaClientId . T.pack <$> getEnv "KAFKA_CLIENT_ID"
    kPartition <- Partition . read <$> getEnv "KAFKA_PARTITION"
    dbPool    <- mkPool env
    return AppConfig
        { appEnv = env
        , appDbConn = dbPool
        , appKafkaClientId = kClientId
        , appKafkaPartition = kPartition
        , kafkaState = kState
        , pollInterval = pInterval
        }

mkKafkaState :: IO KafkaState
mkKafkaState = do
    kId   <- getEnv "KAFKA_CLIENT_ID"
    kHost <- getEnv "KAFKA_HOST"
    kPort <- getEnv "KAFKA_PORT"
    return $ K.mkKafkaState
        (KString (BS.pack kId))
        (Host (KString $ BS.pack kHost), Port $ read kPort)
