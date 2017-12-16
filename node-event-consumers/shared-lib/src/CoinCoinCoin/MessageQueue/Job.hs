module CoinCoinCoin.MessageQueue.Job
    ( Enqueueable(..)
    , Job(..)
    , Topic(..)
    , PartitionKey(..)
    , mkJob
    , mkTopic
    ) where

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           GHC.Generics           (Generic)
import qualified Network.Kafka.Protocol as K

import           CoinCoinCoin.MessageQueue.Types (Topic (..))
import           CoinCoinCoin.UUID               (UUID)
import qualified CoinCoinCoin.UUID               as UUID

class (ToJSON a, FromJSON a) => Enqueueable a where
    topic :: a -> Topic
    partitionKey :: a -> Maybe PartitionKey

newtype PartitionKey = PartitionKey ByteString
    deriving (Show, Eq)

data Job a = Job
    { jobId      :: UUID
    , payload    :: a
    , enqueuedAt :: UTCTime
    } deriving (Show, Generic)

instance ToJSON a => ToJSON (Job a)
instance FromJSON a => FromJSON (Job a)
instance (Enqueueable a) => Enqueueable (Job a) where
    topic = topic . payload
    partitionKey = partitionKey . payload

mkJob :: Enqueueable a => a -> IO (Job a)
mkJob a = do
    now <- getCurrentTime
    jId <- UUID.nextRandom
    return $ Job jId a now

mkTopic :: Topic -> K.TopicName
mkTopic = K.TName . K.KString . BS.pack . show
