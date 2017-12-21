module Web3.Types
    ( Address(..)
    , Event(..)
    , EventId(..)
    , Hash(..)
    ) where

import           Data.Aeson (FromJSON(..), (.:))
import qualified Data.Aeson as AE
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)


data Event a b = Event
    { eventAddress          :: Address
    , eventBlockNumber      :: Int
    , eventTransactionHash  :: Hash
    , eventTransactionIndex :: Int
    , eventBlockHash        :: Hash
    , eventLogIndex         :: Int
    , eventRemoved          :: Bool
    , eventId               :: EventId
    , eventReturnValues     :: b
    , eventEvent            :: a
    , eventSignature        :: Hash
    } deriving (Show, Generic)

instance (FromJSON a, FromJSON b) => FromJSON (Event a b) where
    parseJSON = AE.withObject "Event" $ \o ->
        Event
            <$> o .: "address"
            <*> o .: "blockNumber"
            <*> o .: "transactionHash"
            <*> o .: "transactionIndex"
            <*> o .: "blockHash"
            <*> o .: "logIndex"
            <*> o .: "removed"
            <*> o .: "id"
            <*> o .: "returnValues"
            <*> o .: "event"
            <*> o .: "signature"

newtype Address = Address Text
    deriving (Show, Eq, Ord, Generic)

instance IsString Address where
    fromString = Address . T.pack

instance FromJSON Address


newtype Hash = Hash Text
    deriving (Show, Eq, Ord, Generic)

instance IsString Hash where
    fromString = Hash . T.pack

instance FromJSON Hash


newtype EventId = EventId Text
    deriving (Show, Eq, Ord, Generic)

instance IsString EventId where
    fromString = EventId . T.pack

instance FromJSON EventId
