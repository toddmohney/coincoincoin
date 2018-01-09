module Truffle.Types
    ( BuildArtifact(..)
    , Network(..)
    , NetworkId(..)
    ) where

import Data.Aeson (FromJSON(..), FromJSONKey, Value)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

import Web3.Types (Address)

data BuildArtifact = BuildArtifact
    { contractName :: Text
    , abi :: Value
    , networks :: HashMap NetworkId Network
    , updatedAt :: UTCTime
    } deriving (Show, Eq, Generic)

instance FromJSON BuildArtifact

newtype NetworkId = NetworkId Text
    deriving (Show, Eq, FromJSONKey, Generic, Hashable, IsString)

instance FromJSON NetworkId

newtype Network = Network
    { address :: Address
    } deriving (Show, Eq, Generic)

instance FromJSON Network
