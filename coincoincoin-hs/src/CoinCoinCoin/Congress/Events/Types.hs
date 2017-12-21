module CoinCoinCoin.Congress.Events.Types
    ( Voted(..)
    , VotedReturnValues(..)
    ) where

import Data.Aeson (FromJSON(..), Value(..))
import Data.Aeson.Types as AE
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)

import Web3.Types (Address(..))

data Voted = Voted
    deriving (Show, Eq, Generic)

instance FromJSON Voted where
    parseJSON str@(String s) =
        case TL.unpack (TL.fromStrict s) of
            "Voted" -> return Voted
            _  -> AE.typeMismatch "Unable to parse as Voted: " str
    parseJSON v = AE.typeMismatch "Unable to parse as Voted" v


data VotedReturnValues = VotedReturnValues
    { proposalID :: Text
    , position :: Bool
    , voter :: Address
    , justification :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON VotedReturnValues
