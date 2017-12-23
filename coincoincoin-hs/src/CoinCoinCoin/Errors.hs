module CoinCoinCoin.Errors
    ( ConnectionError(..)
    , ParseError(..)
    ) where

import Control.Monad.Catch (Exception)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype ConnectionError = KafkaConnectionError Text
    deriving (Show, Generic)

instance Exception ConnectionError

newtype ParseError = JSONParseError Text
    deriving (Show, Generic)

instance Exception ParseError
