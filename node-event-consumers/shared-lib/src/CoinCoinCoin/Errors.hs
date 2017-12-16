module CoinCoinCoin.Errors
    ( ConnectionError(..)
    , NotFoundError(..)
    ) where

import           Control.Monad.Catch (Exception)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

newtype ConnectionError = KafkaConnectionError Text
    deriving (Show, Generic)

instance Exception ConnectionError

data NotFoundError = RecordNotFound String
    deriving (Show, Generic)

instance Exception NotFoundError
