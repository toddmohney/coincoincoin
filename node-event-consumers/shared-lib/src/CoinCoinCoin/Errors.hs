module CoinCoinCoin.Errors
    ( ConnectionError(..)
    ) where

import           Control.Monad.Catch (Exception)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

newtype ConnectionError = KafkaConnectionError Text
    deriving (Show, Generic)

instance Exception ConnectionError
