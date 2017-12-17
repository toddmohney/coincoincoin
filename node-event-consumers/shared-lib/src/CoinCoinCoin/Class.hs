module CoinCoinCoin.Class
    ( MonadStats(..)
    , MonadTime(..)
    , UTCTime
    , module CoinCoinCoin.StatsD
    ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

import CoinCoinCoin.StatsD

class (Monad m) => MonadTime m where
    getCurrentTime :: m UTCTime

instance MonadTime IO where
    getCurrentTime = Time.getCurrentTime

class (MonadIO m) => MonadStats m where
    recordStats :: [Stat] -> m ()
    recordTiming :: Bucket -> IO a -> m a
