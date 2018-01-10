{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module CoinCoinCoin.Classes.MonadTime
    ( MonadTime(..)
    , UTCTime
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Logger (LoggingT)
import           Control.Monad.Reader (ReaderT, MonadTrans)
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as Time

class (MonadIO m) => MonadTime m where
    getCurrentTime :: m UTCTime

    default getCurrentTime :: (MonadTrans t, MonadTime m1, m ~ t m1) => m UTCTime
    getCurrentTime = liftIO getCurrentTime

instance (MonadIO m, MonadTime m) => MonadTime (ReaderT s m)
instance (MonadIO m, MonadTime m) => MonadTime (LoggingT m)

instance MonadTime IO where
    getCurrentTime = Time.getCurrentTime
