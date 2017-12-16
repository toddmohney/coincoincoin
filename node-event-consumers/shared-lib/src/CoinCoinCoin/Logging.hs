module CoinCoinCoin.Logging
    ( runLogging
    , runLogging'
    ) where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger
    ( LogLevel (..)
    , LogSource
    , LoggingT (..)
    )
import qualified Control.Monad.Logger   as L
import           Data.Monoid            ((<>))
import           Data.Time.Clock        (getCurrentTime)

import CoinCoinCoin.Environment (Environment (..))

runLogging :: (MonadIO m) => Bool -> Environment -> LoggingT m a -> m a
runLogging isDebugging env =
    let logFilterFn = logFilter isDebugging env
    in L.runStdoutLoggingT . addTimestamp . L.filterLogger logFilterFn

runLogging' :: (MonadIO m) => LoggingT m a -> m a
runLogging' = runLogging False Production

addTimestamp :: LoggingT m a -> LoggingT m a
addTimestamp (LoggingT f) = LoggingT $ \logger ->
    f $ \loc src lvl msg -> do
        now <- getCurrentTime
        let tsMsg = "[" <> L.toLogStr (show now) <> "] " <> msg
        logger loc src lvl tsMsg

logFilter :: Bool -> Environment -> (LogSource -> LogLevel -> Bool)
logFilter isDebugging env _ lvl =
    isDebugging || envLogFilter env
    where
        envLogFilter Test        = lvl >= LevelWarn
        envLogFilter Development = lvl >= LevelInfo
        envLogFilter Production  = lvl >= LevelInfo
