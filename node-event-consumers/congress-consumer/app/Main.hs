module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (MonadIO)
import Network.Kafka (TopicAndMessage)

import App (AppConfig(..))
import qualified App
import CoinCoinCoin.MessageQueue (MonadMessageQueue(..))

main :: IO ()
main = do
    cfg <- App.mkAppConfig
    forever $ do
        void $ App.runAppT cfg doIt
        threadDelay (pollInterval cfg)

doIt :: ( MonadIO m
        , MonadMessageQueue m
        ) => m ()
doIt = consumeEvents >>= mapM_ processEvent

consumeEvents :: ( MonadIO m
                 , MonadMessageQueue m
                 ) => m [TopicAndMessage]
consumeEvents = undefined

processEvent :: (MonadIO m) => TopicAndMessage -> m ()
processEvent = undefined
