module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (MonadIO)

import App (AppConfig(..))
import qualified App

main :: IO ()
main = do
    cfg <- App.mkAppConfig
    forever $ do
        void $ App.runAppT cfg doIt
        threadDelay (pollInterval cfg)

doIt :: (MonadIO m) => m ()
doIt = undefined
