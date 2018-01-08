module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Concurrent.Async as A
import qualified Control.Monad as M
import Control.Monad.Logger (logInfoN)
import qualified Data.Aeson as AE
import Data.ByteString (ByteString)
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Text as T
import Prelude hiding (readFile)
import qualified System.FilePath as FP

import CoinCoinCoin.Class (MonadFileReader(..))
import Truffle.Types (BuildArtifact)

import App (AppT, runAppT)
import AppConfig (AppConfig(..), loadConfig)

main :: IO ()
main = do
    config <- loadConfig
    runAppT config $ do
        buildArtifacts <- fmap AE.eitherDecodeStrict <$> loadBuildArtifacts (appContractsPath config) :: AppT IO [Either String BuildArtifact]
        let (lefts, rights) = L.partition E.isRight buildArtifacts
        logInfoN . T.pack $ show config
        logInfoN . T.pack . show $ length lefts
        logInfoN . T.pack . show $ length rights

loadBuildArtifacts :: (MonadIO m, MonadFileReader m) => FilePath -> m [ByteString]
loadBuildArtifacts path = do
    isDir <- doesDirectoryExist path
    if isDir
    then do
        files <- fmap (path FP.</>) <$> listDirectory path
        M.join <$> liftIO (A.mapConcurrently loadBuildArtifacts files)
    else
        (:[]) <$> readFile path
