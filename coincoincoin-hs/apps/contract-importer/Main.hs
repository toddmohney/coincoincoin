module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Concurrent.Async as A
import qualified Control.Monad as M
import Control.Monad.Logger (logInfoN)
import qualified Data.Aeson as AE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either as E
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Prelude hiding (readFile)
import qualified System.FilePath as FP

import CoinCoinCoin.Class (MonadFileReader(..))
import CoinCoinCoin.Database.Models
    ( Contract(..)
    )
import qualified CoinCoinCoin.Database.Models as M
import Truffle.Types (BuildArtifact)
import qualified Truffle.Types as TT

import App (AppT, MonadDbWriter(..), runAppT)
import AppConfig (AppConfig(..), loadConfig)

main :: IO ()
main = do
    config <- loadConfig
    M.runMigrations (appDbConn config)
    runAppT config $ do
        eBuildArtifacts <- fmap AE.eitherDecodeStrict <$> loadBuildArtifacts (appContractsPath config) :: AppT IO [Either String BuildArtifact]
        let (errs, buildArtifacts) = E.partitionEithers eBuildArtifacts
        logInfoN . T.pack $ show config
        logInfoN . T.pack . show $ length errs
        logInfoN . T.pack . show $ length buildArtifacts

        upsertBuildArtifacts buildArtifacts

upsertBuildArtifacts :: (MonadDbWriter m) => [BuildArtifact] -> m ()
upsertBuildArtifacts artifacts =
    let contracts = concatMap mkContracts artifacts
    in mapM_ upsertContract contracts

mkContracts :: BuildArtifact -> [Contract]
mkContracts a =
    HM.foldrWithKey (\k v acc -> acc ++ [mkContract a k v]) [] (TT.networks a)

mkContract :: BuildArtifact -> TT.NetworkId -> TT.Network -> Contract
mkContract a nId n =
    Contract
        (TT.contractName a)
        nId
        (TT.address n)
        (LBS.toStrict . AE.encode $ TT.abi a)
        (TT.updatedAt a)

loadBuildArtifacts :: (MonadIO m, MonadFileReader m) => FilePath -> m [ByteString]
loadBuildArtifacts path = do
    isDir <- doesDirectoryExist path
    if isDir
    then do
        files <- fmap (path FP.</>) <$> listDirectory path
        M.join <$> liftIO (A.mapConcurrently loadBuildArtifacts files)
    else
        (:[]) <$> readFile path
