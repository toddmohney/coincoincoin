module Main where

import           Control.Monad.Logger (logInfoN)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Either as E
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import           CoinCoinCoin.Class (MonadFileReader(..))
import           CoinCoinCoin.Database.Models (Contract(..))
import qualified CoinCoinCoin.Database.Models as M
import           Truffle.Types (BuildArtifact)
import qualified Truffle.Types as TT

import App (MonadDbWriter(..), runAppT)
import AppConfig (AppConfig(..), loadConfig)

main :: IO ()
main = do
    config <- loadConfig
    M.runMigrations (appDbConn config)
    runAppT config $ do
        eBuildArtifacts <- loadBuildArtifacts (appContractsPath config)
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

loadBuildArtifacts :: ( MonadFileReader m
                      ) => FilePath -> m [Either String BuildArtifact]
loadBuildArtifacts filePath =
    fmap AE.eitherDecodeStrict <$> readFilesRecursively filePath
