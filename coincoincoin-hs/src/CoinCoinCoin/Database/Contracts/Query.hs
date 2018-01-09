module CoinCoinCoin.Database.Contracts.Query
    ( upsertContract
    , upsertNetwork
    ) where

import Data.Text (Text)
import Database.Persist.Postgresql

import CoinCoinCoin.Database.Models
import qualified Truffle.Types as TT

findContractByName :: Text -> SqlPersistT IO (Maybe (Entity Contract))
findContractByName name =
    getBy (UniqueContractName name)

findNetwork :: TT.NetworkId -> ContractId -> SqlPersistT IO (Maybe (Entity Network))
findNetwork nId cId =
    getBy (UniqueNetworkIdAndContractId nId cId)

upsertContract :: Contract -> SqlPersistT IO ContractId
upsertContract c =
    findContractByName (contractName c) >>= \case
        Nothing -> insert c
        (Just (Entity contractId _)) -> do
            replace contractId c
            return contractId

upsertNetwork :: Network -> SqlPersistT IO NetworkId
upsertNetwork n =
    findNetwork (networkNetworkId n) (networkContractId n) >>= \case
        Nothing -> insert n
        (Just (Entity networkId _)) -> do
            replace networkId n
            return networkId
