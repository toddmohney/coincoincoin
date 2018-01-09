module CoinCoinCoin.Database.Contracts.Query
    ( upsertContract
    ) where

import Database.Persist.Postgresql

import CoinCoinCoin.Database.Models
import Truffle.Types (NetworkId)
import Web3.Types (Address)

findContract :: Address -> NetworkId -> SqlPersistT IO (Maybe (Entity Contract))
findContract addr networkId =
    getBy (UniqueContractAddressAndNetwork addr networkId)

upsertContract :: Contract -> SqlPersistT IO ContractId
upsertContract c =
    findContract (contractAddress c) (contractNetworkId c) >>= \case
        Nothing -> insert c
        (Just (Entity contractId _)) -> do
            replace contractId c
            return contractId
