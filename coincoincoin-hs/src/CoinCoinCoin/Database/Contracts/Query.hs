module CoinCoinCoin.Database.Contracts.Query
    ( upsertContract
    ) where

import Database.Persist.Postgresql

import CoinCoinCoin.Database.Models

upsertContract :: Contract -> SqlPersistT IO (Entity Contract)
upsertContract c =
    let uniqueKey = UniqueContractAddressAndNetwork (contractAddress c) (contractNetworkId c)
    in upsertBy uniqueKey c
        [ Update ContractAbi (contractAbi c) Assign
        , Update ContractUpdatedAt (contractUpdatedAt c) Assign
        ]
