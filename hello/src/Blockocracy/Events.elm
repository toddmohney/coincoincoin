module Blockocracy.Events
    exposing
        ( BlockchainEvent(..)
        , bannerMessage
        )

import Blockocracy.Proposals.Events as PE
import Web3.Web3 exposing (TxHash, TxReceipt)


type BlockchainEvent
    = ProposalAdded (Result String TxReceipt)
    | ProposalAddedTxHashCreated (Result String TxHash)


bannerMessage : BlockchainEvent -> String
bannerMessage bcEvt =
    case bcEvt of
        ProposalAdded result ->
            case result of
                Err err ->
                    err

                Ok txReceipt ->
                    case PE.parseProposalAddedEvent txReceipt of
                        Err err ->
                            err

                        Ok evt ->
                            "Mined! Address: " ++ toString txReceipt.blockHash ++ " Block number" ++ toString txReceipt.blockNumber ++ " Gas used: " ++ toString txReceipt.cumulativeGasUsed ++ " event: " ++ toString evt

        ProposalAddedTxHashCreated result ->
            case result of
                Err err ->
                    err

                Ok txHash ->
                    "Tx received " ++ toString txHash ++ " Waiting for the tx to be mined."
