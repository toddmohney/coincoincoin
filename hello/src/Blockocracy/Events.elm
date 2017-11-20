module Blockocracy.Events
    exposing
        ( BlockchainEvent(..)
        , Context(..)
        , bannerMessage
        )

import Blockocracy.Proposals.Events as PE
import Blockocracy.Votes.Events as VE
import Web3.Web3 exposing (TxHash, TxReceipt)


type Context
    = Proposal
    | Vote


type BlockchainEvent
    = TxHashCreated Context (Result String TxHash)
    | TxReceiptReceived Context (Result String TxReceipt)


bannerMessage : BlockchainEvent -> String
bannerMessage bcEvt =
    case bcEvt of
        TxHashCreated ctx res ->
            case ctx of
                Proposal ->
                    txHashCreatedMessage "New proposal tx received " res

                Vote ->
                    txHashCreatedMessage "Vote tx received " res

        TxReceiptReceived ctx res ->
            case ctx of
                Proposal ->
                    txReceiptMessage PE.parseProposalAddedEvent res

                Vote ->
                    txReceiptMessage VE.parseVotedEvent res


txHashCreatedMessage : String -> Result String TxHash -> String
txHashCreatedMessage intro res =
    case res of
        Err err ->
            err

        Ok txHash ->
            intro ++ toString txHash ++ " Waiting for the tx to be mined."


txReceiptMessage : (TxReceipt -> Result String a) -> Result String TxReceipt -> String
txReceiptMessage eventParserFn res =
    case res of
        Err err ->
            err

        Ok txReceipt ->
            case eventParserFn txReceipt of
                Err err ->
                    err

                Ok evt ->
                    "Mined! Address: " ++ toString txReceipt.blockHash ++ " Block number" ++ toString txReceipt.blockNumber ++ " Gas used: " ++ toString txReceipt.cumulativeGasUsed ++ " event: " ++ toString evt
