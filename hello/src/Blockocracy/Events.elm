module Blockocracy.Events
    exposing
        ( BlockchainEvent(..)
        )

import Web3.Web3 exposing (TxReceipt)


type BlockchainEvent
    = ProposalAdded (Result String TxReceipt)
