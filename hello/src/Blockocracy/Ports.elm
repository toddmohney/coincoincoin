port module Blockocracy.Ports
    exposing
        ( NewProposalRequest
        , submitProposal
        , toRequest
        )

import Blockocracy.Proposal exposing (Proposal)
import Views.TxForm exposing (Tx)
import Web3.Web3 as Web3


type alias NewProposalRequest =
    { senderAddress : String
    , gasPrice : Int
    , beneficiary : String
    , etherAmount : Float
    , details : String
    }


toRequest : Tx -> Proposal -> NewProposalRequest
toRequest tx proposal =
    NewProposalRequest
        (Web3.getAccountAddress tx.senderAddress)
        tx.gasPrice
        (Web3.getAccountAddress proposal.beneficiary)
        proposal.etherAmount
        proposal.details


port submitProposal : NewProposalRequest -> Cmd msg
