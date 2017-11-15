port module Blockocracy.Ports
    exposing
        ( submitProposal
        )

import Blockocracy.Proposal exposing (Proposal)


type alias NewProposalRequest =
    { senderAddress : String
    , gasPrice : Int
    , proposal : Proposal
    }


port submitProposal : NewProposalRequest -> Cmd msg
