port module Blockocracy.Ports
    exposing
        ( addMember
        , removeMember
        , getProposal
        , proposalReceived
        , submitProposal
        , submitVote
        )

import Blockocracy.Members.Model exposing (MemberRequest)
import Blockocracy.Proposal exposing (ProposalRequest)
import Blockocracy.Vote exposing (VoteRequest)
import Json.Decode exposing (Value)


port getProposal : Int -> Cmd msg


port proposalReceived : (Value -> msg) -> Sub msg


port submitProposal : ProposalRequest -> Cmd msg


port submitVote : VoteRequest -> Cmd msg


port addMember : MemberRequest -> Cmd msg


port removeMember : MemberRequest -> Cmd msg
