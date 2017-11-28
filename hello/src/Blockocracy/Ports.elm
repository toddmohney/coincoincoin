port module Blockocracy.Ports
    exposing
        ( addMember
        , executeProposal
        , getProposal
        , getVotingRules
        , proposalReceived
        , removeMember
        , submitProposal
        , submitVote
        , updateVotingRules
        , votingRulesReceived
        )

import Blockocracy.Members.Model exposing (MemberRequest)
import Blockocracy.Proposal exposing (ProposalRequest, ProposalExecutionRequest)
import Blockocracy.Vote exposing (VoteRequest, VotingRulesRequest)
import Json.Decode exposing (Value)


port getProposal : Int -> Cmd msg


port proposalReceived : (Value -> msg) -> Sub msg


port submitProposal : ProposalRequest -> Cmd msg


port executeProposal : ProposalExecutionRequest -> Cmd msg


port submitVote : VoteRequest -> Cmd msg


port addMember : MemberRequest -> Cmd msg


port removeMember : MemberRequest -> Cmd msg


port getVotingRules : String -> Cmd msg


port votingRulesReceived : (Value -> msg) -> Sub msg


port updateVotingRules : VotingRulesRequest -> Cmd msg
