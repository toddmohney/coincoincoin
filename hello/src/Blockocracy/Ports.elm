port module Blockocracy.Ports
    exposing
        ( NewProposalRequest
        , addMember
        , removeMember
        , submitProposal
        , submitVote
        , toMemberRequest
        , toNewProposalRequest
        , toVoteRequest
        )

import Blockocracy.Members.Model exposing (Member)
import Blockocracy.Proposal exposing (Proposal)
import Blockocracy.Vote exposing (Vote)
import Views.TxForm exposing (Tx)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias NewProposalRequest =
    { senderAddress : String
    , gasPrice : Int
    , beneficiary : String
    , etherAmount : Float
    , details : String
    }


type alias VoteRequest =
    { senderAddress : String
    , gasPrice : Int
    , proposalNumber : Int
    , proposalSupport : Bool
    , supportJustification : String
    }


type alias MemberRequest =
    { senderAddress : String
    , gasPrice : Int
    , memberAddress : String
    , memberName : String
    }


toNewProposalRequest : Tx -> Proposal -> NewProposalRequest
toNewProposalRequest tx proposal =
    NewProposalRequest
        (Web3.getAccountAddress tx.senderAddress)
        tx.gasPrice
        (Web3.getAccountAddress proposal.beneficiary)
        proposal.etherAmount
        proposal.details


toVoteRequest : Tx -> Vote -> VoteRequest
toVoteRequest tx vote =
    VoteRequest
        (Web3.getAccountAddress tx.senderAddress)
        tx.gasPrice
        vote.proposalNumber
        vote.proposalSupport
        vote.supportJustification


toMemberRequest : Tx -> Member -> MemberRequest
toMemberRequest tx member =
    MemberRequest
        (Web3.getAccountAddress tx.senderAddress)
        tx.gasPrice
        (Web3.getAccountAddress member.account)
        member.name


port submitProposal : NewProposalRequest -> Cmd msg


port submitVote : VoteRequest -> Cmd msg


port addMember : MemberRequest -> Cmd msg


port removeMember : MemberRequest -> Cmd msg
