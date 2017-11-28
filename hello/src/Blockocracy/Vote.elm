module Blockocracy.Vote
    exposing
        ( Vote
        , VoteRequest
        , VotingRules
        , VotingRulesRequest
        , toVoteRequest
        , toVotingRulesRequest
        , votingRulesDecoder
        )

import Json.Decode as Decode exposing (int, Decoder, Value)
import Json.Decode.Pipeline exposing (decode, required)
import Web3.Web3 as Web3 exposing (AccountAddress(..))
import Views.TxForm exposing (Tx)


type alias Vote =
    { proposalNumber : Int
    , proposalSupport : Bool
    , supportJustification : String
    }


type alias VoteRequest =
    { senderAddress : String
    , gasPrice : Int
    , proposalNumber : Int
    , proposalSupport : Bool
    , supportJustification : String
    }


type alias VotingRules =
    { minimumQuorum : Int
    , debatingPeriodInMinutes : Int
    , majorityMargin : Int
    }


type alias VotingRulesRequest =
    { senderAddress : String
    , gasPrice : Int
    , minimumQuorum : Int
    , debatingPeriodInMinutes : Int
    , majorityMargin : Int
    }


votingRulesDecoder : Decoder VotingRules
votingRulesDecoder =
    decode VotingRules
        |> required "minimumQuorum" Decode.int
        |> required "debatingPeriodInMinutes" Decode.int
        |> required "majorityMargin" Decode.int


toVoteRequest : Tx -> Vote -> VoteRequest
toVoteRequest tx vote =
    VoteRequest
        (Web3.getAccountAddress tx.senderAddress)
        tx.gasPrice
        vote.proposalNumber
        vote.proposalSupport
        vote.supportJustification


toVotingRulesRequest : Tx -> VotingRules -> VotingRulesRequest
toVotingRulesRequest tx votingRules =
    VotingRulesRequest
        (Web3.getAccountAddress tx.senderAddress)
        tx.gasPrice
        votingRules.minimumQuorum
        votingRules.debatingPeriodInMinutes
        votingRules.majorityMargin
