module Blockocracy.Vote
    exposing
        ( Vote
        , VoteRequest
        , VotingRules
        , proposalNumberLens
        , proposalSupportLens
        , supportJustificationLens
        , defForm
        , toVoteRequest
        , votingRulesDecoder
        )

import Forms.Model exposing (..)
import Json.Decode as Decode exposing (int, Decoder, Value)
import Json.Decode.Pipeline exposing (decode, required)
import Monocle.Lens exposing (..)
import Views.TxForm exposing (Tx)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


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


defForm : Form Vote
defForm =
    Form defVote []


defVote : Vote
defVote =
    Vote 0 False ""


proposalNumberLens : Lens (Form Vote) Int
proposalNumberLens =
    Lens proposalNumberGetter proposalNumberSetter


proposalNumberGetter : Form Vote -> Int
proposalNumberGetter f =
    f.model.proposalNumber


proposalNumberSetter : Int -> Form Vote -> Form Vote
proposalNumberSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | proposalNumber = val } }


proposalSupportLens : Lens (Form Vote) Bool
proposalSupportLens =
    Lens proposalSupportGetter proposalSupportSetter


proposalSupportGetter : Form Vote -> Bool
proposalSupportGetter f =
    f.model.proposalSupport


proposalSupportSetter : Bool -> Form Vote -> Form Vote
proposalSupportSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | proposalSupport = val } }


supportJustificationLens : Lens (Form Vote) String
supportJustificationLens =
    Lens supportJustificationGetter supportJustificationSetter


supportJustificationGetter : Form Vote -> String
supportJustificationGetter f =
    f.model.supportJustification


supportJustificationSetter : String -> Form Vote -> Form Vote
supportJustificationSetter val f =
    let
        formModel =
            f.model
    in
        { f | model = { formModel | supportJustification = val } }
