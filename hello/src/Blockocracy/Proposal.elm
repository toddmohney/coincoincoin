module Blockocracy.Proposal
    exposing
        ( Proposal
        , ProposalRequest
        , ProposalResponse
        , beneficiaryLens
        , defForm
        , detailsLens
        , etherAmountLens
        , toProposalRequest
        , proposalResponseDecoder
        )

import Forms.Model exposing (..)
import Json.Decode as Decode exposing (int, string, nullable, Decoder, Value)
import Json.Decode.Pipeline exposing (decode, required)
import Monocle.Lens exposing (..)
import Views.TxForm exposing (Tx)
import Web3.Web3 as Web3 exposing (AccountAddress(..), Address, BigNumber)


type alias Proposal =
    { beneficiary : AccountAddress
    , etherAmount : Float
    , details : String
    }


type alias ProposalRequest =
    { senderAddress : String
    , gasPrice : Int
    , beneficiary : String
    , etherAmount : Float
    , details : String
    }


type alias ProposalResponse =
    { amount : BigNumber
    , currentResult : Int
    , description : String
    , executed : Bool
    , numberOfVotes : Int
    , proposalPassed : Bool
    , proposalHash : Address
    , recipient : AccountAddress
    , votingDeadline : Int
    }


proposalResponseDecoder : Decoder ProposalResponse
proposalResponseDecoder =
    decode ProposalResponse
        |> required "amount" Web3.bigNumberDecoder
        |> required "currentResult" Decode.int
        |> required "description" Decode.string
        |> required "executed" Decode.bool
        |> required "numberOfVotes" Decode.int
        |> required "proposalPassed" Decode.bool
        |> required "proposalHash" Web3.addressDecoder
        |> required "recipient" Web3.accountDecoder
        |> required "votingDeadline" Decode.int


toProposalRequest : Tx -> Proposal -> ProposalRequest
toProposalRequest tx proposal =
    ProposalRequest
        (Web3.getAccountAddress tx.senderAddress)
        tx.gasPrice
        (Web3.getAccountAddress proposal.beneficiary)
        proposal.etherAmount
        proposal.details


defForm : Form Proposal
defForm =
    Form defProposal []


defProposal : Proposal
defProposal =
    Proposal (Web3.mkAccountAddress "0x00") 0 ""


beneficiaryLens : Lens (Form Proposal) AccountAddress
beneficiaryLens =
    Lens beneficiaryGetter beneficiarySetter


beneficiaryGetter : Form Proposal -> AccountAddress
beneficiaryGetter f =
    f.model.beneficiary


beneficiarySetter : AccountAddress -> Form Proposal -> Form Proposal
beneficiarySetter addr f =
    let
        proposal =
            f.model
    in
        { f | model = { proposal | beneficiary = addr } }


etherAmountLens : Lens (Form Proposal) Float
etherAmountLens =
    Lens etherAmountGetter etherAmountSetter


etherAmountGetter : Form Proposal -> Float
etherAmountGetter f =
    f.model.etherAmount


etherAmountSetter : Float -> Form Proposal -> Form Proposal
etherAmountSetter amt f =
    let
        proposal =
            f.model
    in
        { f | model = { proposal | etherAmount = amt } }


detailsLens : Lens (Form Proposal) String
detailsLens =
    Lens detailsGetter detailsSetter


detailsGetter : Form Proposal -> String
detailsGetter f =
    f.model.details


detailsSetter : String -> Form Proposal -> Form Proposal
detailsSetter details f =
    let
        proposal =
            f.model
    in
        { f | model = { proposal | details = details } }
