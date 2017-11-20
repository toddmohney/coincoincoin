module Blockocracy.Proposal
    exposing
        ( Proposal
        , ProposalRequest
        , beneficiaryLens
        , defForm
        , detailsLens
        , etherAmountLens
        , toProposalRequest
        )

import Forms.Model exposing (..)
import Monocle.Lens exposing (..)
import Views.TxForm exposing (Tx)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


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
