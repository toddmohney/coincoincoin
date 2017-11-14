module Data.Proposal
    exposing
        ( Proposal
        , beneficiaryLens
        , defForm
        , detailsLens
        , etherAmountLens
        , weiGasPriceLens
        )

import Data.Form exposing (..)
import Monocle.Lens exposing (..)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias Proposal =
    { beneficiary : AccountAddress
    , etherAmount : Float
    , details : String
    , weiGasPrice : Int
    }


defForm : Form Proposal
defForm =
    Form defProposal []


defProposal : Proposal
defProposal =
    Proposal (Web3.mkAccountAddress "0x00") 0 "" 0


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

        newProposal =
            { proposal | beneficiary = addr }
    in
        { f | model = newProposal }


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

        newProposal =
            { proposal | etherAmount = amt }
    in
        { f | model = newProposal }


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

        newProposal =
            { proposal | details = details }
    in
        { f | model = newProposal }


weiGasPriceLens : Lens (Form Proposal) Int
weiGasPriceLens =
    Lens weiGasPriceGetter weiGasPriceSetter


weiGasPriceGetter : Form Proposal -> Int
weiGasPriceGetter f =
    f.model.weiGasPrice


weiGasPriceSetter : Int -> Form Proposal -> Form Proposal
weiGasPriceSetter weiGasPrice f =
    let
        proposal =
            f.model

        newProposal =
            { proposal | weiGasPrice = weiGasPrice }
    in
        { f | model = newProposal }
