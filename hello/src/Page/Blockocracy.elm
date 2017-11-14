module Page.Blockocracy exposing (BlockocracyPage, Msg(..), init, update, view)

import Data.Form as Form
    exposing
        ( Form
        , errorsLens
        )
import Data.Proposal as Prop
    exposing
        ( Proposal
        , beneficiaryLens
        , detailsLens
        , etherAmountLens
        , weiGasPriceLens
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task exposing (Task)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias BlockocracyPage =
    { proposalForm : Form Proposal
    }


init : Task PageLoadError BlockocracyPage
init =
    Task.succeed <|
        BlockocracyPage Prop.defForm


view : BlockocracyPage -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderMembersPanel
            , renderAdminPanel
            ]
        ]


renderMembersPanel : Html Msg
renderMembersPanel =
    div
        [ class "col-sm-6" ]
        [ h2 [] [ text "Submit a Proposal" ]
        , proposalForm
        ]


proposalForm : Html Msg
proposalForm =
    div
        []
        [ div
            [ class "form-group" ]
            [ label [ for "beneficiary" ] [ text "Beneficiary" ]
            , input
                [ class "form-control"
                , name "beneficiary"
                , type_ "text"
                , placeholder "Beneficiary address"
                , onInput (InputChanged BeneficiaryAddress)
                ]
                []
            , label [ for "etherAmount" ] [ text "Amount (in Ether)" ]
            , input
                [ class "form-control"
                , name "etherAmount"
                , type_ "number"
                , placeholder "Amount (in Ether)"
                , onInput (InputChanged EtherAmount)
                ]
                []
            , label [ for "proposalDetails" ] [ text "Proposal details" ]
            , textarea
                [ class "form-control"
                , onInput (InputChanged Details)
                ]
                []
            , label [ for "gasPrice" ] [ text "Gas price (in wei)" ]
            , input
                [ class "form-control"
                , name "gasPrice"
                , type_ "number"
                , value "20000000000"
                , placeholder "Gas price (in Wei)"
                , onInput (InputChanged GasPrice)
                ]
                []
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick ProposalSubmitted
            ]
            [ text "Submit Proposal" ]
        ]


renderAdminPanel : Html Msg
renderAdminPanel =
    div
        [ class "col-sm-6" ]
        [ text "Hi, I'm the admin panel" ]


type Msg
    = InputChanged InputField String
    | ProposalSubmitted


type InputField
    = BeneficiaryAddress
    | EtherAmount
    | Details
    | GasPrice


update : Msg -> BlockocracyPage -> ( BlockocracyPage, Cmd Msg )
update msg model =
    case msg of
        InputChanged BeneficiaryAddress addr ->
            ( { model | proposalForm = beneficiaryLens.set (Web3.mkAccountAddress addr) model.proposalForm }
            , Cmd.none
            )

        InputChanged EtherAmount amt ->
            case String.toFloat amt of
                Err err ->
                    ( { model | proposalForm = errorsLens.set [ err ] model.proposalForm }
                    , Cmd.none
                    )

                Ok eth ->
                    ( { model | proposalForm = etherAmountLens.set eth model.proposalForm }
                    , Cmd.none
                    )

        InputChanged Details desc ->
            ( { model | proposalForm = detailsLens.set desc model.proposalForm }
            , Cmd.none
            )

        InputChanged GasPrice gas ->
            case String.toInt gas of
                Err err ->
                    ( { model | proposalForm = errorsLens.set [ err ] model.proposalForm }
                    , Cmd.none
                    )

                Ok wei ->
                    ( { model | proposalForm = weiGasPriceLens.set wei model.proposalForm }
                    , Cmd.none
                    )

        ProposalSubmitted ->
            ( model, Cmd.none )
