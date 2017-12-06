module Blockocracy.Pages.Propose exposing (Page, Msg(..), init, update, view)

import Forms.Model
    exposing
        ( Form
        , errorsLens
        )
import Blockocracy.Proposal as Prop
    exposing
        ( Proposal
        , beneficiaryLens
        , detailsLens
        , etherAmountLens
        )
import Blockocracy.Ports as Ports
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, onInput)
import Task exposing (Task)
import Errors.Pages.Errored as Errored exposing (PageLoadError, pageLoadError)
import Session exposing (Session)
import Views.TxForm as TxForm exposing (Tx)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias Page =
    { proposalForm : Form Proposal
    , session : Session
    }


type Msg
    = ProposalInputChanged InputField
    | ProposalSubmitted


type InputField
    = BeneficiaryAddress String
    | EtherAmount String
    | Details String


init : Session -> Task PageLoadError Page
init session =
    Task.succeed <|
        Page Prop.defForm session


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderProposalPanel model
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick ProposalSubmitted
            ]
            [ text "Submit Proposal" ]
        ]


renderProposalPanel : Page -> Html Msg
renderProposalPanel model =
    div
        [ class "col-sm-12" ]
        [ h2 [] [ text "Submit a Proposal" ]
        , proposalForm
        ]


proposalForm : Html Msg
proposalForm =
    div
        []
        [ h3 [] [ text "Fill out your proposal" ]
        , div
            [ class "form-group" ]
            [ label [ for "beneficiary" ] [ text "Beneficiary" ]
            , input
                [ class "form-control"
                , name "beneficiary"
                , type_ "text"
                , placeholder "Beneficiary address"
                , onInput (ProposalInputChanged << BeneficiaryAddress)
                ]
                []
            , label [ for "etherAmount" ] [ text "Amount (in Ether)" ]
            , input
                [ class "form-control"
                , name "etherAmount"
                , type_ "number"
                , placeholder "Amount (in Ether)"
                , onInput (ProposalInputChanged << EtherAmount)
                ]
                []
            , label [ for "proposalDetails" ] [ text "Proposal details" ]
            , textarea
                [ class "form-control"
                , onInput (ProposalInputChanged << Details)
                ]
                []
            ]
        ]


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        ProposalInputChanged input ->
            ( { model | proposalForm = updateProposalForm model.proposalForm input }
            , Cmd.none
            )

        ProposalSubmitted ->
            ( model
            , Ports.submitProposal <|
                Prop.toProposalRequest
                    (Tx model.session.accountAddress Web3.defaultGasPrice)
                    model.proposalForm.model
            )


updateProposalForm : Form Proposal -> InputField -> Form Proposal
updateProposalForm form input =
    case input of
        BeneficiaryAddress val ->
            beneficiaryLens.set (Web3.mkAccountAddress val) form

        EtherAmount val ->
            case String.toFloat val of
                Err err ->
                    errorsLens.set [ err ] form

                Ok eth ->
                    etherAmountLens.set eth form

        Details val ->
            detailsLens.set val form
