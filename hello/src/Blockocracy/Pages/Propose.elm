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
import Views.TxForm as TxForm
    exposing
        ( Tx
        , TxFormMsg(..)
        )
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias Page =
    { txForm : Form Tx
    , proposalForm : Form Proposal
    }


type Msg
    = TxFormInputChanged TxFormMsg String
    | ProposalInputChanged InputField
    | ProposalSubmitted


type InputField
    = BeneficiaryAddress String
    | EtherAmount String
    | Details String


init : Task PageLoadError Page
init =
    Task.succeed <|
        Page TxForm.defForm Prop.defForm


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderProposalPanel model
            ]
        , div
            [ class "row" ]
            [ renderTxForm model
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick ProposalSubmitted
            ]
            [ text "Submit Proposal" ]
        ]


renderTxForm : Page -> Html Msg
renderTxForm model =
    div
        [ class "col-sm-12" ]
        [ h2 [] [ text "TX Form" ]
        , txForm model.txForm
        ]


renderProposalPanel : Page -> Html Msg
renderProposalPanel model =
    div
        [ class "col-sm-12" ]
        [ h2 [] [ text "Submit a Proposal" ]
        , proposalForm
        ]


txForm : Form Tx -> Html Msg
txForm form =
    div
        []
        [ h3 [] [ text "Enter your transaction details" ]
        , TxForm.render form TxFormInputChanged
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

        TxFormInputChanged msg val ->
            ( { model | txForm = TxForm.updateForm model.txForm val msg }
            , Cmd.none
            )

        ProposalSubmitted ->
            ( model
            , Ports.submitProposal <| Prop.toProposalRequest model.txForm.model model.proposalForm.model
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
