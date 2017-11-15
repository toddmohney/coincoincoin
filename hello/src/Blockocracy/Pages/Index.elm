module Blockocracy.Pages.Index exposing (Page, Msg(..), init, update, view)

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
import Html.Events exposing (onClick, onInput)
import Task exposing (Task)
import Errors.Pages.Errored as Errored exposing (PageLoadError, pageLoadError)
import Views.TxForm as TxForm
    exposing
        ( Tx
        , TxFormMsg(..)
        )
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias Page =
    { proposalForm : Form Proposal
    , txForm : Form Tx
    }


init : Task PageLoadError Page
init =
    Task.succeed <|
        Page Prop.defForm TxForm.defForm


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderMembersPanel
            ]
        ]


renderMembersPanel : Html Msg
renderMembersPanel =
    div
        [ class "col-sm-12" ]
        [ h2 [] [ text "Submit a Proposal" ]
        , txForm
        , proposalForm
        ]


txForm : Html Msg
txForm =
    div
        []
        [ h3 [] [ text "Enter your transaction details" ]
        , TxForm.render TxFormInputChanged
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
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick ProposalSubmitted
            ]
            [ text "Submit Proposal" ]
        ]


type Msg
    = TxFormInputChanged TxFormMsg String
    | InputChanged InputField String
    | ProposalSubmitted


type InputField
    = BeneficiaryAddress
    | EtherAmount
    | Details


update : Msg -> Page -> ( Page, Cmd Msg )
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

        TxFormInputChanged msg val ->
            ( { model | txForm = TxForm.updateForm model.txForm val msg }
            , Cmd.none
            )

        ProposalSubmitted ->
            ( model
            , Ports.submitProposal <| Ports.toNewProposalRequest model.txForm.model model.proposalForm.model
            )
