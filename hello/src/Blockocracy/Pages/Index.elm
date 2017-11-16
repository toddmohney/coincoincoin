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
import Blockocracy.Vote as Vote
    exposing
        ( Vote
        , proposalNumberLens
        , proposalSupportLens
        , supportJustificationLens
        )
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
    { proposalForm : Form Proposal
    , txForm : Form Tx
    , voteForm : Form Vote
    }


type Msg
    = TxFormInputChanged TxFormMsg String
    | ProposalInputChanged InputField
    | ProposalSubmitted
    | VoteInputChanged VoteInputField
    | VoteSubmitted


type InputField
    = BeneficiaryAddress String
    | EtherAmount String
    | Details String


type VoteInputField
    = ProposalNumber String
    | ProposalSupport Bool
    | SupportJustification String


init : Task PageLoadError Page
init =
    Task.succeed <|
        Page Prop.defForm TxForm.defForm Vote.defForm


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderTxForm model
            ]
        , div
            [ class "row" ]
            [ renderMembersPanel model
            , renderVotingPanel model
            ]
        ]


renderTxForm : Page -> Html Msg
renderTxForm model =
    div
        [ class "col-sm-12" ]
        [ h2 [] [ text "TX Form" ]
        , txForm model.txForm
        ]


renderMembersPanel : Page -> Html Msg
renderMembersPanel model =
    div
        [ class "col-sm-6" ]
        [ h2 [] [ text "Submit a Proposal" ]
        , proposalForm
        ]


renderVotingPanel : Page -> Html Msg
renderVotingPanel model =
    div
        [ class "col-sm-6" ]
        [ h2 [] [ text "Vote on a Proposal" ]
        , voteForm model
        ]


txForm : Form Tx -> Html Msg
txForm form =
    div
        []
        [ h3 [] [ text "Enter your transaction details" ]
        , TxForm.render form TxFormInputChanged
        ]


voteForm : Page -> Html Msg
voteForm model =
    div
        []
        [ h3 [] [ text "Complete your ballot" ]
        , div
            [ class "form-group" ]
            [ label [ for "proposalNumber" ] [ text "Proposal number" ]
            , input
                [ class "form-control"
                , name "proposalNumber"
                , type_ "numeric"
                , placeholder "Proposal number"
                , onInput (VoteInputChanged << ProposalNumber)
                ]
                []
            , div
                [ class "checkbox" ]
                [ label
                    [ for "supportsProposal" ]
                    [ input
                        [ name "supportsProposal"
                        , type_ "checkbox"
                        , onCheck (VoteInputChanged << ProposalSupport)
                        ]
                        []
                    , text "I support this proposal"
                    ]
                ]
            , label [ for "justification" ] [ text "Justificaiton" ]
            , textarea
                [ class "form-control"
                , onInput (VoteInputChanged << SupportJustification)
                ]
                []
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick VoteSubmitted
            ]
            [ text "Submit Ballot" ]
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
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick ProposalSubmitted
            ]
            [ text "Submit Proposal" ]
        ]


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        ProposalInputChanged input ->
            ( { model | proposalForm = updateProposalForm model.proposalForm input }
            , Cmd.none
            )

        VoteInputChanged input ->
            ( { model | voteForm = updateVoteForm model.voteForm input }
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

        VoteSubmitted ->
            ( model
            , Ports.submitVote <| Ports.toVoteRequest model.txForm.model model.voteForm.model
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


updateVoteForm : Form Vote -> VoteInputField -> Form Vote
updateVoteForm form input =
    case input of
        ProposalNumber val ->
            case String.toInt val of
                Err err ->
                    errorsLens.set [ err ] form

                Ok num ->
                    proposalNumberLens.set num form

        ProposalSupport val ->
            proposalSupportLens.set val form

        SupportJustification val ->
            supportJustificationLens.set val form
