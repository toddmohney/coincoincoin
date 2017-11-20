module Blockocracy.Pages.Vote exposing (Page, Msg(..), init, update, view)

import Forms.Model
    exposing
        ( Form
        , errorsLens
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
import Errors.Pages.Errored as Errored exposing (PageLoadError)
import Views.TxForm as TxForm exposing (Tx, TxFormMsg(..))


type alias Page =
    { txForm : Form Tx
    , voteForm : Form Vote
    }


type Msg
    = TxFormInputChanged TxFormMsg String
    | VoteInputChanged VoteInputField
    | VoteSubmitted


type VoteInputField
    = ProposalNumber String
    | ProposalSupport Bool
    | SupportJustification String


init : Task PageLoadError Page
init =
    Task.succeed <|
        Page TxForm.defForm Vote.defForm


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderTxForm model
            ]
        , div
            [ class "row" ]
            [ renderVotingPanel model
            ]
        ]


renderTxForm : Page -> Html Msg
renderTxForm model =
    div
        [ class "col-sm-12" ]
        [ h2 [] [ text "TX Form" ]
        , txForm model.txForm
        ]


renderVotingPanel : Page -> Html Msg
renderVotingPanel model =
    div
        [ class "col-sm-12" ]
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


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        VoteInputChanged input ->
            ( { model | voteForm = updateVoteForm model.voteForm input }
            , Cmd.none
            )

        TxFormInputChanged msg val ->
            ( { model | txForm = TxForm.updateForm model.txForm val msg }
            , Cmd.none
            )

        VoteSubmitted ->
            ( model
            , Ports.submitVote <| Vote.toVoteRequest model.txForm.model model.voteForm.model
            )


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
