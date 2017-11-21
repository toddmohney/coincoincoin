module Blockocracy.Pages.Vote exposing (Page, Msg(..), init, update, view)

import Forms.Model
    exposing
        ( Form
        , errorsLens
        )
import Blockocracy.Proposal exposing (ProposalResponse)
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
    , selectedProposal : Maybe ProposalResponse
    }


type Msg
    = TxFormInputChanged TxFormMsg String
    | VoteInputChanged VoteInputField
    | VoteSubmitted
    | ProposalLoaded (Result String ProposalResponse)


type VoteInputField
    = ProposalNumber String
    | ProposalSupport Bool
    | SupportJustification String


init : Task PageLoadError Page
init =
    Task.succeed <|
        Page TxForm.defForm Vote.defForm Nothing


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderVotingPanel model
            , proposalPreview model
            ]
        , div
            [ class "row" ]
            [ renderTxForm model
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick VoteSubmitted
            ]
            [ text "Submit Ballot" ]
        ]


renderTxForm : Page -> Html Msg
renderTxForm model =
    div
        [ class "col-sm-6" ]
        [ h2 [] [ text "TX Form" ]
        , txForm model.txForm
        ]


renderVotingPanel : Page -> Html Msg
renderVotingPanel model =
    div
        [ class "col-sm-6" ]
        [ h2 [] [ text "Vote on a Proposal" ]
        , voteForm model
        ]


proposalPreview : Page -> Html Msg
proposalPreview model =
    case model.selectedProposal of
        Nothing ->
            div [] []

        Just proposal ->
            div
                [ class "col-sm-6" ]
                [ h2 [] [ text "Selected Proposal" ]
                , pre
                    []
                    [ text << String.join "\n," <| String.split "," (toString proposal)
                    ]
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
        ]


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        VoteInputChanged input ->
            ( { model | voteForm = updateVoteForm model.voteForm input }
            , inputEffects input
            )

        TxFormInputChanged msg val ->
            ( { model | txForm = TxForm.updateForm model.txForm val msg }
            , Cmd.none
            )

        VoteSubmitted ->
            ( model
            , Ports.submitVote <| Vote.toVoteRequest model.txForm.model model.voteForm.model
            )

        ProposalLoaded result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok proposalResponse ->
                    ( { model | selectedProposal = Just proposalResponse }
                    , Cmd.none
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


inputEffects : VoteInputField -> Cmd Msg
inputEffects msg =
    case msg of
        ProposalNumber val ->
            case String.toInt val of
                Err _ ->
                    Cmd.none

                Ok num ->
                    Ports.getProposal num

        _ ->
            Cmd.none
