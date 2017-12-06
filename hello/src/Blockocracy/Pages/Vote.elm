module Blockocracy.Pages.Vote exposing (Page, Msg(..), init, update, view)

import Forms.Model
    exposing
        ( Form
        , errorsLens
        )
import Blockocracy.Proposal exposing (ProposalResponse)
import Blockocracy.Ports as Ports
import Blockocracy.Vote as Vote exposing (Vote)
import Blockocracy.Votes.Forms as VoteForm
    exposing
        ( proposalNumberLens
        , proposalSupportLens
        , supportJustificationLens
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, onInput)
import Session exposing (Session)
import Task exposing (Task)
import Errors.Pages.Errored as Errored exposing (PageLoadError)
import Views.TxForm as TxForm exposing (Tx)
import Web3.Web3 as Web3


type alias Page =
    { voteForm : Form Vote
    , selectedProposal : Maybe ProposalResponse
    , session : Session
    }


type Msg
    = VoteInputChanged VoteInputField
    | VoteSubmitted
    | ProposalLoaded (Result String ProposalResponse)


type VoteInputField
    = ProposalNumber String
    | ProposalSupport Bool
    | SupportJustification String


init : Session -> Task PageLoadError Page
init session =
    Task.succeed <|
        Page VoteForm.defForm Nothing session


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderVotingPanel model
            , proposalPreview model
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick VoteSubmitted
            ]
            [ text "Submit Ballot" ]
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

        VoteSubmitted ->
            ( model
            , Ports.submitVote <|
                Vote.toVoteRequest
                    (Tx model.session.accountAddress Web3.defaultGasPrice)
                    model.voteForm.model
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
