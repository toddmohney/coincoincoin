module Blockocracy.Admin.Pages.Members
    exposing
        ( Page
        , Msg(..)
        , init
        , update
        , view
        )

import Async exposing (External(..))
import Blockocracy.Members.Model as Member exposing (Member, accountLens, nameLens)
import Blockocracy.Ports as Ports
import Blockocracy.Proposal as Proposal
    exposing
        ( ProposalExecution
        , ProposalResponse
        , proposalIDLens
        )
import Blockocracy.Vote as Vote exposing (VotingRules)
import Blockocracy.Votes.VotingRulesForms as VRF
    exposing
        ( minimumQuorumLens
        , debatingPeriodInMinutesLens
        , majorityMarginLens
        )
import Forms.Model exposing (errorsLens)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Task exposing (Task)
import Errors.Pages.Errored as Errored exposing (PageLoadError, pageLoadError)
import Forms.Model as Form exposing (Form, modelLens)
import Session exposing (Session)
import Views.TxForm as TxForm exposing (Tx)
import Web3.Web3 as Web3


type alias Page =
    { memberForm : Form Member
    , votingRules : External VotingRules
    , votingRulesForm : Form VotingRules
    , proposalExecutionForm : Form ProposalExecution
    , selectedProposal : Maybe ProposalResponse
    , session : Session
    }


type Msg
    = InputChanged InputField
    | MemberAdded
    | MemberRemoved
    | VotingRulesUpdated
    | VotingRulesLoaded (Result String VotingRules)
    | ProposalExecuted
    | ProposalLoaded (Result String ProposalResponse)


type InputField
    = MemberAddress String
    | MemberName String
    | MinimumQuorum String
    | DebatePeriod String
    | MajorityMargin String
    | ProposalID String


init : Session -> Task PageLoadError Page
init session =
    Task.succeed <|
        Page
            (Form (Member (Web3.mkAccountAddress "0x00") "") [])
            Loading
            VRF.defForm
            (Form (ProposalExecution 0) [])
            Nothing
            session


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        InputChanged field ->
            case field of
                MemberAddress val ->
                    ( { model | memberForm = accountLens.set (Web3.mkAccountAddress val) model.memberForm }
                    , Cmd.none
                    )

                MemberName val ->
                    ( { model | memberForm = nameLens.set val model.memberForm }
                    , Cmd.none
                    )

                MinimumQuorum val ->
                    case String.toInt val of
                        Err err ->
                            ( { model | votingRulesForm = errorsLens.set [ err ] model.votingRulesForm }
                            , Cmd.none
                            )

                        Ok quorum ->
                            ( { model | votingRulesForm = minimumQuorumLens.set quorum model.votingRulesForm }
                            , Cmd.none
                            )

                DebatePeriod val ->
                    case String.toInt val of
                        Err err ->
                            ( { model | votingRulesForm = errorsLens.set [ err ] model.votingRulesForm }
                            , Cmd.none
                            )

                        Ok minutes ->
                            ( { model | votingRulesForm = debatingPeriodInMinutesLens.set minutes model.votingRulesForm }
                            , Cmd.none
                            )

                MajorityMargin val ->
                    case String.toInt val of
                        Err err ->
                            ( { model | votingRulesForm = errorsLens.set [ err ] model.votingRulesForm }
                            , Cmd.none
                            )

                        Ok margin ->
                            ( { model | votingRulesForm = majorityMarginLens.set margin model.votingRulesForm }
                            , Cmd.none
                            )

                ProposalID val ->
                    case String.toInt val of
                        Err err ->
                            ( { model | proposalExecutionForm = errorsLens.set [ err ] model.proposalExecutionForm }
                            , Cmd.none
                            )

                        Ok propId ->
                            ( { model | proposalExecutionForm = proposalIDLens.set propId model.proposalExecutionForm }
                            , Ports.getProposal propId
                            )

        MemberAdded ->
            ( model
            , Ports.addMember <|
                Member.toMemberRequest
                    (Tx model.session.accountAddress Web3.defaultGasPrice)
                    model.memberForm.model
            )

        MemberRemoved ->
            ( model
            , Ports.removeMember <|
                Member.toMemberRequest
                    (Tx model.session.accountAddress Web3.defaultGasPrice)
                    model.memberForm.model
            )

        VotingRulesUpdated ->
            ( model
            , Ports.updateVotingRules <|
                Vote.toVotingRulesRequest
                    (Tx model.session.accountAddress Web3.defaultGasPrice)
                    model.votingRulesForm.model
            )

        VotingRulesLoaded result ->
            case result of
                Err err ->
                    ( { model | votingRules = LoadError err }
                    , Cmd.none
                    )

                Ok rules ->
                    ( { model | votingRules = Loaded rules }
                    , Cmd.none
                    )

        ProposalLoaded result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok proposalResponse ->
                    ( { model | selectedProposal = Just proposalResponse }
                    , Cmd.none
                    )

        ProposalExecuted ->
            ( model
            , Ports.executeProposal <|
                Proposal.toProposalExecutionRequest
                    (Tx model.session.accountAddress Web3.defaultGasPrice)
                    model.proposalExecutionForm.model
            )


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-sm-6" ]
                [ renderMemberManagement model
                ]
            , div
                [ class "col-sm-6" ]
                [ renderVotingRules model
                , renderChangeVotingRulesForm model
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-sm-6" ]
                [ renderProposalExecutionForm model
                ]
            , proposalPreview model
            ]
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


renderProposalExecutionForm : Page -> Html Msg
renderProposalExecutionForm model =
    div
        [ class "clearfix" ]
        [ h3 [] [ text "Execute Proposal" ]
        , div
            [ class "form-group" ]
            [ label [ for "proposalNumber" ] [ text "Proposal number" ]
            , input
                [ class "form-control"
                , name "proposalNumber"
                , type_ "numeric"
                , value <| toString model.proposalExecutionForm.model.proposalID
                , onInput (InputChanged << ProposalID)
                ]
                []
            ]
        , button
            [ classList [ ( "pull-left", True ), ( "btn", True ), ( "btn-primary", True ) ]
            , onClick ProposalExecuted
            ]
            [ text "Execute Proposal" ]
        ]


renderMemberManagement : Page -> Html Msg
renderMemberManagement model =
    div
        []
        [ h2 [] [ text "Add / Remove a Member" ]
        , memberForm
        , button
            [ classList [ ( "pull-left", True ), ( "btn", True ), ( "btn-danger", True ) ]
            , onClick MemberRemoved
            ]
            [ text "Remove Member" ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick MemberAdded
            ]
            [ text "Add New Member" ]
        ]


renderChangeVotingRulesForm : Page -> Html Msg
renderChangeVotingRulesForm model =
    div
        []
        [ h3 [] [ text "Update Voting Rules" ]
        , div
            [ class "form-group" ]
            [ label [ for "minimumQuorum" ] [ text "Minimum quorum" ]
            , input
                [ class "form-control"
                , name "minimumQuorum"
                , type_ "numeric"
                , value <| toString model.votingRulesForm.model.minimumQuorum
                , onInput (InputChanged << MinimumQuorum)
                ]
                []
            , label [ for "debatePeriod" ] [ text "Debate period in minutes" ]
            , input
                [ class "form-control"
                , name "debatePeriod"
                , type_ "numeric"
                , value <| toString model.votingRulesForm.model.debatingPeriodInMinutes
                , onInput (InputChanged << DebatePeriod)
                ]
                []
            , label [ for "majorityMargin" ] [ text "Majority margin required" ]
            , input
                [ class "form-control"
                , name "majorityMargin"
                , type_ "numeric"
                , value <| toString model.votingRulesForm.model.majorityMargin
                , onInput (InputChanged << MajorityMargin)
                ]
                []
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , onClick VotingRulesUpdated
            ]
            [ text "Update Voting Rules" ]
        ]


renderVotingRules : Page -> Html Msg
renderVotingRules model =
    let
        body =
            case model.votingRules of
                NotLoaded ->
                    div [] [ text <| "Waiting to load..." ]

                Loading ->
                    div [] [ text <| "Loading..." ]

                LoadError err ->
                    div [] [ text <| "An error occurred:", text err ]

                Loaded rules ->
                    div
                        []
                        [ div
                            []
                            [ strong [] [ text "Minimum quorum: " ]
                            , div [ class "pull-right" ] [ text <| toString rules.minimumQuorum ]
                            ]
                        , div
                            []
                            [ strong [] [ text "Debate period (minutes): " ]
                            , div [ class "pull-right" ] [ text <| toString rules.debatingPeriodInMinutes ]
                            ]
                        , div
                            []
                            [ strong [] [ text "Majority margin required: " ]
                            , div [ class "pull-right" ] [ text <| toString rules.majorityMargin ]
                            ]
                        ]
    in
        div [] [ h2 [] [ text "Voting Rules" ], body ]


memberForm : Html Msg
memberForm =
    div
        []
        [ h3 [] [ text "Enter the Member's Address" ]
        , div
            [ class "form-group" ]
            [ label [ for "memberName" ] [ text "Member name" ]
            , input
                [ class "form-control"
                , name "memberName"
                , type_ "text"
                , placeholder "Member name"
                , onInput (InputChanged << MemberName)
                ]
                []
            , label [ for "memberAddress" ] [ text "Member address" ]
            , input
                [ class "form-control"
                , name "memberAddress"
                , type_ "text"
                , placeholder "Member address"
                , onInput (InputChanged << MemberAddress)
                ]
                []
            ]
        ]
