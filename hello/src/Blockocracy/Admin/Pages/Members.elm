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
import Blockocracy.Vote exposing (VotingRules)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Task exposing (Task)
import Errors.Pages.Errored as Errored exposing (PageLoadError, pageLoadError)
import Forms.Model as Form exposing (Form, modelLens)
import Views.TxForm as TxForm exposing (Tx, TxFormMsg(..))
import Web3.Web3 as Web3


type alias Page =
    { txForm : Form Tx
    , memberForm : Form Member
    , votingRules : External VotingRules
    }


type Msg
    = TxFormInputChanged TxFormMsg String
    | InputChanged InputField String
    | MemberAdded
    | MemberRemoved
    | VotingRulesLoaded (Result String VotingRules)


type InputField
    = MemberAddress
    | MemberName


init : Task PageLoadError Page
init =
    Task.succeed <|
        Page
            TxForm.defForm
            (Form (Member (Web3.mkAccountAddress "0x00") "") [])
            Loading


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        TxFormInputChanged msg val ->
            ( { model | txForm = TxForm.updateForm model.txForm val msg }
            , Cmd.none
            )

        InputChanged field val ->
            case field of
                MemberAddress ->
                    ( { model | memberForm = accountLens.set (Web3.mkAccountAddress val) model.memberForm }
                    , Cmd.none
                    )

                MemberName ->
                    ( { model | memberForm = nameLens.set val model.memberForm }
                    , Cmd.none
                    )

        MemberAdded ->
            ( model
            , Ports.addMember <|
                Member.toMemberRequest model.txForm.model model.memberForm.model
            )

        MemberRemoved ->
            ( model
            , Ports.removeMember <|
                Member.toMemberRequest model.txForm.model model.memberForm.model
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
                [ renderCongressVotingRules model
                ]
            ]
        ]


renderMemberManagement : Page -> Html Msg
renderMemberManagement model =
    div
        []
        [ h2 [] [ text "Add / Remove a Member" ]
        , memberForm
        , txForm model.txForm
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


renderCongressVotingRules : Page -> Html Msg
renderCongressVotingRules model =
    case model.votingRules of
        NotLoaded ->
            div
                []
                [ h2 [] [ text "Voting Rules" ]
                , div
                    []
                    [ text <| "Waiting to load..."
                    ]
                ]

        Loading ->
            div
                []
                [ h2 [] [ text "Voting Rules" ]
                , div
                    []
                    [ text <| "Loading..."
                    ]
                ]

        LoadError err ->
            div
                []
                [ h2 [] [ text "Voting Rules" ]
                , div
                    []
                    [ text <| "An error occurred:"
                    , text err
                    ]
                ]

        Loaded rules ->
            div
                []
                [ h2 [] [ text "Voting Rules" ]
                , div
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
                ]


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
                , onInput (InputChanged MemberName)
                ]
                []
            , label [ for "memberAddress" ] [ text "Member address" ]
            , input
                [ class "form-control"
                , name "memberAddress"
                , type_ "text"
                , placeholder "Member address"
                , onInput (InputChanged MemberAddress)
                ]
                []
            ]
        ]


txForm : Form Tx -> Html Msg
txForm form =
    div
        []
        [ h3 [] [ text "Enter your transaction details" ]
        , TxForm.render form TxFormInputChanged
        ]
