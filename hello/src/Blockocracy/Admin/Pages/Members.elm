module Blockocracy.Admin.Pages.Members
    exposing
        ( Page
        , Msg(..)
        , init
        , update
        , view
        )

import Blockocracy.Ports as Ports
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Task exposing (Task)
import Errors.Pages.Errored as Errored exposing (PageLoadError, pageLoadError)
import Forms.Model as Form exposing (Form, modelLens)
import Views.TxForm as TxForm exposing (Tx, TxFormMsg(..))
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias Page =
    { txForm : Form Tx
    , memberForm : Form AccountAddress
    }


type Msg
    = TxFormInputChanged TxFormMsg String
    | InputChanged InputField String
    | MemberAdded
    | MemberRemoved


type InputField
    = MemberAddress


init : Task PageLoadError Page
init =
    Task.succeed <|
        Page
            TxForm.defForm
            (Form (Web3.mkAccountAddress "0x00") [])


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
                    ( { model | memberForm = modelLens.set (Web3.mkAccountAddress val) model.memberForm }
                    , Cmd.none
                    )

        MemberAdded ->
            ( model
            , Ports.addMember <|
                Ports.toMemberRequest model.txForm.model model.memberForm.model
            )

        MemberRemoved ->
            ( model
            , Ports.removeMember <|
                Ports.toMemberRequest model.txForm.model model.memberForm.model
            )


view : Page -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ div
                [ class "col-sm-12" ]
                [ h2 [] [ text "Add / Remove a Member" ]
                , txForm
                , memberForm
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
            [ label [ for "memberAddress" ] [ text "Member address" ]
            , input
                [ class "form-control"
                , name "memberAddress"
                , type_ "text"
                , placeholder "Member address"
                , onInput (InputChanged MemberAddress)
                ]
                []
            ]
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


txForm : Html Msg
txForm =
    div
        []
        [ h3 [] [ text "Enter your transaction details" ]
        , TxForm.render TxFormInputChanged
        ]
