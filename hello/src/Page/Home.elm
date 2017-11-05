module Page.Home exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (disabled, class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Ports as Ports
import Task exposing (Task)
import Web3.Web3 as Web3 exposing (AccountAddress(..), Tx)


-- MODEL --


type alias Model =
    { helloForm : HelloForm
    , helloCount : Maybe Int
    , sayHelloResult : Maybe HelloResult
    }

updateTxHash : Model -> String -> Model
updateTxHash model txHash =
    case model.sayHelloResult of
        Nothing ->
            let result = { defResult | txHash = Just txHash }
            in
                { model | sayHelloResult = Just result }

        (Just helloResult) ->
            let newResult = { helloResult | txHash = Just txHash }
            in
                { model | sayHelloResult = Just newResult }

updateTx : Model -> Tx -> Model
updateTx model tx =
    case model.sayHelloResult of
        Nothing ->
            let result = { defResult | tx = Just tx }
            in
                { model | sayHelloResult = Just result }

        (Just helloResult) ->
            let newResult = { helloResult | tx = Just tx }
            in
                { model | sayHelloResult = Just newResult }

updateTxConfirmations : Model -> Int -> Model
updateTxConfirmations model ct =
    case model.sayHelloResult of
        Nothing ->
            let result = { defResult | txConfirmations = ct }
            in
                { model | sayHelloResult = Just result }

        (Just helloResult) ->
            let newResult = { helloResult | txConfirmations = ct }
            in
                { model | sayHelloResult = Just newResult }

type alias HelloForm =
    { address : Maybe AccountAddress
    , isValid : Bool
    , errors  : List String
    }

type alias HelloResult =
    { txHash : Maybe String
    , tx : Maybe Tx
    , txConfirmations : Int
    }

defResult : HelloResult
defResult = HelloResult Nothing Nothing 0


init : Task PageLoadError Model
init =
    Task.succeed
        <| Model (HelloForm Nothing False []) Nothing Nothing



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ renderHelloForm model.helloForm
        , hr [] []
        , renderHelloCount model.helloCount
        , renderSayHelloResult model.sayHelloResult
        ]

renderHelloCount : Maybe Int -> Html Msg
renderHelloCount mCount =
    case mCount of
        Nothing -> div [] []
        (Just ct) ->
            div
                []
                [ text <| "You have said hello to the blockchain " ++ toString ct ++ " times."
                ]

renderSayHelloResult : Maybe HelloResult -> Html Msg
renderSayHelloResult mResult =
    case mResult of
        Nothing -> div [] []
        (Just result) ->
            case result.txHash of
                Nothing ->
                    div
                        [ class "text-danger" ]
                        [ text "Missing tx hash!" ]
                (Just txHash) ->
                    div []
                        [ div [ class "text-success" ] [ text "Tx received!" ]
                        , div [] [ text <| "Tx Hash " ++ txHash ]
                        , div [] [ text <| "Tx Confirmations " ++ toString result.txConfirmations ]
                        , renderTx result.tx
                        ]

renderTx : Maybe Tx -> Html Msg
renderTx mTx =
    case mTx of
        Nothing ->
            div [ class "text-warning" ] [ text "Waiting for tx to be mined" ]
        (Just tx) ->
            div []
                [ div [] [ text "Tx mined!" ]
                , pre [] [ text << String.join "\n" << String.split "," <| toString tx ]
                ]

renderHelloForm : HelloForm -> Html Msg
renderHelloForm form =
    div []
        [ div
            [ class "form-group" ]
            [ input
                [ class "form-control"
                , type_ "text"
                , placeholder "AccountAddress"
                , onInput AccountAddressInput
                ]
                []
            , renderFormValidation form
            ]

        , button
            [ classList [("btn", True), ("btn-primary", True)]
            , disabled <| not form.isValid
            , onClick SayHelloRequested
            ]
            [ text "Say Hello" ]

        , button
            [ classList [("btn", True), ("btn-default", True)]
            , disabled <| not form.isValid
            , onClick GetHelloRequested
            ]
            [ text "Get Hello Count" ]
        ]

renderFormValidation : HelloForm -> Html Msg
renderFormValidation form =
    div [ class "text-danger" ]
        [ text <| String.concat form.errors
        ]



-- UPDATE --


type Msg = AccountAddressInput String
         | GetHelloRequested
         | HelloCountReceived Int
         | SayHelloRequested
         | HelloTxReceived String
         | HelloTxReceiptReceived (Result String Tx)
         | HelloTxConfirmed Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AccountAddressInput addr ->
            let
                form = model.helloForm
                newForm = validateForm { form | address = Just (Web3.mkAccountAddress addr) }
                newModel = { model | helloForm = newForm }
            in
                (newModel, Cmd.none)

        SayHelloRequested ->
            case model.helloForm.address of
                Nothing ->
                    (model, Cmd.none)
                (Just addr) ->
                    (model, Ports.sayHello <| Web3.getAccountAddress addr)

        GetHelloRequested ->
            case model.helloForm.address of
                Nothing ->
                    (model, Cmd.none)
                (Just addr) ->
                    (model, Ports.getHelloCount <| Web3.getAccountAddress addr)

        HelloCountReceived ct ->
            let newModel = { model | helloCount = Just ct }
            in (newModel, Cmd.none)

        HelloTxReceived txHash ->
            let newModel = updateTxHash model txHash
            in (newModel, Cmd.none)

        HelloTxConfirmed txCt ->
            let newModel = updateTxConfirmations model txCt
            in (newModel, Cmd.none)

        HelloTxReceiptReceived result ->
            case result of
                (Err err) ->
                    Debug.log err (model, Cmd.none)
                (Ok tx) ->
                    let newModel = updateTx model tx
                    in (newModel, Cmd.none)


validateForm : HelloForm -> HelloForm
validateForm form =
    case form.address of
        Nothing ->
            { form | isValid = True
            , errors = ["Please enter your blockchain address"]
            }
        (Just addr) ->
            if addrLength addr == addrLength Web3.sampleAccountAddress
                then
                    { form | isValid = True
                    , errors = []
                    }
                else
                    { form | isValid = False
                    , errors = ["Your blockchain address must be " ++ toString (addrLength Web3.sampleAccountAddress) ++ " characters long"]
                    }

addrLength : AccountAddress -> Int
addrLength = String.length << Web3.getAccountAddress
