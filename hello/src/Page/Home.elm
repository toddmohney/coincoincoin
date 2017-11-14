module Page.Home exposing (HomePage, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (disabled, class, classList, for, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (Value)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Ports as Ports exposing (SayHelloRequest)
import Task exposing (Task)
import Web3.Web3 as Web3 exposing (AccountAddress(..), TxHash(..), Tx, TxReceipt)


-- MODEL --


type alias HomePage =
    { helloForm : HelloForm
    , helloCount : Maybe Int
    , sayHelloResult : Maybe HelloResult
    , txForm : TxForm
    , txResult : Maybe TxResult
    }


updateTxHash : HomePage -> String -> HomePage
updateTxHash model txHash =
    let
        newHelloResult =
            case model.sayHelloResult of
                Nothing ->
                    { defResult | txHash = Just txHash }

                Just helloResult ->
                    { helloResult | txHash = Just txHash }
    in
        { model | sayHelloResult = Just newHelloResult }


updateHelloResult : HomePage -> TxReceipt -> HomePage
updateHelloResult model tx =
    let
        newResult =
            case model.sayHelloResult of
                Nothing ->
                    { defResult | tx = Just tx }

                Just helloResult ->
                    { helloResult | tx = Just tx }
    in
        { model | sayHelloResult = Just newResult }


updateTxResult : HomePage -> Tx -> HomePage
updateTxResult model tx =
    let
        newResult =
            case model.txResult of
                Nothing ->
                    TxResult tx

                Just txResult ->
                    { txResult | tx = tx }
    in
        { model | txResult = Just newResult }


updateTxConfirmations : HomePage -> Int -> HomePage
updateTxConfirmations model ct =
    let
        newResult =
            case model.sayHelloResult of
                Nothing ->
                    { defResult | txConfirmations = ct }

                Just helloResult ->
                    { helloResult | txConfirmations = ct }
    in
        { model | sayHelloResult = Just newResult }


type alias HelloForm =
    { address : Maybe AccountAddress
    , gasPrice : Int
    , isValid : Bool
    , errors : List String
    }


defHelloForm : HelloForm
defHelloForm =
    HelloForm Nothing 20000000000 False []


type alias HelloResult =
    { txHash : Maybe String
    , tx : Maybe TxReceipt
    , txConfirmations : Int
    }


defResult : HelloResult
defResult =
    HelloResult Nothing Nothing 0


type alias TxForm =
    { tx : Maybe TxHash
    , isValid : Bool
    , errors : List String
    }


defTxForm : TxForm
defTxForm =
    TxForm Nothing False []


type alias TxResult =
    { tx : Tx
    }


init : Task PageLoadError HomePage
init =
    Task.succeed <|
        HomePage defHelloForm Nothing Nothing defTxForm Nothing



-- VIEW --


view : HomePage -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            [ renderHelloContainer model
            , renderTxContainer model
            ]
        ]


renderHelloContainer : HomePage -> Html Msg
renderHelloContainer model =
    div [ class "col-md-6" ]
        [ renderHelloForm model.helloForm
        , hr [] []
        , renderHelloCount model.helloCount
        , renderSayHelloResult model.sayHelloResult
        ]


renderTxContainer : HomePage -> Html Msg
renderTxContainer model =
    div [ class "col-md-6" ]
        [ renderTxForm model.txForm
        , renderTx model.txResult
        ]


renderTx : Maybe TxResult -> Html Msg
renderTx mTx =
    case mTx of
        Nothing ->
            div [ class "text-warning" ] []

        Just tx ->
            div []
                [ div [] [ text "Tx" ]
                , pre [] [ text << String.join "\n" << String.split "," <| toString tx ]
                ]


renderHelloCount : Maybe Int -> Html Msg
renderHelloCount mCount =
    case mCount of
        Nothing ->
            div [] []

        Just ct ->
            div
                []
                [ text <| "You have said hello to the blockchain " ++ toString ct ++ " times."
                ]


renderSayHelloResult : Maybe HelloResult -> Html Msg
renderSayHelloResult mResult =
    case mResult of
        Nothing ->
            div [] []

        Just result ->
            case result.txHash of
                Nothing ->
                    div
                        [ class "text-danger" ]
                        [ text "Missing tx hash!" ]

                Just txHash ->
                    div []
                        [ div [ class "text-success" ] [ text "TxReceipt received!" ]
                        , div [] [ text <| "Tx Hash " ++ txHash ]
                        , div [] [ text <| "Tx Confirmations " ++ toString result.txConfirmations ]
                        , renderTxReceipt result.tx
                        ]


renderTxReceipt : Maybe TxReceipt -> Html Msg
renderTxReceipt mTxReceipt =
    case mTxReceipt of
        Nothing ->
            div [ class "text-warning" ] [ text "Waiting for tx to be mined" ]

        Just txReceipt ->
            div []
                [ div [] [ text "Tx mined!" ]
                , pre [] [ text << String.join "\n" << String.split "," <| toString txReceipt ]
                ]


renderTxForm : TxForm -> Html Msg
renderTxForm form =
    div []
        [ div
            [ class "form-group" ]
            [ label [ for "txAddress" ] [ text "Tx address" ]
            , input
                [ class "form-control"
                , name "txAddress"
                , type_ "text"
                , placeholder "Tx address"
                , onInput TxAddressInput
                ]
                []
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-default", True ) ]
            , disabled <| not form.isValid
            , onClick GetTxRequested
            ]
            [ text "Get Tx" ]
        ]


renderHelloForm : HelloForm -> Html Msg
renderHelloForm form =
    div []
        [ div
            [ class "form-group" ]
            [ label [ for "accountAddress" ] [ text "Account address" ]
            , input
                [ class "form-control"
                , name "accountAddress"
                , type_ "text"
                , placeholder "Account address"
                , onInput AccountAddressInput
                ]
                []
            , label [ for "gasPrice" ] [ text "Gas price (in wei)" ]
            , input
                [ class "form-control"
                , name "gasPrice"
                , type_ "number"
                , value "20000000000"
                , placeholder "Gas price (in Wei)"
                , onInput GasPriceInput
                ]
                []
            , renderFormValidation form
            ]
        , button
            [ classList [ ( "btn", True ), ( "btn-primary", True ) ]
            , disabled <| not form.isValid
            , onClick SayHelloRequested
            ]
            [ text "Say Hello" ]
        , button
            [ classList [ ( "btn", True ), ( "btn-default", True ) ]
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


type Msg
    = AccountAddressInput String
    | GasPriceInput String
    | TxAddressInput String
    | GetHelloRequested
    | HelloCountReceived Int
    | SayHelloRequested
    | HelloTxReceived String
    | HelloTxReceiptReceived (Result String TxReceipt)
    | HelloTxConfirmed Int
    | HelloTxError Value
    | GetTxRequested
    | TxReceived (Result String Tx)


update : Msg -> HomePage -> ( HomePage, Cmd Msg )
update msg model =
    case msg of
        AccountAddressInput addr ->
            let
                form =
                    model.helloForm

                newForm =
                    validateHelloForm { form | address = Just (Web3.mkAccountAddress addr) }

                newHomePage =
                    { model | helloForm = newForm }
            in
                ( newHomePage, Cmd.none )

        GasPriceInput price ->
            let
                form =
                    model.helloForm

                newForm =
                    case String.toInt price of
                        Err err ->
                            { form | isValid = False, errors = [ err ] }

                        Ok p ->
                            validateHelloForm { form | gasPrice = p }

                newHomePage =
                    { model | helloForm = newForm }
            in
                ( newHomePage, Cmd.none )

        TxAddressInput addr ->
            let
                form =
                    model.txForm

                newForm =
                    validateTxForm { form | tx = Just (Web3.mkTxHash addr) }

                newHomePage =
                    { model | txForm = newForm }
            in
                ( newHomePage, Cmd.none )

        SayHelloRequested ->
            case model.helloForm.address of
                Nothing ->
                    ( model, Cmd.none )

                Just addr ->
                    ( model
                    , Ports.sayHello <| SayHelloRequest (Web3.getAccountAddress addr) (model.helloForm.gasPrice)
                    )

        GetHelloRequested ->
            case model.helloForm.address of
                Nothing ->
                    ( model, Cmd.none )

                Just addr ->
                    ( model
                    , Ports.getHelloCount <| Web3.getAccountAddress addr
                    )

        HelloCountReceived ct ->
            let
                newHomePage =
                    { model | helloCount = Just ct }
            in
                ( newHomePage, Cmd.none )

        HelloTxReceived txHash ->
            let
                newHomePage =
                    updateTxHash model txHash
            in
                ( newHomePage, Cmd.none )

        HelloTxConfirmed txCt ->
            let
                newHomePage =
                    updateTxConfirmations model txCt
            in
                ( newHomePage, Cmd.none )

        HelloTxReceiptReceived result ->
            case result of
                Err err ->
                    Debug.log err ( model, Cmd.none )

                Ok tx ->
                    let
                        newHomePage =
                            updateHelloResult model tx
                    in
                        ( newHomePage, Cmd.none )

        HelloTxError err ->
            Debug.log "Error!" ( model, Cmd.none )

        GetTxRequested ->
            case model.txForm.tx of
                Nothing ->
                    ( model, Cmd.none )

                Just addr ->
                    ( model
                    , Ports.getTx <| Web3.getTxHash addr
                    )

        TxReceived result ->
            case result of
                Err err ->
                    Debug.log err ( model, Cmd.none )

                Ok tx ->
                    let
                        newHomePage =
                            updateTxResult model tx
                    in
                        ( newHomePage, Cmd.none )


validateTxForm : TxForm -> TxForm
validateTxForm form =
    case form.tx of
        Nothing ->
            { form
                | isValid = False
                , errors = [ "Please enter your tx address" ]
            }

        Just addr ->
            if txHashLength addr == txHashLength Web3.sampleTxHash then
                { form
                    | isValid = True
                    , errors = []
                }
            else
                { form
                    | isValid = False
                    , errors = [ "Your tx address must be " ++ toString (txHashLength Web3.sampleTxHash) ++ " characters long" ]
                }


validateHelloForm : HelloForm -> HelloForm
validateHelloForm form =
    case form.address of
        Nothing ->
            { form
                | isValid = False
                , errors = [ "Please enter your blockchain address" ]
            }

        Just addr ->
            if addrLength addr == addrLength Web3.sampleAccountAddress then
                { form
                    | isValid = True
                    , errors = []
                }
            else
                { form
                    | isValid = False
                    , errors = [ "Your blockchain address must be " ++ toString (addrLength Web3.sampleAccountAddress) ++ " characters long" ]
                }


addrLength : AccountAddress -> Int
addrLength =
    String.length << Web3.getAccountAddress


txHashLength : TxHash -> Int
txHashLength =
    String.length << Web3.getTxHash
