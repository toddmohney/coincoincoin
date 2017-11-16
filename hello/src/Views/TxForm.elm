module Views.TxForm
    exposing
        ( Tx
        , TxFormMsg(..)
        , TxFormMsgWrapper
        , defForm
        , render
        , gasPriceLens
        , senderAddressLens
        , updateForm
        )

import Forms.Model exposing (Form, errorsLens)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Monocle.Lens exposing (..)
import Web3.Web3 as Web3 exposing (AccountAddress(..))


type alias Tx =
    { senderAddress : AccountAddress
    , gasPrice : Int
    }


type TxFormMsg
    = AddressInputChanged
    | GasPriceInputChanged


type alias TxFormMsgWrapper msg =
    TxFormMsg -> String -> msg


defForm : Form Tx
defForm =
    Form
        (Tx (Web3.mkAccountAddress "0x00") 20000000000)
        []


updateForm : Form Tx -> String -> TxFormMsg -> Form Tx
updateForm form val msg =
    case msg of
        AddressInputChanged ->
            senderAddressLens.set (Web3.mkAccountAddress val) form

        GasPriceInputChanged ->
            case String.toInt val of
                Err err ->
                    errorsLens.set [ err ] form

                Ok gas ->
                    gasPriceLens.set gas form


senderAddressLens : Lens (Form Tx) AccountAddress
senderAddressLens =
    Lens senderAddressGetter senderAddressSetter


senderAddressGetter : Form Tx -> AccountAddress
senderAddressGetter f =
    f.model.senderAddress


senderAddressSetter : AccountAddress -> Form Tx -> Form Tx
senderAddressSetter addr f =
    let
        tx =
            f.model
    in
        { f | model = { tx | senderAddress = addr } }


gasPriceLens : Lens (Form Tx) Int
gasPriceLens =
    Lens gasPriceGetter gasPriceSetter


gasPriceGetter : Form Tx -> Int
gasPriceGetter f =
    f.model.gasPrice


gasPriceSetter : Int -> Form Tx -> Form Tx
gasPriceSetter gas f =
    let
        tx =
            f.model
    in
        { f | model = { tx | gasPrice = gas } }


render : Form Tx -> TxFormMsgWrapper msg -> Html msg
render form msgFn =
    div
        [ class "form-group" ]
        [ label [ for "accountAddress" ] [ text "Account address" ]
        , input
            [ class "form-control"
            , name "accountAddress"
            , type_ "text"
            , placeholder "Account address"
            , onInput (msgFn AddressInputChanged)
            ]
            []
        , label [ for "gasPrice" ] [ text "Gas price (in wei)" ]
        , input
            [ class "form-control"
            , name "gasPrice"
            , type_ "number"
            , value << toString <| gasPriceLens.get form
            , placeholder "Gas price (in Wei)"
            , onInput (msgFn GasPriceInputChanged)
            ]
            []
        ]
