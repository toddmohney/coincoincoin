port module Ports exposing
    ( getHelloCount
    , helloCountReceived
    , sayHello
    , helloTxReceived
    , helloTxReceiptReceived
    , helloTxConfirmed
    , helloTxMined
    , helloTxError
    )

import Json.Encode exposing (Value)

port getHelloCount : String -> Cmd msg
port helloCountReceived : (Int -> msg) -> Sub msg

port sayHello : String -> Cmd msg
port helloTxReceived : (String -> msg) -> Sub msg
port helloTxReceiptReceived : (Value -> msg) -> Sub msg
port helloTxConfirmed : (Int -> msg) -> Sub msg
port helloTxMined : (Value -> msg) -> Sub msg
port helloTxError : (Value -> msg) -> Sub msg
