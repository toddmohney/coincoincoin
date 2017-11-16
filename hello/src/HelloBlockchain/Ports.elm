port module HelloBlockchain.Ports
    exposing
        ( SayHelloRequest
        , getHelloCount
        , getTx
        , helloCountReceived
        , helloTxConfirmed
        , helloTxError
        , helloTxMined
        , helloTxReceiptReceived
        , helloTxReceived
        , sayHello
        , txReceived
        )

import Json.Encode exposing (Value)


type alias SayHelloRequest =
    { address : String
    , gasPrice : Int
    }


port getTx : String -> Cmd msg


port getHelloCount : String -> Cmd msg


port sayHello : SayHelloRequest -> Cmd msg


port txReceived : (Value -> msg) -> Sub msg


port helloCountReceived : (Int -> msg) -> Sub msg


port helloTxReceived : (String -> msg) -> Sub msg


port helloTxReceiptReceived : (Value -> msg) -> Sub msg


port helloTxConfirmed : (Int -> msg) -> Sub msg


port helloTxMined : (Value -> msg) -> Sub msg


port helloTxError : (Value -> msg) -> Sub msg
