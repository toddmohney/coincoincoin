module Web3.Web3Test exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Decode as Decode
import Web3.Web3 as Web3


suite : Test
suite =
    describe "Web3.Web3"
        [ describe "txDecoder"
            [ test "decodes a tx paylaod" <|
                \_ ->
                    let
                        decoded =
                            Decode.decodeString Web3.txDecoder txJSON
                    in
                        case decoded of
                            Err err ->
                                Debug.log err <| Expect.equal False True

                            Ok _ ->
                                Expect.equal True True
            ]
        ]


txJSON : String
txJSON =
    """
        { "blockHash":"0x0000000000000000000000000000000000000000000000000000000000000000",
            "blockNumber":null,
            "from":"0x6F466bb3540e96436298c8cb8fB2F07c515F8068",
            "gas":40116,
            "gasPrice":"20000000000",
            "hash":"0xf8a12b6ac2c609d383a18631306d572622d04ee5651e3183114f73c6df42ebf2",
            "input":"0xef5fb05b",
            "nonce":33,
            "to":"0x32C9e197951A3674AAB69A5Df245A27069fB6e5d",
            "transactionIndex":0,
            "value":"0",
            "v":"0x41",
            "r":"0x3186e35a9d334ee2632abaf2f1a2479b0676b1f6944f3af6ce8329ef76621f78",
            "s":"0x7e8a0dc8a44ea3f4de0d14de367345b5c359cb9d4380e30799b2451284b651b7" }
    """
