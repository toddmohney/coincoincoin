module NodeDiagnostics.NodeDiagnostics
    exposing
        ( NodeDiagnostics
        , nodeDiagnosticsDecoder
        )

import Json.Decode as Decode exposing (bool, int, string, nullable, Decoder, Value)
import Json.Decode.Pipeline exposing (decode, required)
import Web3.Web3 as Web3 exposing (AccountAddress, BigNumber)


type alias NodeDiagnostics =
    { coinbase : AccountAddress
    , isMining : Bool
    , hashrate : Int
    , gasPrice : BigNumber
    , isSyncing : Bool
    , blockNumber : Int
    , accounts : List AccountAddress
    }


nodeDiagnosticsDecoder : Decoder NodeDiagnostics
nodeDiagnosticsDecoder =
    decode NodeDiagnostics
        |> required "coinbase" Web3.accountDecoder
        |> required "isMining" bool
        |> required "hashrate" int
        |> required "gasPrice" Web3.bigNumberDecoder
        |> required "isSyncing" bool
        |> required "blockNumber" int
        |> required "accounts" (Decode.list Web3.accountDecoder)
