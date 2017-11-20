module Web3.Web3
    exposing
        ( AccountAddress(..)
        , Address(..)
        , BigNumber
        , Event
        , TxHash(..)
        , Tx
        , TxReceipt
        , accountDecoder
        , bigNumberDecoder
        , getAccountAddress
        , getAddress
        , getTxHash
        , mkAccountAddress
        , mkTxHash
        , sampleAccountAddress
        , sampleTxHash
        , txDecoder
        , txHashDecoder
        , txReceiptDecoder
        )

import Dict as Dict exposing (Dict)
import Json.Decode as Decode exposing (int, string, nullable, Decoder, Value)
import Json.Decode.Pipeline exposing (decode, required)


type alias BigNumber =
    String


bigNumberDecoder : Decoder BigNumber
bigNumberDecoder =
    Decode.string


type Address
    = Address String


getAddress : Address -> String
getAddress (Address addr) =
    addr


addressDecoder : Decoder Address
addressDecoder =
    Decode.map Address Decode.string


type AccountAddress
    = AccountAddress Address


mkAccountAddress : String -> AccountAddress
mkAccountAddress =
    AccountAddress << Address


getAccountAddress : AccountAddress -> String
getAccountAddress (AccountAddress addr) =
    getAddress addr


accountDecoder : Decoder AccountAddress
accountDecoder =
    Decode.map AccountAddress addressDecoder


type BlockAddress
    = BlockAddress Address


mkBlockhash : String -> BlockAddress
mkBlockhash =
    BlockAddress << Address


blockHashDecoder : Decoder BlockAddress
blockHashDecoder =
    Decode.map BlockAddress addressDecoder


type TxHash
    = TxHash Address


mkTxHash : String -> TxHash
mkTxHash =
    TxHash << Address


getTxHash : TxHash -> String
getTxHash (TxHash addr) =
    getAddress addr


txHashDecoder : Decoder TxHash
txHashDecoder =
    Decode.map TxHash addressDecoder


type alias Tx =
    { blockHash : BlockAddress
    , blockNumber : Maybe Int
    , from : AccountAddress
    , to : AccountAddress
    , gas : Int
    , gasPrice : String
    , transactionHash : TxHash
    , input : String
    , nonce : Int
    , r : String
    , s : String
    , v : String
    , transactionIndex : Int
    , value : String
    }


txDecoder : Decoder Tx
txDecoder =
    decode Tx
        |> required "blockHash" blockHashDecoder
        |> required "blockNumber" (nullable Decode.int)
        |> required "from" accountDecoder
        |> required "to" accountDecoder
        |> required "gas" Decode.int
        |> required "gasPrice" Decode.string
        |> required "hash" txHashDecoder
        |> required "input" Decode.string
        |> required "nonce" Decode.int
        |> required "r" Decode.string
        |> required "s" Decode.string
        |> required "v" Decode.string
        |> required "transactionIndex" Decode.int
        |> required "value" Decode.string


type alias TxReceipt =
    { blockHash : BlockAddress
    , blockNumber : Int
    , contractAddress : Maybe String
    , cumulativeGasUsed : Int
    , events : Dict String Event
    , from : AccountAddress
    , gasUsed : Int
    , logsBloom : String
    , root : BlockAddress
    , to : AccountAddress
    , transactionHash : TxHash
    , transactionIndex : Int
    }


type EventAddress
    = EventAddress Address


eventAddressDecoder : Decoder EventAddress
eventAddressDecoder =
    Decode.map EventAddress addressDecoder


type alias Event =
    { address : EventAddress
    , blockHash : BlockAddress
    , blockNumber : Int
    , event : String
    , id : String
    , logIndex : Int
    , removed : Bool
    , returnValues : Value
    , signature : String
    , transactionHash : TxHash
    , transactionIndex : Int
    }


eventDecoder : Decoder Event
eventDecoder =
    decode Event
        |> required "address" eventAddressDecoder
        |> required "blockHash" blockHashDecoder
        |> required "blockNumber" Decode.int
        |> required "event" Decode.string
        |> required "id" Decode.string
        |> required "logIndex" Decode.int
        |> required "removed" Decode.bool
        |> required "returnValues" (Decode.value)
        |> required "signature" Decode.string
        |> required "transactionHash" txHashDecoder
        |> required "transactionIndex" Decode.int


txReceiptDecoder : Decoder TxReceipt
txReceiptDecoder =
    decode TxReceipt
        |> required "blockHash" blockHashDecoder
        |> required "blockNumber" Decode.int
        |> required "contractAddress" (nullable string)
        |> required "cumulativeGasUsed" Decode.int
        |> required "events" (Decode.dict eventDecoder)
        |> required "from" accountDecoder
        |> required "gasUsed" Decode.int
        |> required "logsBloom" Decode.string
        |> required "root" blockHashDecoder
        |> required "to" accountDecoder
        |> required "transactionHash" txHashDecoder
        |> required "transactionIndex" Decode.int


sampleAccountAddress : AccountAddress
sampleAccountAddress =
    mkAccountAddress "0x0000000000000000000000000000000000000000"


sampleTxHash : TxHash
sampleTxHash =
    mkTxHash "0x0000000000000000000000000000000000000000000000000000000000000000"
