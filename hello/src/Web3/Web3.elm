module Web3.Web3
    exposing
        ( AccountAddress(..)
        , Address(..)
        , BigNumber
        , Event
        , TxAddress(..)
        , Tx
        , TxReceipt
        , accountDecoder
        , addressDecoder
        , bigNumberDecoder
        , getAccountAddress
        , getAddress
        , getTxAddress
        , mkAccountAddress
        , mkTxAddress
        , sampleAccountAddress
        , sampleTxAddress
        , txDecoder
        , txAddressDecoder
        , txReceiptDecoder
        )

import Dict as Dict exposing (Dict)
import Json.Decode as Decode exposing (int, string, nullable, Decoder, Value)
import Json.Decode.Pipeline exposing (decode, required)


type alias BigNumber =
    String


type Address
    = Address String


type AccountAddress
    = AccountAddress Address


type BlockAddress
    = BlockAddress Address


type EventAddress
    = EventAddress Address


type TxAddress
    = TxAddress Address


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
    , transactionHash : TxAddress
    , transactionIndex : Int
    }


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
    , transactionHash : TxAddress
    , transactionIndex : Int
    }


type alias Tx =
    { blockHash : BlockAddress
    , blockNumber : Maybe Int
    , from : AccountAddress
    , to : AccountAddress
    , gas : Int
    , gasPrice : String
    , transactionHash : TxAddress
    , input : String
    , nonce : Int
    , r : String
    , s : String
    , v : String
    , transactionIndex : Int
    , value : String
    }


bigNumberDecoder : Decoder BigNumber
bigNumberDecoder =
    Decode.string


getAddress : Address -> String
getAddress (Address addr) =
    addr


addressDecoder : Decoder Address
addressDecoder =
    Decode.map Address Decode.string


mkAccountAddress : String -> AccountAddress
mkAccountAddress =
    AccountAddress << Address


getAccountAddress : AccountAddress -> String
getAccountAddress (AccountAddress addr) =
    getAddress addr


accountDecoder : Decoder AccountAddress
accountDecoder =
    Decode.map AccountAddress addressDecoder


mkBlockhash : String -> BlockAddress
mkBlockhash =
    BlockAddress << Address


blockHashDecoder : Decoder BlockAddress
blockHashDecoder =
    Decode.map BlockAddress addressDecoder


mkTxAddress : String -> TxAddress
mkTxAddress =
    TxAddress << Address


getTxAddress : TxAddress -> String
getTxAddress (TxAddress addr) =
    getAddress addr


txAddressDecoder : Decoder TxAddress
txAddressDecoder =
    Decode.map TxAddress addressDecoder


txDecoder : Decoder Tx
txDecoder =
    decode Tx
        |> required "blockHash" blockHashDecoder
        |> required "blockNumber" (nullable Decode.int)
        |> required "from" accountDecoder
        |> required "to" accountDecoder
        |> required "gas" Decode.int
        |> required "gasPrice" Decode.string
        |> required "hash" txAddressDecoder
        |> required "input" Decode.string
        |> required "nonce" Decode.int
        |> required "r" Decode.string
        |> required "s" Decode.string
        |> required "v" Decode.string
        |> required "transactionIndex" Decode.int
        |> required "value" Decode.string


eventAddressDecoder : Decoder EventAddress
eventAddressDecoder =
    Decode.map EventAddress addressDecoder


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
        |> required "transactionHash" txAddressDecoder
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
        |> required "transactionHash" txAddressDecoder
        |> required "transactionIndex" Decode.int


sampleAccountAddress : AccountAddress
sampleAccountAddress =
    mkAccountAddress "0x0000000000000000000000000000000000000000"


sampleTxAddress : TxAddress
sampleTxAddress =
    mkTxAddress "0x0000000000000000000000000000000000000000000000000000000000000000"
