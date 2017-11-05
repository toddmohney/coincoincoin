module Web3.Web3 exposing
    ( AccountAddress(..)
    , Address(..)
    , Tx
    , txDecoder
    , getAddress
    , getAccountAddress
    , mkAccountAddress
    , sampleAccountAddress
    )

import Json.Decode as Decode exposing (int, string, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required)

type Address = Address String

getAddress : Address -> String
getAddress (Address addr) = addr

addressDecoder : Decoder Address
addressDecoder = Decode.map Address Decode.string

type AccountAddress = AccountAddress Address

mkAccountAddress : String -> AccountAddress
mkAccountAddress = AccountAddress << Address

getAccountAddress : AccountAddress -> String
getAccountAddress (AccountAddress addr) = getAddress addr

accountDecoder : Decoder AccountAddress
accountDecoder = Decode.map AccountAddress addressDecoder

type BlockAddress = BlockAddress Address

mkBlockhash : String -> BlockAddress
mkBlockhash = BlockAddress << Address

blockHashDecoder : Decoder BlockAddress
blockHashDecoder = Decode.map BlockAddress addressDecoder

type TxHash = TxHash Address

mkTxhash : String -> TxHash
mkTxhash = TxHash << Address

txHashDecoder : Decoder TxHash
txHashDecoder = Decode.map TxHash addressDecoder

type alias Tx =
    { blockHash         : BlockAddress
    , blockNumber       : Int
    , contractAddress   : Maybe String
    , cumulativeGasUsed : Int
    , from              : AccountAddress
    , gasUsed           : Int
    , logsBloom         : String
    , root              : BlockAddress
    , to                : AccountAddress
    , transactionHash   : TxHash
    , transactionIndex  : Int
    }

txDecoder : Decoder Tx
txDecoder =
    decode Tx
        |> required "blockHash" blockHashDecoder
        |> required "blockNumber" Decode.int
        |> required "contractAddress" (nullable string)
        |> required "cumulativeGasUsed" Decode.int
        |> required "from" accountDecoder
        |> required "gasUsed" Decode.int
        |> required "logsBloom" Decode.string
        |> required "root" blockHashDecoder
        |> required "to" accountDecoder
        |> required "transactionHash" txHashDecoder
        |> required "transactionIndex" Decode.int

sampleAccountAddress : AccountAddress
sampleAccountAddress = mkAccountAddress "0x0000000000000000000000000000000000000000"
