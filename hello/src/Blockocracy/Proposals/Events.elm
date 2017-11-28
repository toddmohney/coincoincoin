module Blockocracy.Proposals.Events
    exposing
        ( ProposalEvent
        , ProposalAddedEvent
        , parseProposalAddedEvent
        , parseProposalExecutedEvent
        )

import Dict as Dict
import Json.Decode as D exposing (int, string, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Monocle.Optional exposing (..)
import Result
import Web3.Web3 as Web3 exposing (AccountAddress(..), BigNumber, Event, TxReceipt)


type ProposalEvent
    = ProposalAdded
    | ProposalTallied


type alias ProposalAddedEvent =
    { proposalID : String
    , recipient : AccountAddress
    , amount : BigNumber
    , description : String
    }


type alias ProposalExecutedEvent =
    { proposalID : Int
    , result : Int
    , quorum : Int
    , active : Bool
    }


parseProposalAddedEvent : TxReceipt -> Result String ProposalAddedEvent
parseProposalAddedEvent txReceipt =
    case proposalAddedEventOpt.getOption txReceipt of
        Nothing ->
            Err <| "Event not found: " ++ toString ProposalAdded

        Just evt ->
            D.decodeValue proposalAddedEventDecoder evt.returnValues


proposalAddedEventDecoder : Decoder ProposalAddedEvent
proposalAddedEventDecoder =
    decode ProposalAddedEvent
        |> required "proposalID" D.string
        |> required "recipient" Web3.accountDecoder
        |> required "amount" Web3.bigNumberDecoder
        |> required "description" D.string


proposalAddedEventOpt : Optional TxReceipt Event
proposalAddedEventOpt =
    Optional proposalAddedEventGet proposalAddedEventSet


proposalAddedEventGet : TxReceipt -> Maybe Event
proposalAddedEventGet txReceipt =
    Dict.get (toString ProposalAdded) txReceipt.events


proposalAddedEventSet : Event -> TxReceipt -> TxReceipt
proposalAddedEventSet evt txReceipt =
    { txReceipt
        | events = Dict.insert (toString ProposalAdded) evt txReceipt.events
    }


parseProposalExecutedEvent : TxReceipt -> Result String ProposalExecutedEvent
parseProposalExecutedEvent txReceipt =
    case proposalExecutedEventOpt.getOption txReceipt of
        Nothing ->
            Err <| "Event not found: " ++ toString ProposalTallied

        Just evt ->
            D.decodeValue proposalExecutedEventDecoder evt.returnValues


proposalExecutedEventDecoder : Decoder ProposalExecutedEvent
proposalExecutedEventDecoder =
    decode ProposalExecutedEvent
        |> required "proposalID" (D.string |> D.andThen toIntDecoder)
        |> required "result" (D.string |> D.andThen toIntDecoder)
        |> required "quorum" (D.string |> D.andThen toIntDecoder)
        |> required "active" (nullable D.bool |> D.andThen toBoolDecoder)


toIntDecoder : String -> Decoder Int
toIntDecoder str =
    case String.toInt str of
        Err err ->
            D.fail err

        Ok a ->
            D.succeed a


toBoolDecoder : Maybe Bool -> Decoder Bool
toBoolDecoder bool =
    case bool of
        Nothing ->
            D.succeed False

        Just b ->
            D.succeed b


proposalExecutedEventOpt : Optional TxReceipt Event
proposalExecutedEventOpt =
    Optional proposalExecutedEventGet proposalExecutedEventSet


proposalExecutedEventGet : TxReceipt -> Maybe Event
proposalExecutedEventGet txReceipt =
    Dict.get (toString ProposalTallied) txReceipt.events


proposalExecutedEventSet : Event -> TxReceipt -> TxReceipt
proposalExecutedEventSet evt txReceipt =
    { txReceipt
        | events = Dict.insert (toString ProposalTallied) evt txReceipt.events
    }
