module Blockocracy.Proposals.Events
    exposing
        ( ProposalEvent
        , ProposalAddedEvent
        , parseProposalAddedEvent
        )

import Dict as Dict
import Json.Decode as D exposing (int, string, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Monocle.Optional exposing (..)
import Result
import Web3.Web3 as Web3 exposing (AccountAddress(..), BigNumber, Event, TxReceipt)


type ProposalEvent
    = ProposalAdded


type alias ProposalAddedEvent =
    { proposalID : String
    , recipient : AccountAddress
    , amount : BigNumber
    , description : String
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
