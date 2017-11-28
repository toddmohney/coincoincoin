module Blockocracy.Votes.Events
    exposing
        ( VoteEvent
        , VotedEvent
        , parseVotedEvent
        , parseChangeOfRulesEvent
        )

import Dict as Dict
import Json.Decode as D exposing (int, string, nullable, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Monocle.Optional exposing (..)
import Result
import Web3.Web3 as Web3 exposing (AccountAddress(..), BigNumber, Event, TxReceipt)


type VoteEvent
    = Voted
    | ChangeOfRules


type alias VotedEvent =
    { proposalID : String
    , position : Bool
    , voter : AccountAddress
    , justification : String
    }


type alias VotingRulesChangedEvent =
    { newMinimumQuorum : Int
    , newDebatingPeriodInMinutes : Int
    , newMajorityMargin : Int
    }


parseVotedEvent : TxReceipt -> Result String VotedEvent
parseVotedEvent txReceipt =
    case votedEventOpt.getOption txReceipt of
        Nothing ->
            Err <| "Event not found: " ++ toString Voted

        Just evt ->
            D.decodeValue votedEventDecoder evt.returnValues


parseChangeOfRulesEvent : TxReceipt -> Result String VotingRulesChangedEvent
parseChangeOfRulesEvent txReceipt =
    case votingRulesChangedEventOpt.getOption txReceipt of
        Nothing ->
            Err <| "Event not found: " ++ toString ChangeOfRules

        Just evt ->
            D.decodeValue votingRulesChangedEventDecoder evt.returnValues


votedEventDecoder : Decoder VotedEvent
votedEventDecoder =
    decode VotedEvent
        |> required "proposalID" D.string
        |> required "position" (D.map (Maybe.withDefault False) <| nullable D.bool)
        |> required "voter" Web3.accountDecoder
        |> required "justification" D.string


votedEventOpt : Optional TxReceipt Event
votedEventOpt =
    Optional votedEventGet votedEventSet


votedEventGet : TxReceipt -> Maybe Event
votedEventGet txReceipt =
    Dict.get (toString Voted) txReceipt.events


votedEventSet : Event -> TxReceipt -> TxReceipt
votedEventSet evt txReceipt =
    { txReceipt
        | events = Dict.insert (toString Voted) evt txReceipt.events
    }


votingRulesChangedEventDecoder : Decoder VotingRulesChangedEvent
votingRulesChangedEventDecoder =
    decode VotingRulesChangedEvent
        |> required "newMinimumQuorum" (D.string |> D.andThen toIntDecoder)
        |> required "newDebatingPeriodInMinutes" (D.string |> D.andThen toIntDecoder)
        |> required "newMajorityMargin" (D.string |> D.andThen toIntDecoder)


toIntDecoder : String -> Decoder Int
toIntDecoder str =
    case String.toInt str of
        Err err ->
            D.fail err

        Ok a ->
            D.succeed a


votingRulesChangedEventOpt : Optional TxReceipt Event
votingRulesChangedEventOpt =
    Optional votingRulesChangedEventGet votingRulesChangedEventSet


votingRulesChangedEventGet : TxReceipt -> Maybe Event
votingRulesChangedEventGet txReceipt =
    Dict.get (toString ChangeOfRules) txReceipt.events


votingRulesChangedEventSet : Event -> TxReceipt -> TxReceipt
votingRulesChangedEventSet evt txReceipt =
    { txReceipt
        | events = Dict.insert (toString ChangeOfRules) evt txReceipt.events
    }
