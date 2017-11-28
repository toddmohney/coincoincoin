port module Ports
    exposing
        ( proposalAdded
        , proposalAddedTxAddressCreated
        , proposalExecuted
        , proposalExecutedTxHashCreated
        , voted
        , votedTxAddressCreated
        , votingRulesUpdatedTxHashCreated
        , votingRulesUpdated
        )

import Json.Encode exposing (Value)


port proposalAdded : (Value -> msg) -> Sub msg


port proposalAddedTxAddressCreated : (Value -> msg) -> Sub msg


port proposalExecuted : (Value -> msg) -> Sub msg


port proposalExecutedTxHashCreated : (Value -> msg) -> Sub msg


port voted : (Value -> msg) -> Sub msg


port votedTxAddressCreated : (Value -> msg) -> Sub msg


port votingRulesUpdatedTxHashCreated : (Value -> msg) -> Sub msg


port votingRulesUpdated : (Value -> msg) -> Sub msg
