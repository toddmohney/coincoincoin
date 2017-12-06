port module Ports
    exposing
        ( proposalAdded
        , proposalAddedTxAddressCreated
        , proposalExecuted
        , proposalExecutedTxAddressCreated
        , sessionLoaded
        , voted
        , votedTxAddressCreated
        , votingRulesUpdatedTxAddressCreated
        , votingRulesUpdated
        )

import Json.Encode exposing (Value)


port proposalAdded : (Value -> msg) -> Sub msg


port proposalAddedTxAddressCreated : (Value -> msg) -> Sub msg


port proposalExecuted : (Value -> msg) -> Sub msg


port proposalExecutedTxAddressCreated : (Value -> msg) -> Sub msg


port voted : (Value -> msg) -> Sub msg


port votedTxAddressCreated : (Value -> msg) -> Sub msg


port votingRulesUpdatedTxAddressCreated : (Value -> msg) -> Sub msg


port votingRulesUpdated : (Value -> msg) -> Sub msg


port sessionLoaded : (Value -> msg) -> Sub msg
