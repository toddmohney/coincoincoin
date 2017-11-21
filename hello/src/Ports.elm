port module Ports
    exposing
        ( proposalAdded
        , proposalAddedTxAddressCreated
        , voted
        , votedTxAddressCreated
        )

import Json.Encode exposing (Value)


port proposalAdded : (Value -> msg) -> Sub msg


port proposalAddedTxAddressCreated : (Value -> msg) -> Sub msg


port voted : (Value -> msg) -> Sub msg


port votedTxAddressCreated : (Value -> msg) -> Sub msg
