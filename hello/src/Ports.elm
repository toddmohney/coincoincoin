port module Ports
    exposing
        ( proposalAdded
        , proposalAddedTxHashCreated
        , voted
        , votedTxHashCreated
        )

import Json.Encode exposing (Value)


port proposalAdded : (Value -> msg) -> Sub msg


port proposalAddedTxHashCreated : (Value -> msg) -> Sub msg


port voted : (Value -> msg) -> Sub msg


port votedTxHashCreated : (Value -> msg) -> Sub msg
