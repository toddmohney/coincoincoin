port module Ports
    exposing
        ( proposalAdded
        , proposalAddedTxHashCreated
        )

import Json.Encode exposing (Value)


port proposalAdded : (Value -> msg) -> Sub msg


port proposalAddedTxHashCreated : (Value -> msg) -> Sub msg
