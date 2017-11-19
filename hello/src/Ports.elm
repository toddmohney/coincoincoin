port module Ports
    exposing
        ( proposalAdded
        )

import Json.Encode exposing (Value)


port proposalAdded : (Value -> msg) -> Sub msg
