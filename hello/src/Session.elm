module Session
    exposing
        ( Session
        )

import Web3.Web3 as Web3 exposing (AccountAddress)


type alias Session =
    { accountAddress : AccountAddress
    }
