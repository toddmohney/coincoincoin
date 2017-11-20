module Blockocracy.Events
    exposing
        ( BlockchainEvent(..)
        , Context(..)
        , bannerMessage
        )

import Blockocracy.Proposals.Events as PE
import Blockocracy.Votes.Events as VE
import Html exposing (..)
import Html.Attributes exposing (..)
import Web3.Web3 exposing (Address(..), TxHash(..), TxReceipt)


type Context
    = Proposal
    | Vote


type BlockchainEvent
    = TxHashCreated Context (Result String TxHash)
    | TxReceiptReceived Context (Result String TxReceipt)


bannerMessage : BlockchainEvent -> Html msg
bannerMessage bcEvt =
    case bcEvt of
        TxHashCreated ctx res ->
            case ctx of
                Proposal ->
                    txHashCreatedMessage "New proposal tx received " res

                Vote ->
                    txHashCreatedMessage "Vote tx received " res

        TxReceiptReceived ctx res ->
            case ctx of
                Proposal ->
                    txReceiptMessage PE.parseProposalAddedEvent res

                Vote ->
                    txReceiptMessage VE.parseVotedEvent res


txHashCreatedMessage : String -> Result String TxHash -> Html msg
txHashCreatedMessage intro res =
    case res of
        Err err ->
            div [] [ text err ]

        Ok (TxHash (Address txHash)) ->
            div
                []
                [ a
                    [ target "_blank"
                    , href <| "http://localhost:8000/#/transaction/" ++ txHash
                    ]
                    [ text <| intro ++ txHash
                    ]
                , div
                    []
                    [ text " Waiting for the tx to be mined."
                    ]
                ]


txReceiptMessage : (TxReceipt -> Result String a) -> Result String TxReceipt -> Html msg
txReceiptMessage eventParserFn res =
    case res of
        Err err ->
            div [] [ text err ]

        Ok txReceipt ->
            case eventParserFn txReceipt of
                Err err ->
                    div [] [ text err ]

                Ok evt ->
                    div []
                        [ a
                            [ target "_blank"
                            , href <| "http://localhost:8000/#/block/" ++ toString txReceipt.blockNumber
                            ]
                            [ text <| "Tx mined in block " ++ toString txReceipt.blockNumber
                            ]
                        , div
                            []
                            [ text "Tx details"
                            , renderTx txReceipt evt
                            ]
                        ]


renderTx : TxReceipt -> a -> Html msg
renderTx txReceipt evt =
    pre
        []
        [ text <|
            "Block number"
                ++ toString txReceipt.blockNumber
                ++ "\n"
                ++ "Gas used: "
                ++ toString txReceipt.cumulativeGasUsed
                ++ "\n"
                ++ "Event: "
                ++ toString evt
        ]
