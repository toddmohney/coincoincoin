module NodeDiagnostics.Pages.Overview exposing (Page, Msg(..), init, update, view)

import Async exposing (External(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Errors.Pages.Errored as Errored exposing (PageLoadError, pageLoadError)
import NodeDiagnostics.NodeDiagnostics exposing (NodeDiagnostics)
import Session exposing (Session)
import Task exposing (Task)
import Web3.Web3 as Web3


type alias Page =
    { session : Session
    , nodeDiagnostics : External NodeDiagnostics
    }


type Msg
    = DiagnosticsLoaded (Result String NodeDiagnostics)


init : Session -> Task PageLoadError Page
init session =
    Task.succeed <|
        Page session (NotLoaded)


update : Msg -> Page -> ( Page, Cmd Msg )
update msg model =
    case msg of
        DiagnosticsLoaded (Err err) ->
            ( { model | nodeDiagnostics = LoadError err }
            , Cmd.none
            )

        DiagnosticsLoaded (Ok nodeDiagnostics) ->
            ( { model | nodeDiagnostics = Loaded nodeDiagnostics }
            , Cmd.none
            )


view : Page -> Html Msg
view model =
    div
        [ class "container" ]
        [ h2 [] [ text "Node Diagnostics" ]
        , renderNodeDiagnostics model.nodeDiagnostics
        ]


renderNodeDiagnostics : External NodeDiagnostics -> Html Msg
renderNodeDiagnostics nodeDiagnostics =
    case nodeDiagnostics of
        NotLoaded ->
            div [] [ text "Loading diagnostics..." ]

        Loading ->
            div [] [ text "Loading diagnostics..." ]

        LoadError err ->
            div [] [ text <| "Error loading diagnostics: " ++ err ]

        Loaded diagnostics ->
            ul
                [ class "list-group" ]
                [ li [ class "list-group-item" ] [ text <| "Coinbase: " ++ Web3.getAccountAddress diagnostics.coinbase ]
                , li [ class "list-group-item" ] [ text <| "Mining: " ++ toString diagnostics.isMining ]
                , li [ class "list-group-item" ] [ text <| "Hashrate: " ++ toString diagnostics.hashrate ]
                , li [ class "list-group-item" ] [ text <| "Gas price: " ++ toString diagnostics.gasPrice ]
                , li [ class "list-group-item" ] [ text <| "Syncing: " ++ toString diagnostics.isSyncing ]
                , li [ class "list-group-item" ] [ text <| "Block number: " ++ toString diagnostics.blockNumber ]
                , li [ class "list-group-item" ] [ text <| "Accounts: " ++ toString diagnostics.accounts ]
                ]
