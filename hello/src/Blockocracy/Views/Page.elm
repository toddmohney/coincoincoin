module Blockocracy.Views.Page
    exposing
        ( ActivePage(..)
        , frame
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Route as R exposing (Route)


type ActivePage
    = Vote
    | Propose
    | Admin


frame : ActivePage -> Html msg -> Html msg
frame page content =
    div [ class "container" ]
        [ viewHeader page
        , content
        ]


viewHeader : ActivePage -> Html msg
viewHeader page =
    ul
        [ classList [ ( "nav", True ), ( "nav-pills", True ) ] ]
        [ li
            [ classList [ ( "active", page == Vote ) ] ]
            [ a
                [ R.href (R.Blockocracy R.Vote) ]
                [ text "Vote" ]
            ]
        , li
            [ classList [ ( "active", page == Propose ) ] ]
            [ a
                [ R.href (R.Blockocracy R.Propose) ]
                [ text "Propose" ]
            ]
        , li
            [ classList [ ( "active", page == Admin ) ] ]
            [ a
                [ R.href (R.Blockocracy R.Admin) ]
                [ text "Admin" ]
            ]
        ]
