module Blockocracy.Views.Page
    exposing
        ( ActivePage(..)
        , frame
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)


type ActivePage
    = Index
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
            [ classList [ ( "active", page == Index ) ] ]
            [ a
                [ Route.href Route.Blockocracy ]
                [ text "Index" ]
            ]
        , li
            [ classList [ ( "active", page == Admin ) ] ]
            [ a
                [ Route.href Route.BlockocracyAdminMembers ]
                [ text "Admin" ]
            ]
        ]
