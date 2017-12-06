module Views.Page exposing (ActivePage(..), bodyId, frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode
import Route as R exposing (Route)
import Session exposing (Session)
import Web3.Web3 as Web3


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type ActivePage
    = Other
    | Home
    | Blockocracy


{-| Take a page's Html and frame it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
frame : Maybe Session -> Bool -> Html msg -> ActivePage -> Html msg -> Html msg
frame session isLoading bannerMsg page content =
    div []
        [ viewHeader session page isLoading
        , banner bannerMsg
        , content
        ]


banner : Html msg -> Html msg
banner bannerMsg =
    div
        [ class "container" ]
        [ div
            [ classList
                [ ( "alert", True )
                , ( "alert-info", True )
                , ( "alert-dismissible", True )
                ]
            ]
            [ button
                [ type_ "button"
                , class "close"
                , property "data-dismiss" (Encode.string "close")
                ]
                []
            , bannerMsg
            ]
        ]


viewHeader : Maybe Session -> ActivePage -> Bool -> Html msg
viewHeader session page isLoading =
    nav
        [ classList [ ( "navbar", True ), ( "navbar-inverse", True ) ] ]
        [ div
            [ class "container" ]
            [ div
                [ class "navbar-header" ]
                [ a
                    [ class "navbar-brand", R.href R.Home ]
                    [ text "Hello, blockchain!" ]
                ]
            , div
                [ classList [ ( "collapse", True ), ( "navbar-collapse", True ) ] ]
                [ ul
                    [ classList [ ( "nav", True ), ( "navbar-nav", True ) ] ]
                    [ li
                        [ classList [ ( "active", page == Blockocracy ) ] ]
                        [ a [ R.href (R.Blockocracy R.Vote) ] [ text "Blockocracy" ] ]
                    ]
                , ul
                    [ classList [ ( "nav", True ), ( "navbar-nav", True ), ( "navbar-right", True ) ] ]
                    [ li
                        []
                        [ sessionName session ]
                    ]
                ]
            ]
        ]


sessionName : Maybe Session -> Html msg
sessionName mSession =
    case mSession of
        Nothing ->
            a [ R.href (R.Blockocracy R.Vote) ] [ text "You are not signed in" ]

        Just session ->
            a [ R.href (R.Blockocracy R.Vote) ] [ text <| Web3.getAccountAddress session.accountAddress ]


{-| This id comes from index.html.

The Feed uses it to scroll to the top of the page (by ID) when switching pages
in the pagination sense.

-}
bodyId : String
bodyId =
    "page-body"
