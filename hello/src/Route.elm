module Route exposing (Route(..), BlockocracySubRoute(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing (Parser, (</>), oneOf, parseHash, s)


-- ROUTING --


type Route
    = Home
    | Blockocracy BlockocracySubRoute
    | NodeDiagnostics


type BlockocracySubRoute
    = Vote
    | Propose
    | Admin


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map (Blockocracy Vote) (s "blockocracy" </> s "vote")
        , Url.map (Blockocracy Propose) (s "blockocracy" </> s "propose")
        , Url.map (Blockocracy Admin) (s "blockocracy" </> s "admin")
        , Url.map NodeDiagnostics (s "node")
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    [ "" ]

                Blockocracy Vote ->
                    [ "blockocracy", "vote" ]

                Blockocracy Propose ->
                    [ "blockocracy", "propose" ]

                Blockocracy Admin ->
                    [ "blockocracy", "admin" ]

                NodeDiagnostics ->
                    [ "node" ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
