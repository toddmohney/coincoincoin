module Main exposing (main)

import Blockocracy.Pages.Index as Blockocracy
import Blockocracy.Admin.Pages.Members as BlockocracyAdmin
import Blockocracy.Views.Page as BVP
import Errors.Pages.Errored as Errored exposing (PageLoadError)
import Errors.Pages.NotFound as NotFound
import HelloBlockchain.Pages.Index as HBC
import HelloBlockchain.Ports as Ports
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Route exposing (Route)
import Task
import Util exposing ((=>))
import Views.Page as Page exposing (ActivePage)
import Web3.Web3 as Web3


-- WARNING: Based on discussions around how asset management features
-- like code splitting and lazy loading have been shaping up, I expect
-- most of this file to become unnecessary in a future release of Elm.
-- Avoid putting things in here unless there is no alternative!


type Page
    = Blank
    | NotFound
    | Errored PageLoadError
    | Home HBC.HelloBlockchainPage
    | Blockocracy Blockocracy.Page
    | BlockocracyAdminMembers BlockocracyAdmin.Page


type BlockocracyPage
    = Index
    | AdminMembers


type PageState
    = Loaded Page
    | TransitioningFrom Page



-- MODEL --


type alias Model =
    { pageState : PageState
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        }


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage False page

        TransitioningFrom page ->
            viewPage True page


viewPage : Bool -> Page -> Html Msg
viewPage isLoading page =
    let
        frame =
            Page.frame isLoading
    in
        case page of
            NotFound ->
                NotFound.view
                    |> frame Page.Other

            Blank ->
                -- This is for the very initial page load, while we are loading
                -- data via HTTP. We could also render a spinner here.
                Html.text ""
                    |> frame Page.Other

            Errored subModel ->
                Errored.view subModel
                    |> frame Page.Other

            Home subModel ->
                HBC.view subModel
                    |> frame Page.Home
                    |> Html.map HomeMsg

            Blockocracy subModel ->
                Blockocracy.view subModel
                    |> BVP.frame BVP.Index
                    |> frame Page.Blockocracy
                    |> Html.map BlockocracyMsg

            BlockocracyAdminMembers subModel ->
                BlockocracyAdmin.view subModel
                    |> BVP.frame BVP.Admin
                    |> frame Page.Blockocracy
                    |> Html.map BlockocracyAdminMembersMsg



-- UPDATE --


type Msg
    = BlockocracyLoaded (Result PageLoadError Blockocracy.Page)
    | BlockocracyAdminMembersLoaded (Result PageLoadError BlockocracyAdmin.Page)
    | BlockocracyMsg Blockocracy.Msg
    | BlockocracyAdminMembersMsg BlockocracyAdmin.Msg
    | HomeLoaded (Result PageLoadError HBC.HelloBlockchainPage)
    | HomeMsg HBC.Msg
    | SetRoute (Maybe Route)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Task.attempt toMsg task

        errored =
            pageErrored model
    in
        case maybeRoute of
            Nothing ->
                { model | pageState = Loaded NotFound } => Cmd.none

            Just Route.Home ->
                transition HomeLoaded HBC.init

            Just Route.Blockocracy ->
                transition BlockocracyLoaded Blockocracy.init

            Just Route.BlockocracyAdminMembers ->
                transition BlockocracyAdminMembersLoaded BlockocracyAdmin.init


pageErrored : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageErrored model activePage errorMessage =
    let
        error =
            Errored.pageLoadError activePage errorMessage
    in
        { model | pageState = Loaded (Errored error) } => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
                ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        errored =
            pageErrored model
    in
        case ( msg, page ) of
            ( SetRoute route, _ ) ->
                setRoute route model

            ( HomeLoaded (Ok subModel), _ ) ->
                { model | pageState = Loaded (Home subModel) } => Cmd.none

            ( HomeLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( HomeMsg subMsg, Home subModel ) ->
                toPage Home HomeMsg HBC.update subMsg subModel

            ( BlockocracyLoaded (Ok subModel), _ ) ->
                { model | pageState = Loaded (Blockocracy subModel) } => Cmd.none

            ( BlockocracyLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( BlockocracyMsg subMsg, Blockocracy subModel ) ->
                toPage Blockocracy BlockocracyMsg Blockocracy.update subMsg subModel

            ( BlockocracyAdminMembersLoaded (Ok subModel), _ ) ->
                { model | pageState = Loaded (BlockocracyAdminMembers subModel) } => Cmd.none

            ( BlockocracyAdminMembersLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( BlockocracyAdminMembersMsg subMsg, BlockocracyAdminMembers subModel ) ->
                toPage BlockocracyAdminMembers BlockocracyAdminMembersMsg BlockocracyAdmin.update subMsg subModel

            ( _, NotFound ) ->
                -- Disregard incoming messages when we're on the
                -- NotFound page.
                model => Cmd.none

            ( _, _ ) ->
                -- Disregard incoming messages that arrived for the wrong page
                model => Cmd.none



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ pageSubscriptions (getPage model.pageState)
        ]


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Errored _ ->
            Sub.none

        NotFound ->
            Sub.none

        Home _ ->
            Sub.batch
                [ Ports.helloCountReceived (HomeMsg << HBC.HelloCountReceived)
                , Ports.helloTxReceived (HomeMsg << HBC.HelloTxReceived)
                , Ports.helloTxReceiptReceived (HomeMsg << HBC.HelloTxReceiptReceived << Decode.decodeValue Web3.txReceiptDecoder)
                , Ports.helloTxMined (HomeMsg << HBC.HelloTxReceiptReceived << Decode.decodeValue Web3.txReceiptDecoder)
                , Ports.helloTxConfirmed (HomeMsg << HBC.HelloTxConfirmed)
                , Ports.helloTxError (HomeMsg << HBC.HelloTxError)
                , Ports.txReceived (HomeMsg << HBC.TxReceived << Decode.decodeValue Web3.txDecoder)
                ]

        Blockocracy _ ->
            Sub.none

        BlockocracyAdminMembers _ ->
            Sub.none



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
