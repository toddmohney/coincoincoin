module Main exposing (main)

import Blockocracy.Admin.Pages.Members as BlockocracyAdmin
import Blockocracy.Events as BE
import Blockocracy.Pages.Propose as BP
import Blockocracy.Pages.Vote as BV
import Blockocracy.Ports as BlockPorts
import Blockocracy.Proposal as Proposal
import Blockocracy.Views.Page as BVP
import Blockocracy.Vote as Vote
import Errors.Pages.Errored as Errored exposing (PageLoadError)
import Errors.Pages.NotFound as NotFound
import Html exposing (..)
import Json.Decode as Decode exposing (Value)
import Navigation exposing (Location)
import Ports as Ports
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
    | BlockocracyVote BV.Page
    | BlockocracyPropose BP.Page
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
    , bannerMessage : Html Msg
    }


init : Value -> Location -> ( Model, Cmd Msg )
init val location =
    setRoute (Route.fromLocation location)
        { pageState = Loaded initialPage
        , bannerMessage = div [] []
        }


initialPage : Page
initialPage =
    Blank



-- VIEW --


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage False page model.bannerMessage

        TransitioningFrom page ->
            viewPage True page model.bannerMessage


viewPage : Bool -> Page -> Html Msg -> Html Msg
viewPage isLoading page bannerMsg =
    let
        frame =
            Page.frame isLoading bannerMsg
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

            BlockocracyVote subModel ->
                BV.view subModel
                    |> BVP.frame BVP.Vote
                    |> Html.map BlockocracyVoteMsg
                    |> frame Page.Blockocracy

            BlockocracyPropose subModel ->
                BP.view subModel
                    |> BVP.frame BVP.Propose
                    |> Html.map BlockocracyProposeMsg
                    |> frame Page.Blockocracy

            BlockocracyAdminMembers subModel ->
                BlockocracyAdmin.view subModel
                    |> BVP.frame BVP.Admin
                    |> Html.map BlockocracyAdminMembersMsg
                    |> frame Page.Blockocracy



-- UPDATE --


type Msg
    = BlockocracyVoteLoaded (Result PageLoadError BV.Page)
    | BlockocracyProposeLoaded (Result PageLoadError BP.Page)
    | BlockocracyAdminMembersLoaded (Result PageLoadError BlockocracyAdmin.Page)
    | BlockocracyVoteMsg BV.Msg
    | BlockocracyProposeMsg BP.Msg
    | BlockocracyAdminMembersMsg BlockocracyAdmin.Msg
    | BannerMsg BE.BlockchainEvent
    | SetRoute (Maybe Route)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task cmds =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Cmd.batch (Task.attempt toMsg task :: cmds)

        errored =
            pageErrored model
    in
        case maybeRoute of
            Nothing ->
                { model | pageState = Loaded NotFound } => Cmd.none

            Just Route.Home ->
                transition BlockocracyVoteLoaded BV.init []

            Just (Route.Blockocracy Route.Vote) ->
                transition BlockocracyVoteLoaded BV.init []

            Just (Route.Blockocracy Route.Propose) ->
                transition BlockocracyProposeLoaded BP.init []

            Just (Route.Blockocracy Route.Admin) ->
                transition
                    BlockocracyAdminMembersLoaded
                    BlockocracyAdmin.init
                    [ BlockPorts.getVotingRules "" ]


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

            ( BannerMsg bcEvt, _ ) ->
                ( { model | bannerMessage = BE.bannerMessage bcEvt }, Cmd.none )

            ( BlockocracyVoteLoaded (Ok subModel), _ ) ->
                { model | pageState = Loaded (BlockocracyVote subModel) } => Cmd.none

            ( BlockocracyVoteLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( BlockocracyVoteMsg subMsg, BlockocracyVote subModel ) ->
                toPage BlockocracyVote BlockocracyVoteMsg BV.update subMsg subModel

            ( BlockocracyProposeLoaded (Ok subModel), _ ) ->
                { model | pageState = Loaded (BlockocracyPropose subModel) } => Cmd.none

            ( BlockocracyProposeLoaded (Err error), _ ) ->
                { model | pageState = Loaded (Errored error) } => Cmd.none

            ( BlockocracyProposeMsg subMsg, BlockocracyPropose subModel ) ->
                toPage BlockocracyPropose BlockocracyProposeMsg BP.update subMsg subModel

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
        , globalSubscriptions
        ]


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


globalSubscriptions : Sub Msg
globalSubscriptions =
    Sub.batch
        [ Ports.proposalAdded <|
            BannerMsg
                << BE.TxReceiptReceived BE.Proposal
                << Decode.decodeValue Web3.txReceiptDecoder
        , Ports.proposalAddedTxAddressCreated <|
            BannerMsg
                << BE.TxAddressCreated BE.Proposal
                << Decode.decodeValue Web3.txHashDecoder
        , Ports.voted <|
            BannerMsg
                << BE.TxReceiptReceived BE.Vote
                << Decode.decodeValue Web3.txReceiptDecoder
        , Ports.votedTxAddressCreated <|
            BannerMsg
                << BE.TxAddressCreated BE.Vote
                << Decode.decodeValue Web3.txHashDecoder
        ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        Blank ->
            Sub.none

        Errored _ ->
            Sub.none

        NotFound ->
            Sub.none

        BlockocracyVote _ ->
            Sub.batch
                [ BlockPorts.proposalReceived <|
                    BlockocracyVoteMsg
                        << BV.ProposalLoaded
                        << Decode.decodeValue Proposal.proposalResponseDecoder
                ]

        BlockocracyPropose _ ->
            Sub.none

        BlockocracyAdminMembers _ ->
            Sub.batch
                [ BlockPorts.votingRulesReceived <|
                    BlockocracyAdminMembersMsg
                        << BlockocracyAdmin.VotingRulesLoaded
                        << Decode.decodeValue Vote.votingRulesDecoder
                ]



-- MAIN --


main : Program Value Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
