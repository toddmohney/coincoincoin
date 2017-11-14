module Page.Blockocracy exposing (BlockocracyPage, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Task exposing (Task)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)


type alias BlockocracyPage =
    { count : Int
    }


init : Task PageLoadError BlockocracyPage
init =
    Task.succeed <|
        BlockocracyPage 1


view : BlockocracyPage -> Html Msg
view model =
    div [ class "container" ]
        [ div
            [ class "row" ]
            []
        ]


type Msg
    = Hi String


update : Msg -> BlockocracyPage -> ( BlockocracyPage, Cmd Msg )
update msg model =
    ( model, Cmd.none )
