module Page.Home exposing (Model, Msg, init, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Task exposing (Task)


-- MODEL --


type alias Model =
    { msg : String
    }


init : Task PageLoadError Model
init =
    Task.succeed (Model "hello blockchain!")



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ text model.msg
        ]



-- UPDATE --


type Msg = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop -> (model, Cmd.none)
