module Page.Home exposing (Model, Msg(..), init, update, view)

import Html exposing (..)
import Html.Attributes exposing (disabled, class, classList, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Ports as Ports
import Task exposing (Task)


-- MODEL --


type alias Model =
    { helloForm : HelloForm
    , helloCount : Maybe Int
    }

type alias HelloForm =
    { address : Maybe Address
    , isValid : Bool
    , errors  : List String
    }

type Address = Address String

getAddress : Address -> String
getAddress (Address addr) = addr


init : Task PageLoadError Model
init =
    Task.succeed
        <| Model (HelloForm Nothing False []) Nothing



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ renderHelloForm model.helloForm
        , renderHelloCount model.helloCount
        ]

renderHelloCount : Maybe Int -> Html Msg
renderHelloCount mCount =
    case mCount of
        Nothing -> div [] []
        (Just ct) ->
            div
                []
                [ text <| "You have said hello to the blockchain " ++ toString ct ++ " times."
                ]

renderHelloForm : HelloForm -> Html Msg
renderHelloForm form =
    div []
        [ div
            [ class "form-group" ]
            [ input
                [ class "form-control"
                , type_ "text"
                , placeholder "Address"
                , onInput AddressInput
                ]
                []
            , renderFormValidation form
            ]

        , button
            [ classList [("btn", True), ("btn-primary", True)]
            , disabled <| not form.isValid
            , onClick SayHelloRequested
            ]
            [ text "Say Hello" ]

        , button
            [ classList [("btn", True), ("btn-default", True)]
            , disabled <| not form.isValid
            , onClick GetHelloRequested
            ]
            [ text "Get Hello Count" ]
        ]

renderFormValidation : HelloForm -> Html Msg
renderFormValidation form =
    div [ class "text-danger" ]
        [ text <| String.concat form.errors
        ]



-- UPDATE --


type Msg = AddressInput String
         | GetHelloRequested
         | HelloCountReceived Int
         | SayHelloRequested


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AddressInput addr ->
            let
                form = model.helloForm
                newForm = validateForm { form | address = Just (Address addr) }
                newModel = { model | helloForm = newForm }
            in (newModel, Cmd.none)

        SayHelloRequested ->
            case model.helloForm.address of
                Nothing ->
                    (model, Cmd.none)
                (Just addr) ->
                    (model, Ports.sayHello <| getAddress addr)

        GetHelloRequested ->
            case model.helloForm.address of
                Nothing ->
                    (model, Cmd.none)
                (Just addr) ->
                    (model, Ports.getHelloCount <| getAddress addr)

        HelloCountReceived ct ->
            let
                newModel = { model | helloCount = Just ct }
            in
                (newModel, Cmd.none)


validateForm : HelloForm -> HelloForm
validateForm form =
    case form.address of
        Nothing ->
            { form | isValid = True
            , errors = ["Please enter your blockchain address"]
            }
        (Just addr) ->
            if addrLength addr == addrLength sampleAddr
                then
                    { form | isValid = True
                    , errors = []
                    }
                else
                    { form | isValid = False
                    , errors = ["Your blockchain address must be " ++ toString (addrLength sampleAddr) ++ " characters long"]
                    }

addrLength : Address -> Int
addrLength (Address addr) = String.length addr

sampleAddr : Address
sampleAddr = Address "0x0000000000000000000000000000000000000000"
