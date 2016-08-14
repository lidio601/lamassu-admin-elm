module Pair exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text, node)
import Html.Attributes exposing (id, attribute)
import Http
import RemoteData exposing (..)


-- MODEL


type alias Model =
    WebData String


getTotem : Cmd Msg
getTotem =
    Http.getString "http://localhost:8093/totem"
        |> RemoteData.asCmd


initModel : Model
initModel =
    NotAsked


load : ( Model, Cmd Msg )
load =
    ( Loading, getTotem )



-- UPDATE


type alias Msg =
    WebData String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    msg ! []


view : Model -> Html Msg
view model =
    case Debug.log "DEBUG3" model of
        NotAsked ->
            div [] []

        Loading ->
            div []
                [ div [] [ text "..." ] ]

        Failure err ->
            div [] [ text (toString err) ]

        Success totem ->
            div [] [ node "qr-code" [ attribute "data" totem ] [] ]
