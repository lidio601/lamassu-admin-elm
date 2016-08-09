module Pair exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.Attributes exposing (id)
import Port
import Http
import RemoteData exposing (RemoteData, WebData)


-- MODEL


type alias Model =
    { totem : Maybe String
    }



-- `Task.andThen` Port.qr "qr"


getTotem : Cmd Msg
getTotem =
    Http.getString "http://localhost:8093/totem"
        |> RemoteData.asCmd
        |> Cmd.map Totem


init : ( Model, Cmd Msg )
init =
    ( { totem = Nothing }, getTotem )



-- UPDATE


type Msg
    = Totem (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG0" msg of
        Totem webdata ->
            case webdata of
                RemoteData.NotAsked ->
                    model ! []

                RemoteData.Loading ->
                    model ! []

                RemoteData.Failure _ ->
                    model ! []

                RemoteData.Success totem ->
                    model ! [ Port.qr "qr" (Debug.log "DEBUG1" totem) ]


view : Model -> Html Msg
view model =
    div []
        [ Html.canvas [ id "qr" ] []
        ]
