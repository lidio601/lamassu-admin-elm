module Pair exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.Attributes exposing (id)
import Port
import Http
import RemoteData exposing (RemoteData, WebData)


-- MODEL


type alias Model =
    WebData String



-- `Task.andThen` Port.qr "qr"


getTotem : Cmd Msg
getTotem =
    Http.getString "http://localhost:8093/totem"
        |> RemoteData.asCmd
        |> Cmd.map Totem


init : ( Model, Cmd Msg )
init =
    ( RemoteData.NotAsked, Cmd.none )


load : ( Model, Cmd Msg )
load =
    ( RemoteData.Loading, getTotem )



-- UPDATE


type Msg
    = Totem (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG0" msg of
        Totem webdata ->
            let
                displayQr totem =
                    totem ! [ Port.qr "qr" (Debug.log "DEBUG1" totem) ]
            in
                RemoteData.update displayQr webdata


view : Model -> Html Msg
view model =
    Html.canvas [ id "qr" ] []
