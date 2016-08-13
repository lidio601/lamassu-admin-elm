module Pair exposing (..)

import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import Html.Attributes exposing (id)
import Port
import Http
import RemoteData exposing (..)


-- MODEL


type alias Model =
    WebData String



-- `Task.andThen` Port.qr "qr"


getTotem : Cmd Msg
getTotem =
    Http.getString "http://localhost:8093/totem"
        |> RemoteData.asCmd
        |> Cmd.map Totem


initModel : Model
initModel =
    NotAsked


load : ( Model, Cmd Msg )
load =
    ( Loading, getTotem )



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
    case Debug.log "DEBUG3" model of
        NotAsked ->
            div [] []

        Loading ->
            div []
                [ div [] [ text "..." ]
                , Html.canvas [ id "qr" ] []
                ]

        Failure err ->
            div [] [ text (Debug.log "DEBUG2" (toString err)) ]

        Success _ ->
            div []
                [ div [] [ text "Loaded" ]
                , Html.canvas [ id "qr" ] []
                ]
