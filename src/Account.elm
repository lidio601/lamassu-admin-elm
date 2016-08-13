module Account exposing (..)

import Http
import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import RemoteData exposing (RemoteData, WebData)
import AccountRecord


type alias Model =
    WebData AccountRecord.AccountResult


get : String -> Cmd Msg
get code =
    Http.getString ("http://localhost:8093/account/" ++ code)
        |> RemoteData.asCmd


init : ( Model, Cmd Msg )
init =
    ( RemoteData.NotAsked, Cmd.none )


load : String -> ( Model, Cmd Msg )
load code =
    ( RemoteData.Loading, get code )



-- UPDATE


type alias Msg =
    WebData String


update : Msg -> Model -> ( Model, Cmd Msg )
update webdata model =
    let
        decodingMapper json =
            ( AccountRecord.decode json, Cmd.none )
    in
        RemoteData.update decodingMapper webdata


view : Model -> Html Msg
view model =
    div [] []
