module Account exposing (..)

import Http
import Html exposing (Html, Attribute, a, div, hr, input, span, text)
import RemoteData exposing (..)
import AccountRecord


type alias Model =
    WebData AccountRecord.AccountResult


get : String -> Cmd Msg
get code =
    Http.getString ("http://localhost:8093/account/" ++ code)
        |> RemoteData.asCmd


initModel : Model
initModel =
    RemoteData.NotAsked


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
    case model of
        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success accountResult ->
            case accountResult of
                Ok account ->
                    div [] [ text ("Account: " ++ account.display) ]

                Err err ->
                    div [] [ text "Server error" ]
