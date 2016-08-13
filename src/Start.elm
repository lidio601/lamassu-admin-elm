module Start exposing (..)

import Http
import RemoteData exposing (RemoteData, WebData)
import InitialRecord


type alias Model =
    WebData InitialRecord.InitialRecordResult


get : Cmd Msg
get =
    Http.getString "http://localhost:8093/initial"
        |> RemoteData.asCmd
        |> Cmd.map Initial


init : ( Model, Cmd Msg )
init =
    ( RemoteData.NotAsked, Cmd.none )


load : ( Model, Cmd Msg )
load =
    ( RemoteData.Loading, get )



-- UPDATE


type Msg
    = Initial (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG0" msg of
        Initial webdata ->
            let
                decodingMapper json =
                    ( InitialRecord.decode json, Cmd.none )
            in
                RemoteData.update decodingMapper webdata
