module Start exposing (..)

import Http
import RemoteData exposing (RemoteData, WebData)
import InitialRecord


type alias Model =
    { initialRecordResult : WebData InitialRecord.InitialRecordResult }


get : Cmd Msg
get =
    Http.getString "http://localhost:8093/initial"
        |> RemoteData.asCmd
        |> Cmd.map Initial


init : ( Model, Cmd Msg )
init =
    ( { initialRecordResult = RemoteData.Loading }, get )



-- UPDATE


type Msg
    = Initial (WebData String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "DEBUG0" msg of
        Initial webdata ->
            let
                initialRecordResult =
                    RemoteData.map InitialRecord.decode webdata
            in
                { initialRecordResult = initialRecordResult } ! []
