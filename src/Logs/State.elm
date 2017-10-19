module Logs.State exposing (..)

import RemoteData exposing (..)
import Logs.Rest exposing (..)
import Logs.Types exposing (..)


init : Model
init =
    { logs = NotAsked, machines = NotAsked }


load : String -> ( Model, Cmd Msg )
load id =
    ( { logs = Loading, machines = Loading }, getData id )


getData : String -> Cmd Msg
getData id =
    Cmd.batch [ getLogs id, getMachines ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadLogs response ->
            ( { model | logs = response }
            , Cmd.none
            )

        LoadMachines response ->
            ( { model | machines = response }
            , Cmd.none
            )
