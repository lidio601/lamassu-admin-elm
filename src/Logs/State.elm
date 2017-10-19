module Logs.State exposing (..)

import RemoteData exposing (..)
import Logs.Rest exposing (..)
import Logs.Types exposing (..)


init : Model
init =
    NotAsked


loadCmd : String -> Cmd Msg
loadCmd id =
    getLogs id


load : String -> ( Model, Cmd Msg )
load id =
    ( Loading, getLogs id )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadLogs loadedModel ->
            loadedModel ! []
