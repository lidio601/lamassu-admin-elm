module Logs.Rest exposing (..)

import RemoteData exposing (..)
import Http


-- import HttpBuilder exposing (..)

import Logs.Decoder exposing (logsDecoder, machinesDecoder)
import Logs.Types exposing (..)


getLogs : Maybe String -> Cmd Msg
getLogs maybeId =
    Http.get ("/api/logs/" ++ (Maybe.withDefault "" maybeId)) logsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map LoadLogs


getMachines : Cmd Msg
getMachines =
    Http.get "/api/machines/" machinesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map LoadMachines
