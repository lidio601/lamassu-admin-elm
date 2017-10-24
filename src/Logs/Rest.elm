module Logs.Rest exposing (..)

import RemoteData exposing (..)
import Http


-- import HttpBuilder exposing (..)

import Logs.Decoder exposing (logsDecoder, machinesDecoder)
import Logs.Types exposing (..)


getLogs : String -> Cmd Msg
getLogs id =
    Http.get ("/api/logs/" ++ id) logsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map LoadLogs


getMachines : Cmd Msg
getMachines =
    Http.get "/api/machines/" machinesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map LoadMachines


getDefaultLogs : Cmd Msg
getDefaultLogs =
    Http.get ("/api/logs/") logsDecoder
        |> RemoteData.sendRequest
        |> Cmd.map LoadLogs
