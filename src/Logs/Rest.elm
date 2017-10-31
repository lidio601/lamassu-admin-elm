module Logs.Rest exposing (..)

import RemoteData exposing (..)
import Http
import HttpBuilder exposing (..)
import Common.Logs.Decoder exposing (logsDecoder, machinesDecoder, supportLogDecoder)
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


shareLogs : String -> Cmd Msg
shareLogs id =
    post ("/api/support_logs?deviceId=" ++ id)
        |> withExpect (Http.expectJson supportLogDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map LoadSupportLog
