module Logs.Rest exposing (..)

import RemoteData exposing (..)
import Http
import HttpBuilder exposing (..)
import Logs.Decoder exposing (logsDecoder)
import Logs.Types exposing (..)


getLogs : String -> Cmd Msg
getLogs id =
    get ("/api/logs/" ++ id)
        |> withExpect (Http.expectJson logsDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map LoadLogs
