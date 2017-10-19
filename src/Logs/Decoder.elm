module Logs.Decoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (date, fromResult)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Logs.Types exposing (..)


logsDecoder : Decoder (List Log)
logsDecoder =
    field "logs" (list logDecoder)


logDecoder : Decoder Log
logDecoder =
    decode Log
        |> required "id" string
        |> required "deviceId" (nullable string)
        |> required "name" (nullable string)
        |> required "timestamp" (nullable date)
        |> required "logLevel" (nullable string)
        |> required "message" (nullable string)
