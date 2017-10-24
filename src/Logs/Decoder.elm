module Logs.Decoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Extra exposing (date, fromResult)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Logs.Types exposing (..)


logsDecoder : Decoder Logs
logsDecoder =
    decode Logs
        |> required "logs" (list logDecoder)
        |> required "currentMachine" machineDecoder


logDecoder : Decoder Log
logDecoder =
    decode Log
        |> required "id" string
        |> required "deviceId" (nullable string)
        |> required "timestamp" (nullable date)
        |> required "logLevel" (nullable string)
        |> required "message" (nullable string)


machinesDecoder : Decoder Machines
machinesDecoder =
    field "machines" (list machineDecoder)


machineDecoder : Decoder Machine
machineDecoder =
    decode Machine
        |> required "deviceId" string
        |> required "name" string
