module MachinesDecoder exposing (machinesDecoder)

import Json.Decode exposing (..)
import MachineTypes exposing (..)


machineDecoder : Decoder Machine
machineDecoder =
    map6 Machine
        (field "deviceId" string)
        (field "name" string)
        (field "cashbox" int)
        (field "cassette1" int)
        (field "cassette2" int)
        (field "paired" bool)


machinesDecoder : Decoder Machines
machinesDecoder =
    map identity
        (field "machines" (list machineDecoder))
