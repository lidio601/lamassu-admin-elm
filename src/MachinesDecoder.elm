module MachinesDecoder exposing (machinesDecoder)

import Json.Decode exposing (..)
import MachineTypes exposing (..)


machineDecoder : Decoder Machine
machineDecoder =
    object6 Machine
        ("deviceId" := string)
        ("name" := string)
        ("cashbox" := int)
        ("cassette1" := int)
        ("cassette2" := int)
        ("paired" := bool)


machinesDecoder : Decoder Machines
machinesDecoder =
    object1 identity
        ("machines" := list machineDecoder)
