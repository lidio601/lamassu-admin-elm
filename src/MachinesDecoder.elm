module MachinesDecoder exposing (machinesDecoder)

import Json.Decode exposing (..)
import MachineTypes exposing (..)


machineDecoder : Decoder Machine
machineDecoder =
    object2 Machine string string


machinesDecoder : Decoder Machines
machinesDecoder =
    list machineDecoder
