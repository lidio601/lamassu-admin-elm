module ConfigDecoder exposing (..)

import Json.Decode exposing (..)
import FieldSetDecoder exposing (..)
import FieldSetTypes exposing (..)
import ConfigTypes exposing (..)


string2machine : String -> Machine
string2machine s =
    if s == "global" then
        GlobalMachine
    else
        MachineId s


machineDecoder : Decoder Machine
machineDecoder =
    map string2machine string


machineConfigDecoder : Decoder MachineConfig
machineConfigDecoder =
    object2 makeFieldSetRow
        ("id" := machineDecoder)
        ("fieldSet" := fieldSetDecoder)


string2Crypto : String -> Crypto
string2Crypto s =
    if s == "global" then
        GlobalCrypto
    else
        CryptoCode s


cryptoDecoder : Decoder Crypto
cryptoDecoder =
    map string2Crypto string


cryptoConfigDecoder : Decoder CryptoConfig
cryptoConfigDecoder =
    object2 makeFieldSetTable
        ("crypto" := cryptoDecoder)
        ("machineConfigs" := list machineConfigDecoder)


configGroupDecoder : Decoder ConfigGroup
configGroupDecoder =
    object3 ConfigGroup
        ("code" := string)
        ("display" := string)
        ("cryptoConfigs" := list cryptoConfigDecoder)
