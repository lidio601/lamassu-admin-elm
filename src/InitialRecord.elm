module InitialRecord exposing (..)

import Json.Decode exposing (..)
import Plugin


type alias GlobalMachine =
    { plugins : List Plugin.Plugin }


type alias GlobalCrypto =
    { global : GlobalMachine
    }


type alias Config =
    { global : GlobalCrypto }


type alias InitialRecord =
    { config : Config }



-- Decoders


globalMachineDecoder : Decoder GlobalMachine
globalMachineDecoder =
    object1 GlobalMachine
        ("plugins" := list Plugin.pluginDecoder)


globalCryptoDecoder : Decoder GlobalCrypto
globalCryptoDecoder =
    object1 GlobalCrypto
        ("global" := globalMachineDecoder)


configDecoder : Decoder Config
configDecoder =
    object1 Config
        ("global" := globalCryptoDecoder)


initialRecordDecoder : Decoder InitialRecord
initialRecordDecoder =
    object1 InitialRecord
        ("config" := configDecoder)


type alias InitialRecordResult =
    Result String InitialRecord


decode : String -> InitialRecordResult
decode string =
    decodeString initialRecordDecoder string
