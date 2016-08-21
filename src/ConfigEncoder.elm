module ConfigEncoder exposing (..)

import Json.Encode exposing (..)
import List
import ConfigTypes exposing (..)
import FieldSetEncoder exposing (..)


encodeCrypto : Crypto -> Value
encodeCrypto crypto =
    case crypto of
        CryptoCode cryptoCode ->
            string cryptoCode

        GlobalCrypto ->
            string "global"


encodeMachine : Machine -> Value
encodeMachine machine =
    case machine of
        MachineId machineId ->
            string machineId

        GlobalMachine ->
            string "global"


encodeMachineConfig : MachineConfig -> Value
encodeMachineConfig machineConfig =
    object
        [ ( "machine", encodeMachine machineConfig.id )
        , ( "fieldSet", encodeFieldSet machineConfig.fieldSet )
        ]


encodeCryptoConfig : CryptoConfig -> Value
encodeCryptoConfig cryptoConfig =
    object
        [ ( "cryptoCode", encodeCrypto cryptoConfig.id )
        , ( "machineConfigs", list (List.map encodeMachineConfig cryptoConfig.rows) )
        ]


encodeConfigGroup : ConfigGroup -> Value
encodeConfigGroup configGroup =
    Json.Encode.object
        [ ( "code", string configGroup.code )
        , ( "display", string configGroup.display )
        , ( "cryptoConfigs", list (List.map encodeCryptoConfig configGroup.cryptoConfigs) )
        ]
