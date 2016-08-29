module ConfigEncoder exposing (..)

import Json.Encode exposing (..)
import List
import ConfigTypes exposing (..)


maybe : Maybe a -> (a -> Value) -> Value
maybe value encoder =
    case value of
        Just v ->
            encoder v

        Nothing ->
            null


encodeFieldValue : FieldValue -> Value
encodeFieldValue fieldValue =
    let
        list =
            case fieldValue of
                FieldString value ->
                    [ ( "fieldType", string "string" )
                    , ( "value", maybe value string )
                    ]

                FieldPercentage value ->
                    [ ( "fieldType", string "percentage" )
                    , ( "value", maybe value int )
                    ]

                FieldInteger value ->
                    [ ( "fieldType", string "integer" )
                    , ( "value", maybe value int )
                    ]
    in
        Json.Encode.object list


encodeField : Field -> Value
encodeField field =
    -- No need to encode status or loadedValue field, they're for client-side only
    Json.Encode.object
        [ ( "code", string field.code )
        , ( "display", string field.display )
        , ( "value", encodeFieldValue field.value )
        ]


isDirty : Field -> Bool
isDirty field =
    field.value /= field.loadedValue



-- Encodes only changed fields


encodeFieldSet : FieldSet -> Value
encodeFieldSet fieldSet =
    let
        fieldValues =
            List.filter isDirty fieldSet.fields
                |> List.map encodeField
    in
        Json.Encode.object
            [ ( "fields", list fieldValues ) ]


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
        [ ( "machine", encodeMachine machineConfig.machine )
        , ( "fieldSet", encodeFieldSet machineConfig.fieldSet )
        ]


encodeCryptoConfig : CryptoConfig -> Value
encodeCryptoConfig cryptoConfig =
    object
        [ ( "cryptoCode", encodeCrypto cryptoConfig.crypto )
        , ( "machineConfigs", list (List.map encodeMachineConfig cryptoConfig.machineConfigs) )
        ]


encodeDisplayRec : DisplayRec -> Value
encodeDisplayRec displayRec =
    object
        [ ( "code", string displayRec.code )
        , ( "display", string displayRec.display )
        ]


encodeConfigScope : ConfigScope -> Value
encodeConfigScope configScope =
    case configScope of
        Global ->
            string "global"

        Specific ->
            string "specific"

        Both ->
            string "both"


encodeConfigGroup : ConfigGroup -> Value
encodeConfigGroup configGroup =
    Json.Encode.object
        [ ( "group", encodeDisplayRec configGroup.group )
        , ( "cryptoScope", encodeConfigScope configGroup.cryptoScope )
        , ( "machineScope", encodeConfigScope configGroup.machineScope )
        , ( "cryptoConfigs", list (List.map encodeCryptoConfig configGroup.cryptoConfigs) )
        ]
