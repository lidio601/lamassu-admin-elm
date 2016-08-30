module ConfigEncoder exposing (..)

import Json.Encode exposing (..)
import List
import ConfigTypes exposing (..)


encodeFieldValue : FieldValue -> Value
encodeFieldValue fieldValue =
    case fieldValue of
        FieldStringValue value ->
            string value

        FieldPercentageValue value ->
            float value

        FieldIntegerValue value ->
            int value


encodeFieldType : FieldValue -> Value
encodeFieldType fieldValue =
    case fieldValue of
        FieldStringValue _ ->
            string "string"

        FieldPercentageValue _ ->
            string "percentage"

        FieldIntegerValue _ ->
            string "integer"


isDirty : Field -> Bool
isDirty field =
    case field.loadedFieldValue of
        Nothing ->
            True
        Just loadedFieldValue ->
            field.fieldValue /= loadedFieldValue


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


encodeField : Field -> Value
encodeField field =
    Json.Encode.object
        [ ( "code", string field.code )
        , ( "crypto", encodeCrypto field.crypto )
        , ( "machine", encodeMachine field.machine )
        , ( "fieldValue", encodeFieldValue field.fieldValue )
        , ( "fieldType", encodeFieldType field.fieldValue )
        ]


encodeConfigGroup : ConfigGroup -> Value
encodeConfigGroup configGroup =
    let
        dirtyFields =
            List.filter isDirty configGroup.values
    in
        Json.Encode.object
            [ ( "values", list (List.map encodeField dirtyFields) )
            ]
