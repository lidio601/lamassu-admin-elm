module ConfigEncoder exposing (..)

import Json.Encode exposing (..)
import List
import ConfigTypes exposing (..)


encodeFieldValue : Maybe FieldValue -> Value
encodeFieldValue maybeFieldValue =
    case maybeFieldValue of
        Nothing ->
            null

        Just fieldValue ->
            case fieldValue of
                FieldStringValue value ->
                    string value

                FieldPercentageValue value ->
                    float value

                FieldIntegerValue value ->
                    int value

                FieldOnOffValue value ->
                    bool value

                FieldAccountValue value ->
                    string value

                FieldCurrencyValue value ->
                    string value


encodeFieldType : Maybe FieldValue -> Value
encodeFieldType maybeFieldValue =
    case maybeFieldValue of
        Nothing ->
            null

        Just fieldValue ->
            case fieldValue of
                FieldStringValue _ ->
                    string "string"

                FieldPercentageValue _ ->
                    string "percentage"

                FieldIntegerValue _ ->
                    string "integer"

                FieldOnOffValue _ ->
                    string "onOff"

                FieldAccountValue _ ->
                    string "account"

                FieldCurrencyValue _ ->
                    string "currency"


dirtyValue : Field -> Maybe ValidDirtyField
dirtyValue field =
    let
        maybeMaybeFieldValue =
            case field.fieldValue of
                Err _ ->
                    Nothing

                Ok maybeFieldValue ->
                    case field.loadedFieldValue of
                        Nothing ->
                            Just maybeFieldValue

                        Just loadedFieldValue ->
                            case maybeFieldValue of
                                Nothing ->
                                    Just maybeFieldValue

                                Just fieldValue ->
                                    if (fieldValue /= loadedFieldValue) then
                                        Just maybeFieldValue
                                    else
                                        Nothing

        toValidDirtyField maybeFieldValue =
            { fieldLocator = field.fieldLocator
            , fieldValue = maybeFieldValue
            }
    in
        Maybe.map toValidDirtyField maybeMaybeFieldValue


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


encodeFieldScope : FieldScope -> Value
encodeFieldScope fieldScope =
    Json.Encode.object
        [ ( "crypto", encodeCrypto fieldScope.crypto )
        , ( "machine", encodeMachine fieldScope.machine )
        ]


encodeFieldLocator : FieldLocator -> Value
encodeFieldLocator fieldLocator =
    Json.Encode.object
        [ ( "fieldScope", encodeFieldScope fieldLocator.fieldScope )
        , ( "code", string fieldLocator.code )
        ]


encodeField : ValidDirtyField -> Value
encodeField field =
    Json.Encode.object
        [ ( "fieldLocator", encodeFieldLocator field.fieldLocator )
        , ( "fieldValue", encodeFieldValue field.fieldValue )
        , ( "fieldType", encodeFieldType field.fieldValue )
        ]


encodeConfigGroup : ConfigGroup -> Value
encodeConfigGroup configGroup =
    let
        dirtyFields =
            List.filterMap dirtyValue configGroup.values
    in
        Json.Encode.object
            [ ( "code", string configGroup.schema.code )
            , ( "values", list (List.map encodeField dirtyFields) )
            ]
