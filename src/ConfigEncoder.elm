module ConfigEncoder exposing (..)

import Json.Encode exposing (..)
import List
import ConfigTypes exposing (..)


encodeFieldValueObject : String -> Value -> Value
encodeFieldValueObject fieldTypeStr value =
    object [ ( "fieldType", string fieldTypeStr ), ( "value", value ) ]


encodeFieldValue : Maybe FieldValue -> Value
encodeFieldValue maybeFieldValue =
    case maybeFieldValue of
        Nothing ->
            null

        Just fieldValue ->
            case fieldValue of
                FieldStringValue value ->
                    encodeFieldValueObject "string" (string value)

                FieldPercentageValue value ->
                    encodeFieldValueObject "percentage" (float value)

                FieldIntegerValue value ->
                    encodeFieldValueObject "integer" (int value)

                FieldOnOffValue value ->
                    encodeFieldValueObject "onOff" (bool value)

                FieldAccountValue accountClass value ->
                    object
                        [ ( "fieldType", string "account" )
                        , ( "accountClass", string accountClass )
                        , ( "value", string value )
                        ]

                FieldCurrencyValue value ->
                    encodeFieldValueObject "currency" (string value)

                FieldLanguageValue value ->
                    encodeFieldValueObject "language" (list (List.map string value))


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


encodeFieldInstance : (valueType -> Value) -> FieldInstance valueType -> Maybe Value
encodeFieldInstance encoder fieldInstance =
    let
        encode value =
            Json.Encode.object
                [ ( "fieldScope", encodeFieldScope fieldInstance.fieldScope )
                , ( "fieldValue", value )
                ]
    in
        encodeValue fieldInstance.value fieldInstance.loadedValue encoder


encodeValue : FieldHolder valueType -> Maybe valueType -> (valueType -> Value) -> Maybe Value
encodeValue fieldHolder maybeLoadedValue encoder =
    let
        onlyDirty maybeValue =
            if (maybeLoadedValue == maybeValue) then
                Nothing
            else
                case maybeValue of
                    Nothing ->
                        Just null

                    Just value ->
                        Just (encoder value)
    in
        Result.toMaybe fieldHolder
            `Maybe.andThen` onlyDirty


encodeFieldCluster : FieldLocator -> FieldCluster -> List Value
encodeFieldCluster fieldLocator fieldCluster =
    case fieldCluster of
        FieldStringCluster fieldInstances ->
            List.filterMap (encodeFieldInstance string) fieldInstances



-- FieldPercentageCluster (FieldInstance Float)
-- FieldIntegerCluster (FieldInstance Int)
-- FieldOnOffCluster (FieldInstance Bool)
-- FieldAccountCluster (FieldInstance ( String, String )) (Selectize.Model String)
-- FieldCurrencyCluster (FieldInstance String) (Selectize.Model String)
-- FieldLanguageCluster (FieldInstance (List String)) (Selectize.Model String)


encodeFieldGroup : FieldGroup -> Maybe Value
encodeFieldGroup fieldGroup =
    let
        fieldClusterValues =
            encodeFieldCluster fieldGroup.fieldLocator fieldGroup.fieldCluster

        toCluster values =
            Json.Encode.object
                [ ( "code", string fieldGroup.code )
                , ( "fieldClusters", values )
                ]
    in
        if List.isEmpty fieldClusterValues then
            Nothing
        else
            Just (List.map toCluster fieldClusterValues)


encodeResults : String -> List FieldGroup -> Value
encodeResults configGroupCode fieldGroups =
    Json.Encode.object
        [ ( "groupCode", string configGroupCode )
        , ( "fieldClusters", list (List.filterMap encodeFieldGroup fieldGroups) )
        ]
