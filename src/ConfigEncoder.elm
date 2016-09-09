module ConfigEncoder exposing (..)

import Json.Encode exposing (..)
import List
import ConfigTypes exposing (..)


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
    object
        [ ( "crypto", encodeCrypto fieldScope.crypto )
        , ( "machine", encodeMachine fieldScope.machine )
        ]


encodeFieldCode : FieldCode -> Value
encodeFieldCode fieldCode =
    let
        fieldClassValue =
            case fieldCode.fieldClass of
                Nothing ->
                    null

                Just fieldClass ->
                    string fieldClass
    in
        object
            [ ( "fieldName", string fieldCode.fieldName )
            , ( "fieldClass", fieldClassValue )
            ]


encodeFieldLocator : FieldLocator -> Value
encodeFieldLocator fieldLocator =
    object
        [ ( "fieldCode", encodeFieldCode fieldLocator.fieldCode )
        , ( "fieldScope", encodeFieldScope fieldLocator.fieldScope )
        ]


encodeValue : FieldHolder valueType -> Maybe valueType -> (valueType -> Value) -> Maybe Value
encodeValue fieldHolder maybeLoadedValue encoder =
    case fieldHolder of
        Nothing ->
            case maybeLoadedValue of
                Nothing ->
                    Nothing

                Just _ ->
                    Just null

        Just result ->
            case result of
                Ok value ->
                    case maybeLoadedValue of
                        Nothing ->
                            Just (encoder value)

                        Just loadedValue ->
                            if (loadedValue == value) then
                                Nothing
                            else
                                Just (encoder value)

                Err _ ->
                    Nothing


encodeFieldInstance :
    FieldCode
    -> (valueType -> Value)
    -> FieldInstance valueType componentModel
    -> Maybe Value
encodeFieldInstance fieldCode encoder fieldInstance =
    let
        fieldLocator =
            { fieldCode = fieldCode
            , fieldScope = fieldInstance.fieldScope
            }

        encode value =
            object
                [ ( "fieldLocator", encodeFieldLocator fieldLocator )
                , ( "fieldValue", value )
                ]
    in
        encodeValue fieldInstance.fieldValue fieldInstance.loadedValue encoder


encodeFieldClusterHelper :
    FieldCode
    -> (valueType -> Value)
    -> List (FieldInstance valueType componentModel)
    -> Maybe Value
encodeFieldClusterHelper fieldCode encoder fieldInstances =
    let
        instances =
            List.filterMap (encodeFieldInstance fieldCode encoder) fieldInstances
    in
        if List.isEmpty instances then
            Nothing
        else
            Just (list instances)


stringTuple : ( String, String ) -> Value
stringTuple ( x, y ) =
    list [ string x, string y ]


encodeFieldCluster : FieldCode -> FieldCluster -> Maybe Value
encodeFieldCluster fieldCode fieldCluster =
    case fieldCluster of
        FieldInputCluster inputCluster ->
            case inputCluster of
                FieldStringCluster fieldInstances ->
                    encodeFieldClusterHelper fieldCode string fieldInstances

                FieldPercentageCluster fieldInstances ->
                    encodeFieldClusterHelper fieldCode float fieldInstances

                FieldIntegerCluster fieldInstances ->
                    encodeFieldClusterHelper fieldCode int fieldInstances

                FieldOnOffCluster fieldInstances ->
                    encodeFieldClusterHelper fieldCode bool fieldInstances

        FieldSelectizeCluster selectizeCluster ->
            case selectizeCluster of
                FieldAccountCluster fieldInstances ->
                    encodeFieldClusterHelper fieldCode string fieldInstances

                FieldCurrencyCluster fieldInstances ->
                    encodeFieldClusterHelper fieldCode string fieldInstances

                FieldLanguageCluster fieldInstances ->
                    encodeFieldClusterHelper fieldCode (list << (List.map string)) fieldInstances


encodeFieldGroup : FieldGroup -> Maybe Value
encodeFieldGroup fieldGroup =
    encodeFieldCluster fieldGroup.fieldCode fieldGroup.fieldCluster


encodeResults : String -> List FieldGroup -> Value
encodeResults configGroupCode fieldGroups =
    object
        [ ( "groupCode", string configGroupCode )
        , ( "fieldValues", list (List.filterMap encodeFieldGroup fieldGroups) )
        ]
