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
    Json.Encode.object
        [ ( "crypto", encodeCrypto fieldScope.crypto )
        , ( "machine", encodeMachine fieldScope.machine )
        ]


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


encodeFieldInstance : (valueType -> Value) -> FieldInstance valueType componentModel -> Maybe Value
encodeFieldInstance encoder fieldInstance =
    let
        encode value =
            Json.Encode.object
                [ ( "fieldScope", encodeFieldScope fieldInstance.fieldScope )
                , ( "fieldValue", value )
                ]
    in
        encodeValue fieldInstance.fieldValue fieldInstance.loadedValue encoder


encodeFieldClusterHelper : (valueType -> Value) -> List (FieldInstance valueType componentModel) -> Maybe Value
encodeFieldClusterHelper encoder fieldInstances =
    let
        instances =
            List.filterMap (encodeFieldInstance encoder) fieldInstances
    in
        if List.isEmpty instances then
            Nothing
        else
            Just
                (object
                    [ ( "fieldInstances", list instances )
                    ]
                )


encodeAccountClusterHelper : String -> (valueType -> Value) -> List (FieldInstance valueType componentModel) -> Maybe Value
encodeAccountClusterHelper accountClass encoder fieldInstances =
    let
        instances =
            List.filterMap (encodeFieldInstance encoder) fieldInstances
    in
        if List.isEmpty instances then
            Nothing
        else
            Just
                (object
                    [ ( "accountClass", string accountClass )
                    , ( "fieldInstances", list instances )
                    ]
                )


stringTuple : ( String, String ) -> Value
stringTuple ( x, y ) =
    list [ string x, string y ]


encodeFieldCluster : FieldCluster -> Maybe Value
encodeFieldCluster fieldCluster =
    case fieldCluster of
        FieldInputCluster inputCluster ->
            case inputCluster of
                FieldStringCluster fieldInstances ->
                    encodeFieldClusterHelper string fieldInstances

                FieldPercentageCluster fieldInstances ->
                    encodeFieldClusterHelper float fieldInstances

                FieldIntegerCluster fieldInstances ->
                    encodeFieldClusterHelper int fieldInstances

                FieldOnOffCluster fieldInstances ->
                    encodeFieldClusterHelper bool fieldInstances

        FieldSelectizeCluster selectizeCluster ->
            case selectizeCluster of
                FieldAccountCluster accountClass fieldInstances ->
                    encodeAccountClusterHelper accountClass string fieldInstances

                FieldCurrencyCluster fieldInstances ->
                    encodeFieldClusterHelper string fieldInstances

                FieldLanguageCluster fieldInstances ->
                    encodeFieldClusterHelper (list << (List.map string)) fieldInstances


encodeFieldGroup : FieldGroup -> Maybe Value
encodeFieldGroup fieldGroup =
    case fieldGroup of
        ClassedFieldGroup fieldGroup ->
            encodeFieldCluster fieldGroup.fieldCluster
                |> Maybe.map
                    (\encodedFieldCluster ->
                        object
                            [ ( "fieldCode", string fieldGroup.fieldCode )
                            , ( "fieldCluster", encodedFieldCluster )
                            , ( "fieldClass", string fieldGroup.fieldClass )
                            ]
                    )

        UnclassedFieldGroup fieldGroup ->
            encodeFieldCluster fieldGroup.fieldCluster
                |> Maybe.map
                    (\encodedFieldCluster ->
                        object
                            [ ( "fieldCode", string fieldGroup.fieldCode )
                            , ( "fieldCluster", encodedFieldCluster )
                            ]
                    )


encodeResults : String -> List FieldGroup -> Value
encodeResults configGroupCode fieldGroups =
    object
        [ ( "groupCode", string configGroupCode )
        , ( "fieldGroups", list (List.filterMap encodeFieldGroup fieldGroups) )
        ]
