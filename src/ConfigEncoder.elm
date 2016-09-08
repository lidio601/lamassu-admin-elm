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


encodeFieldClusterHelper : String -> (valueType -> Value) -> List (FieldInstance valueType) -> Maybe Value
encodeFieldClusterHelper fieldCode encoder fieldInstances =
    let
        instances =
            List.filterMap (encodeFieldInstance encoder) fieldInstances
    in
        if List.isEmpty instances then
            Nothing
        else
            Just
                (object
                    [ ( "fieldCode", string fieldCode )
                    , ( "fieldInstances", list instances )
                    ]
                )


stringTuple : ( String, String ) -> Value
stringTuple ( x, y ) =
    list [ string x, string y ]


encodeFieldCluster : FieldCluster -> Maybe Value
encodeFieldCluster fieldCluster =
    case fieldCluster of
        FieldStringCluster fieldCode fieldInstances ->
            encodeFieldClusterHelper fieldCode string fieldInstances

        FieldPercentageCluster fieldCode fieldInstances ->
            encodeFieldClusterHelper fieldCode float fieldInstances

        FieldIntegerCluster fieldCode fieldInstances ->
            encodeFieldClusterHelper fieldCode int fieldInstances

        FieldOnOffCluster fieldCode fieldInstances ->
            encodeFieldClusterHelper fieldCode bool fieldInstances

        FieldAccountCluster fieldCode fieldInstances _ ->
            encodeFieldClusterHelper fieldCode stringTuple fieldInstances

        FieldCurrencyCluster fieldCode fieldInstances _ ->
            encodeFieldClusterHelper fieldCode string fieldInstances

        FieldLanguageCluster fieldCode fieldInstances _ ->
            encodeFieldClusterHelper fieldCode (list << (List.map string)) fieldInstances


encodeResults : String -> List FieldCluster -> Value
encodeResults configGroupCode fieldClusters =
    Json.Encode.object
        [ ( "groupCode", string configGroupCode )
        , ( "fieldClusters", list (List.filterMap encodeFieldCluster fieldClusters) )
        ]
