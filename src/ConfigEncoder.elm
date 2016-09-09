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


encodeComponentInstanceRec :
    (valueType -> Value)
    -> ComponentFieldInstanceRec valueType componentType
    -> Maybe Value
encodeComponentInstanceRec encoder instance =
    encodeValue instance.fieldValue instance.loadedValue encoder
        |> Maybe.map
            (\fieldValue ->
                (object
                    [ ( "fieldScope", encodeFieldScope instance.fieldScope )
                    , ( "fieldValue", fieldValue )
                    ]
                )
            )


encodeInstanceRec : (valueType -> Value) -> FieldInstanceRec valueType -> Maybe Value
encodeInstanceRec encoder instance =
    encodeValue instance.fieldValue instance.loadedValue encoder
        |> Maybe.map
            (\fieldValue ->
                (object
                    [ ( "fieldScope", encodeFieldScope instance.fieldScope )
                    , ( "fieldValue", fieldValue )
                    ]
                )
            )


encodeFieldInstance : FieldInstance -> Maybe Value
encodeFieldInstance fieldInstance =
    case fieldInstance of
        FieldInputInstance inputInstance ->
            case inputInstance of
                FieldStringInstance instance ->
                    encodeInstanceRec string instance

                FieldPercentageInstance instance ->
                    encodeInstanceRec float instance

                FieldIntegerInstance instance ->
                    encodeInstanceRec int instance

                FieldOnOffInstance instance ->
                    encodeInstanceRec bool instance

        FieldSelectizeInstance selectizeInstance ->
            case selectizeInstance of
                FieldAccountInstance instance ->
                    encodeComponentInstanceRec string instance

                FieldCurrencyInstance instance ->
                    encodeComponentInstanceRec string instance

                FieldLanguageInstance instance ->
                    encodeComponentInstanceRec (list << (List.map string)) instance


encodeFieldGroup : FieldGroup -> Maybe Value
encodeFieldGroup fieldGroup =
    case fieldGroup of
        UnclassedFieldGroup fieldGroup ->
            List.filterMap encodeFieldInstance fieldGroup.fieldInstances
                |> (\instances ->
                        if List.isEmpty instances then
                            Nothing
                        else
                            Just
                                (object
                                    [ ( "fieldCode", string fieldGroup.fieldCode )
                                    , ( "fieldInstances", list instances )
                                    ]
                                )
                   )

        ClassedFieldGroup fieldGroup ->
            List.filterMap encodeFieldInstance fieldGroup.fieldInstances
                |> (\instances ->
                        if List.isEmpty instances then
                            Nothing
                        else
                            Just
                                (object
                                    [ ( "fieldCode", string fieldGroup.fieldCode )
                                    , ( "fieldInstances", list instances )
                                    , ( "fieldClass", string fieldGroup.fieldClass )
                                    ]
                                )
                   )


encodeResults : String -> List FieldGroup -> Value
encodeResults configGroupCode fieldGroups =
    object
        [ ( "groupCode", string configGroupCode )
        , ( "fieldGroups", list (List.filterMap encodeFieldGroup fieldGroups) )
        ]
