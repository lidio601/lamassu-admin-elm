module InitFieldGroup exposing (initFieldCluster)

import Json.Decode exposing (..)
import ConfigTypes exposing (..)
import SelectizeHelpers exposing (..)


maybeValue : ConfigGroup -> FieldLocator -> Decoder valueType -> Maybe valueType
maybeValue configGroup fieldLocator decoder =
    let
        maybeResult =
            List.filter (((==) fieldLocator) << .fieldLocator) configGroup.fieldValues
                |> List.head
                |> Maybe.map .fieldValue
                |> Maybe.map (decodeValue decoder)
    in
        case maybeResult of
            Nothing ->
                Nothing

            Just result ->
                case result of
                    Err _ ->
                        Nothing

                    Ok fieldValue ->
                        Just fieldValue


buildComponentInstance :
    ConfigGroup
    -> Decoder valueType
    -> (FieldScope -> Maybe valueType -> componentType)
    -> FieldLocator
    -> FieldInstance valueType componentType
buildComponentInstance configGroup decoder component fieldLocator =
    let
        maybe =
            maybeValue configGroup fieldLocator decoder

        fieldScope =
            fieldLocator.fieldScope
    in
        { fieldScope = fieldLocator.fieldScope
        , fieldValue = Maybe.map Ok maybe
        , loadedValue = maybe
        , componentModel = component fieldScope maybe
        }


buildInstance :
    ConfigGroup
    -> Decoder valueType
    -> FieldLocator
    -> FieldInstance valueType ()
buildInstance configGroup decoder fieldLocator =
    let
        maybe =
            maybeValue configGroup fieldLocator decoder
    in
        { fieldScope = fieldLocator.fieldScope
        , fieldValue = Maybe.map Ok maybe
        , loadedValue = maybe
        , componentModel = ()
        }


fieldLocators : ConfigGroup -> FieldCode -> List FieldLocator
fieldLocators configGroup fieldCode =
    (fieldScopes configGroup)
        |> List.map (\fieldScope -> { fieldCode = fieldCode, fieldScope = fieldScope })


initFieldCluster : ConfigGroup -> FieldDescriptor -> FieldCluster
initFieldCluster configGroup fieldDescriptor =
    let
        locators =
            fieldLocators configGroup fieldDescriptor.fieldCode

        instance =
            buildInstance configGroup

        componentInstance =
            buildComponentInstance configGroup
    in
        case fieldDescriptor.fieldType of
            FieldTypeInput fieldType ->
                case fieldType of
                    FieldStringType ->
                        List.map (instance string) locators
                            |> FieldStringCluster
                            |> FieldInputCluster

                    FieldPercentageType ->
                        List.map (instance float) locators
                            |> FieldPercentageCluster
                            |> FieldInputCluster

                    FieldIntegerType ->
                        List.map (instance int) locators
                            |> FieldIntegerCluster
                            |> FieldInputCluster

                    FieldOnOffType ->
                        List.map (instance bool) locators
                            |> FieldOnOffCluster
                            |> FieldInputCluster

            FieldTypeSelectize fieldType ->
                case fieldType of
                    FieldAccountType ->
                        case fieldDescriptor.fieldCode.fieldClass of
                            Nothing ->
                                Debug.crash "No accountClass"

                            Just fieldClass ->
                                List.map
                                    (componentInstance string (initAccountSelectize configGroup.data fieldClass))
                                    locators
                                    |> FieldAccountCluster
                                    |> FieldSelectizeCluster

                    FieldCurrencyType ->
                        List.map
                            (componentInstance string (initCurrencySelectize configGroup.data))
                            locators
                            |> FieldCurrencyCluster
                            |> FieldSelectizeCluster

                    FieldLanguageType ->
                        List.map
                            (componentInstance (list string) (initLanguageSelectize configGroup.data))
                            locators
                            |> FieldLanguageCluster
                            |> FieldSelectizeCluster
