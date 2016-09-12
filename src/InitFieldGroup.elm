module InitFieldGroup exposing (init)

import ConfigTypes exposing (..)
import Selectize


-- initSelectizeFieldInstance : FieldLocator -> valueType -> FieldScope -> FieldInstance valueType Selectize.State
-- initSelectizeFieldInstance fieldLocator value fieldScope =
--     { fieldScope = fieldLocator.fieldScope
--     , fieldHolder = Ok (Just value)
--     , loadedValue = Just value
--     , component = Selectize.initialSelectize
--     }
--
--
-- initSelectizeSuite : List FieldScope -> Field -> Suite SelectizeCluster
-- initSelectizeSuite fieldScopes field =
--     let
--         instances =
--             case value of
--                 AccountValue v ->
--                     List.map (initFieldInstance v) fieldScopes
--                         |> AccountCluster
--                         |> Just
--
--                 CurrencyValue v ->
--                     List.map (initSelectizeFieldInstance field.fieldLocator v) fieldScopes
--                         |> CurrencyCluster
--                         |> Just
--
--                 LanguageValue v ->
--                     List.map (initSelectizeFieldInstance field.fieldLocator v) fieldScopes
--                         |> LanguageCluster
--                         |> Just
--
--                 _ ->
--                     Nothing
--     in
--         instances


initInputFieldInstance : FieldLocator -> valueType -> FieldScope -> FieldInstance valueType ()
initInputFieldInstance fieldLocator value fieldScope =
    { fieldSCope = fieldLocator.fieldScope
    , fieldHolder = Ok (Just value)
    , loadedValue = Just value
    , component = ()
    }


initInputCluster : List FieldScope -> Field -> Maybe InputCluster
initInputCluster fieldScopes field =
    case field.fieldValue of
        Nothing ->
            Nothing

        Just value ->
            case value of
                StringValue v ->
                    List.map (initInputFieldInstance field.fieldLocator v) fieldScopes
                        |> StringCluster
                        |> Just

                PercentageValue v ->
                    List.map (initInputFieldInstance field.fieldLocator v) fieldScopes
                        |> PercentageCluster
                        |> Just

                IntegerValue v ->
                    List.map (initInputFieldInstance field.fieldLocator v) fieldScopes
                        |> IntegerCluster
                        |> Just

                OnOffValue v ->
                    List.map (initInputFieldInstance field.fieldLocator v) fieldScopes
                        |> OnOffCluster
                        |> Just

                _ ->
                    Nothing


initInstance : componentType -> FieldScope -> FieldInstance keyValue componentType
initInstance component fieldScope =
    { fieldScope = fieldScope
    , fieldHolder = Ok Nothing
    , loadedValue = Nothing
    , component = component
    }


initValueInstance : componentType -> FieldScope -> valueType -> FieldInstance keyValue componentType
initValueInstance component fieldScope value =
    { fieldScope = fieldScope
    , fieldHolder = Ok (Just value)
    , loadedValue = Just value
    , component = component
    }


mapField : Field -> FieldInstance keyValue componentType -> FieldInstance keyValue componentType
mapField field fieldInstance =
    if fieldInstance.fieldScope == field.fieldLocator.fieldScope then
        case field.fieldValue of
            StringValue v ->
                initValueInstance () field.fieldLocator.fieldScope v
    else
        fieldInstance


mapInputCluster : InputCluster -> Field -> InputCluster
mapInputCluster cluster field =
    case cluster of
        StringCluster instances ->
            List.map (mapField field) instances
                |> StringCluster


mapSuite : Suite clusterType -> Field -> Suite clusterType
mapSuite suite field =
    if suite.code == field.fieldDescriptor.fieldCode then
        { suite | cluster = mapInputCluster suite.cluster field }
    else
        suite


initInputSuite : List FieldScope -> List Field -> FieldDescriptor -> Suite InputCluster
initInputSuite fieldScopes fields fieldDescriptor =
    let
        cluster =
            case fieldDescriptor.fieldType of
                StringType ->
                    StringCluster List.map (initInstance () fields) fieldScopes
    in
        { code = fieldDescriptor.fieldCode, cluster = cluster }


init : ConfigGroup -> FieldGroup
init configGroup =
    let
        scopes =
            fieldScopes configGroup

        fieldDescriptors =
            configGroup.schema.fieldDescriptors

        selectize =
            List.map (initSelectizeSuite scopes) fieldDescriptors

        input =
            List.map (initInputSuite scopes) fieldDescriptors
    in
        { selectize = selectize
        , input = input
        }
