module InitFieldGroup exposing (init)

import ConfigTypes exposing (..)
import Selectize


initSelectizeFieldInstance : FieldLocator -> valueType -> FieldScope -> FieldInstance valueType Selectize.State
initSelectizeFieldInstance fieldLocator value fieldScope =
    { fieldScope = fieldLocator.fieldScope
    , fieldHolder = Ok (Just value)
    , loadedValue = Just value
    , component = Selectize.initialSelectize
    }


initSelectizeSuite : List FieldScope -> Field -> Suite SelectizeCluster
initSelectizeSuite fieldScopes field =
    let
        instances =
            case value of
                AccountValue v ->
                    List.map (initFieldInstance v) fieldScopes
                        |> AccountCluster
                        |> Just

                CurrencyValue v ->
                    List.map (initSelectizeFieldInstance field.fieldLocator v) fieldScopes
                        |> CurrencyCluster
                        |> Just

                LanguageValue v ->
                    List.map (initSelectizeFieldInstance field.fieldLocator v) fieldScopes
                        |> LanguageCluster
                        |> Just

                _ ->
                    Nothing
    in
        instances


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

initInstance : List FieldScope -> List Field -> InputCluster
initInstance fieldScopes fields =

initInputSuite : List FieldScope -> List Field -> FieldDescriptor -> Suite InputCluster
initInputSuite fieldScopes fields fieldDescriptor =
    let
        cluster = case fieldDescriptor.fieldType of
            StringType ->
                StringCluster List.map (initInstance fields) fieldScopes
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
