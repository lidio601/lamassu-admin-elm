module FlattenFieldGroup exposing (flatten)

import ConfigTypes exposing (..)


flattenSelectizeInstance : (valueType -> FieldValue) -> SelectizeFieldInstance valueType -> Maybe Field
flattenSelectizeInstance tagger instance =
    let
        maybeFieldValue =
            case instance.fieldHolder of
                Ok maybeValue ->
                    if maybeValue == instance.loadedValue then
                        Nothing
                    else
                        Just maybeValue

                Err _ ->
                    Nothing

        toField fieldValue =
            { fieldLocator = instance.fieldLocator
            , fieldValue = Maybe.map tagger fieldValue
            }
    in
        Maybe.map toField maybeFieldValue


flattenInputInstance : (valueType -> FieldValue) -> InputFieldInstance valueType -> Maybe Field
flattenInputInstance tagger instance =
    let
        maybeFieldValue =
            case instance.fieldHolder of
                Ok maybeValue ->
                    if maybeValue == instance.loadedValue then
                        Nothing
                    else
                        Just maybeValue

                Err _ ->
                    Nothing

        toField fieldValue =
            { fieldLocator = instance.fieldLocator
            , fieldValue = Maybe.map tagger fieldValue
            }
    in
        Maybe.map toField maybeFieldValue


flattenSelectizeCluster : SelectizeCluster -> List Field
flattenSelectizeCluster cluster =
    case cluster of
        AccountCluster instances ->
            List.filterMap (flattenSelectizeInstance AccountValue) instances

        CurrencyCluster instances ->
            List.filterMap (flattenSelectizeInstance CurrencyValue) instances

        LanguageCluster instances ->
            List.filterMap (flattenSelectizeInstance LanguageValue) instances


flattenInputCluster : InputCluster -> List Field
flattenInputCluster cluster =
    case cluster of
        StringCluster instances ->
            List.filterMap (flattenInputInstance StringValue) instances

        PercentageCluster instances ->
            List.filterMap (flattenInputInstance PercentageValue) instances

        IntegerCluster instances ->
            List.filterMap (flattenInputInstance IntegerValue) instances

        OnOffCluster instances ->
            List.filterMap (flattenInputInstance OnOffValue) instances


flatten : FieldGroup -> List Field
flatten fieldGroup =
    let
        selectizeFields =
            List.concatMap flattenSelectizeCluster fieldGroup.selectize

        inputFields =
            List.concatMap flattenInputCluster fieldGroup.input
    in
        List.append inputFields selectizeFields
