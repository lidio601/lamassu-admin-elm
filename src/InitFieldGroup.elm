module InitFieldGroup exposing (init)

import ConfigTypes exposing (..)
import Selectize


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


mapInputField : Field -> FieldInstance keyValue () -> FieldInstance keyValue ()
mapInputField field fieldInstance =
    if fieldInstance.fieldScope == field.fieldLocator.fieldScope then
        case field.fieldValue of
            Nothing ->
                fieldInstance

            Just fieldValue ->
                case fieldValue of
                    StringValue v ->
                        initValueInstance () field.fieldLocator.fieldScope v
    else
        fieldInstance


mapInputCluster : InputCluster -> Field -> InputCluster
mapInputCluster cluster field =
    case cluster of
        StringCluster instances ->
            List.map (mapInputField field) instances
                |> StringCluster


mapInputSuite : Suite InputCluster -> Field -> Suite InputCluster
mapInputSuite suite field =
    if suite.code == field.fieldDescriptor.fieldCode then
        { suite | cluster = mapInputCluster suite.cluster field }
    else
        suite


fieldDescriptorToField : FieldDescriptor -> FieldScope -> Field
fieldDescriptorToField fieldDescriptor fieldScope =
    let
        fieldLocator =
            { fieldScope = fieldScope
            , code = fieldDescriptor.code
            , fieldType = fieldDescriptor.fieldType
            , fieldClass = fieldDescriptor.fieldClass
            }
    in
        { fieldLocator = fieldLocator, fieldValue = Nothing }


mapField : Field -> Field -> Field
mapField emptyField newField =
    if emptyField.fieldLocator == newField.fieldLocator then
        newField
    else
        emptyField


mapFieldByFields : List Field -> Field -> Field
mapFieldByFields newFields emptyField =
    let
        maybeNewField =
            List.filter (((==) emptyField.fieldLocator) << .fieldLocator) newFields
                |> List.head
    in
        Maybe.withDefault emptyField maybeNewField


mapFields : List Field -> List Field -> List Field
mapFields emptyFields newFields =
    List.map (mapFieldByFields newFields) emptyFields


initInputSuite : List FieldScope -> FieldDescriptor -> Suite InputCluster
initInputSuite fieldScopes fieldDescriptor =
    let
        cluster =
            case fieldDescriptor.fieldType of
                StringType ->
                    StringCluster (List.map (initInstance ()) fieldScopes)
    in
        { code = fieldDescriptor.code, cluster = cluster }


initSelectizeSuite : List FieldScope -> FieldDescriptor -> Suite SelectizeCluster
initSelectizeSuite fieldScopes fieldDescriptor =
    let
        cluster =
            case fieldDescriptor.fieldType of
                AccountType ->
                    AccountCluster (List.map (initInstance Selectize.initialSelectize) fieldScopes)
    in
        { code = fieldDescriptor.code, cluster = cluster }


emptyFields : ConfigGroup -> List Field
emptyFields configGroup =
    let
        scopes =
            fieldScopes configGroup

        byDescriptor fieldDescriptor =
            List.map (fieldDescriptorToField fieldDescriptor) scopes
    in
        List.concatMap byDescriptor configGroup.schema.fieldDescriptors

fieldToInstance
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
