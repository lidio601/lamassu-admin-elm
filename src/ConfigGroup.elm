module ConfigGroup exposing (Msg, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (defaultValue, placeholder)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (stringToCrypto)
import List
import Maybe exposing (oneOf)
import String


type alias Model =
    ConfigGroup



-- UPDATE


type Msg
    = Input Crypto Machine String String


isOfFieldClass : Crypto -> Machine -> String -> Field -> Bool
isOfFieldClass crypto machine fieldCode field =
    field.crypto
        == crypto
        && field.machine
        == machine
        && field.code
        == fieldCode


isNotOfFieldClass crypto machine fieldCode field =
    not (isOfFieldClass crypto machine fieldCode field)


isFieldClass : Field -> Field -> Bool
isFieldClass searchField field =
    isOfFieldClass searchField.crypto searchField.machine searchField.code field


isNotFieldClass : Field -> Field -> Bool
isNotFieldClass searchField field =
    not (isFieldClass searchField field)


placeField : List Field -> Field -> List Field
placeField fieldList field =
    field :: (List.filter (isNotFieldClass field) fieldList)


removeField : List Field -> Crypto -> Machine -> String -> List Field
removeField fieldList crypto machine fieldCode =
    List.filter (isNotOfFieldClass crypto machine fieldCode) fieldList


updateValues : Model -> Crypto -> Machine -> String -> String -> Model
updateValues model crypto machine fieldCode valueString =
    if (String.isEmpty valueString) then
        { model | values = removeField model.values crypto machine fieldCode }
    else
        let
            maybeFieldDescriptor =
                List.filter (\fd -> fd.code == fieldCode) model.schema.entries
                    |> List.head
        in
            case maybeFieldDescriptor of
                Just fieldDescriptor ->
                    let
                        fieldValueResult =
                            stringToFieldValue fieldDescriptor.fieldType valueString
                    in
                        case fieldValueResult of
                            Err _ ->
                                model

                            Ok fieldValue ->
                                let
                                    field =
                                        { code = fieldCode
                                        , crypto = crypto
                                        , machine = machine
                                        , fieldValue = fieldValue
                                        , loadedFieldValue = Nothing
                                        }

                                    values =
                                        placeField model.values field
                                in
                                    { model | values = values }

                Nothing ->
                    model


update : Msg -> Model -> ( Model, Cmd Msg )
update (Input crypto machine fieldCode valueString) model =
    let
        newModel =
            updateValues model crypto machine fieldCode valueString
    in
        (Debug.log "DEBUG22" newModel) ! []



-- View


fieldInput : Crypto -> Machine -> FieldDescriptor -> String -> String -> Html Msg
fieldInput crypto machine fieldDescriptor defaultString placeholderString =
    case fieldDescriptor.fieldType of
        FieldStringType ->
            input
                [ onInput (Input crypto machine fieldDescriptor.code) ]
                []

        FieldPercentageType ->
            input
                [ onInput (Input crypto machine fieldDescriptor.code)
                , defaultValue defaultString
                , placeholder placeholderString
                ]
                []

        FieldIntegerType ->
            input
                [ onInput (Input crypto machine fieldDescriptor.code) ]
                []


pickField : List Field -> Crypto -> Machine -> String -> Maybe Field
pickField fields crypto machine fieldCode =
    List.filter (isOfFieldClass crypto machine fieldCode) fields
        |> List.head


fieldComponent : Model -> Crypto -> Machine -> FieldDescriptor -> Html Msg
fieldComponent model crypto machine fieldDescriptor =
    let
        fieldCode =
            fieldDescriptor.code

        fieldType =
            fieldDescriptor.fieldType

        fields =
            model.values

        maybeGlobal =
            pickField fields GlobalCrypto GlobalMachine fieldCode

        maybeGlobalCrypto =
            pickField fields GlobalCrypto machine fieldCode

        maybeGlobalMachine =
            pickField fields crypto GlobalMachine fieldCode

        maybeSpecific =
            pickField fields crypto machine fieldCode

        maybeSpecificString =
            Maybe.map fieldToString maybeSpecific

        maybeFallbackField =
            oneOf [ maybeSpecific, maybeGlobalMachine, maybeGlobalCrypto, maybeGlobal ]

        maybeFallbackString =
            Maybe.map fieldToString maybeFallbackField

        defaultString =
            Maybe.withDefault "" maybeSpecificString

        fallbackString =
            Maybe.withDefault "" maybeFallbackString
    in
        fieldInput crypto machine fieldDescriptor defaultString fallbackString


cellView : Model -> Crypto -> Machine -> FieldDescriptor -> Html Msg
cellView model crypto machine fieldDescriptor =
    td [] [ fieldComponent model crypto machine fieldDescriptor ]


rowView : Model -> Crypto -> Machine -> Html Msg
rowView model crypto machine =
    tr [] (List.map (cellView model crypto machine) model.schema.entries)


headerCellView : FieldDescriptor -> Html Msg
headerCellView fieldDescriptor =
    td [] [ text fieldDescriptor.display ]


headerRowView : Model -> Crypto -> Html Msg
headerRowView model crypto =
    tr [] (List.map headerCellView model.schema.entries)


tableView : Model -> Crypto -> Html Msg
tableView model crypto =
    let
        headerRow =
            headerRowView model crypto

        machines =
            if (model.schema.machineScope == Specific) then
                model.data.machines
            else
                GlobalMachine :: model.data.machines

        rows =
            List.map (rowView model crypto) machines
    in
        table []
            [ thead [] [ headerRow ]
            , tbody [] rows
            ]


isField : String -> Field -> Bool
isField fieldCode field =
    field.code == fieldCode


view : Model -> String -> Html Msg
view model cryptoCode =
    let
        crypto =
            stringToCrypto cryptoCode
    in
        tableView model crypto
