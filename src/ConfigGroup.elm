module ConfigGroup exposing (Msg, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (defaultValue, placeholder)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (stringToCrypto)
import List
import String


type alias Model =
    ConfigGroup



-- UPDATE


type Msg
    = Input Crypto Machine String String


updateField : FieldSet -> String -> String -> Field -> Maybe Field
updateField currentFieldSet newFieldCode fieldValueString currentTemplateField =
    let
        maybeNewField =
            List.filter (\f -> f.code == currentTemplateField.code) currentFieldSet.fields
                |> List.head
    in
        case maybeNewField of
            Nothing ->
                if (currentTemplateField.code == newFieldCode && not (String.isEmpty fieldValueString)) then
                    Just { currentTemplateField | value = updateFieldValue fieldValueString currentTemplateField.value, loadedValue = NoFieldValue }
                else
                    Nothing

            Just newField ->
                if (newField.code == newFieldCode) then
                    if (String.isEmpty fieldValueString) then
                        Nothing
                    else
                        Just { newField | value = updateFieldValue fieldValueString newField.value }
                else
                    Just newField


isNothing : Maybe x -> Bool
isNothing maybe =
    case maybe of
        Nothing ->
            True

        Just _ ->
            False


updateFieldSet : Model -> String -> String -> FieldSet -> FieldSet
updateFieldSet model fieldCode fieldValueString fieldSet =
    let
        maybeFieldSetTemplate =
            maybePickFieldSet model

        updatedFields =
            case maybeFieldSetTemplate of
                Nothing ->
                    fieldSet.fields

                Just fieldSetTemplate ->
                    List.filterMap (updateField fieldSet fieldCode fieldValueString) fieldSetTemplate.fields
    in
        { fieldSet | fields = updatedFields }


updateMachineConfig : Model -> Machine -> String -> String -> MachineConfig -> MachineConfig
updateMachineConfig model machine fieldCode fieldValueString machineConfig =
    if machineConfig.machine == machine then
        { machineConfig | fieldSet = updateFieldSet model fieldCode fieldValueString machineConfig.fieldSet }
    else
        machineConfig


updateMachineConfigs : Model -> Machine -> String -> String -> List MachineConfig -> List MachineConfig
updateMachineConfigs model machine fieldCode fieldValueString machineConfigs =
    List.map (updateMachineConfig model machine fieldCode fieldValueString) machineConfigs


updateCryptoConfig : Model -> Crypto -> Machine -> String -> String -> CryptoConfig -> CryptoConfig
updateCryptoConfig model crypto machine fieldCode fieldValueString cryptoConfig =
    if cryptoConfig.crypto == crypto then
        { cryptoConfig | machineConfigs = updateMachineConfigs model machine fieldCode fieldValueString cryptoConfig.machineConfigs }
    else
        cryptoConfig


updateCryptoConfigs : Model -> Crypto -> Machine -> String -> String -> List CryptoConfig
updateCryptoConfigs model crypto machine fieldCode fieldValueString =
    List.map (updateCryptoConfig model crypto machine fieldCode fieldValueString) model.cryptoConfigs


updateConfigGroup : Model -> Crypto -> Machine -> String -> String -> Model
updateConfigGroup model crypto machine fieldCode fieldValueString =
    { model | cryptoConfigs = updateCryptoConfigs model crypto machine fieldCode fieldValueString }


update : Msg -> Model -> ( Model, Cmd Msg )
update (Input crypto machine fieldCode valueString) model =
    let
        newModel =
            updateConfigGroup model crypto machine fieldCode valueString
    in
        (Debug.log "DEBUG22" newModel) ! []



-- View


maybePickMachineField : String -> MachineConfig -> Maybe Field
maybePickMachineField fieldCode machineConfig =
    List.filter (isField fieldCode) machineConfig.fieldSet.fields
        |> List.head


maybePickMachine : Machine -> Maybe CryptoConfig -> Maybe MachineConfig
maybePickMachine machine maybeCryptoConfig =
    let
        machineMapper cryptoConfig =
            List.filter (isMachine machine) cryptoConfig.machineConfigs
                |> List.head
    in
        Maybe.andThen maybeCryptoConfig machineMapper


fieldInput : Crypto -> Machine -> Field -> String -> String -> Html Msg
fieldInput crypto machine field defaultString placeholderString =
    case field.value of
        FieldString _ ->
            input
                [ onInput (Input crypto machine field.code) ]
                []

        FieldPercentage maybeVal ->
            input
                [ onInput (Input crypto machine field.code)
                , defaultValue defaultString
                , placeholder placeholderString
                ]
                []

        FieldInteger _ ->
            input
                [ onInput (Input crypto machine field.code) ]
                []

        NoFieldValue ->
            text "Error af763fa4-6eb8-11e6-93cb-df54e365bb1b"


fieldComponent : Crypto -> Machine -> Model -> String -> Html Msg
fieldComponent crypto machine model fieldCode =
    let
        maybeCryptoConfig =
            List.filter (isCrypto crypto) model.cryptoConfigs
                |> List.head

        maybeGlobalCryptoConfig =
            List.filter (isCrypto GlobalCrypto) model.cryptoConfigs
                |> List.head

        maybeField =
            maybePickMachine machine maybeCryptoConfig
                `Maybe.andThen` (maybePickMachineField fieldCode)

        maybeCryptoField =
            maybePickMachine GlobalMachine maybeCryptoConfig
                `Maybe.andThen` (maybePickMachineField fieldCode)

        maybeMachineField =
            maybePickMachine machine maybeGlobalCryptoConfig
                `Maybe.andThen` (maybePickMachineField fieldCode)

        maybeGlobalField =
            maybePickMachine GlobalMachine maybeGlobalCryptoConfig
                `Maybe.andThen` (maybePickMachineField fieldCode)

        maybeFallbackField =
            Maybe.oneOf [ maybeField, maybeCryptoField, maybeMachineField, maybeGlobalField ]

        fallback =
            case maybeFallbackField of
                Nothing ->
                    ""

                Just fallbackField ->
                    fieldToString fallbackField
    in
        case maybeField of
            Nothing ->
                case maybeFallbackField of
                    Nothing ->
                        div [] [ text "Error 9d2cdbae-6e96-11e6-80ee-3f4d113632d0" ]

                    Just field ->
                        fieldInput crypto machine field "" fallback

            Just field ->
                fieldInput crypto machine field (fieldToString field) fallback


cellView : Model -> Crypto -> Machine -> String -> Html Msg
cellView model crypto machine fieldCode =
    td [] [ fieldComponent crypto machine model fieldCode ]


maybePickFieldSet : Model -> Maybe FieldSet
maybePickFieldSet model =
    let
        maybeMachineConfigs =
            List.head model.cryptoConfigs
                |> Maybe.map .machineConfigs
    in
        maybeMachineConfigs
            `Maybe.andThen` List.head
            |> Maybe.map .fieldSet


maybePickFieldCodes : Model -> Maybe (List String)
maybePickFieldCodes model =
    maybePickFieldSet model
        |> Maybe.map .fields
        |> Maybe.map (List.map .code)


rowView : Model -> Crypto -> Machine -> Html Msg
rowView model crypto machine =
    case (maybePickFieldCodes model) of
        Nothing ->
            tr [] [ text "Error bd646b26-6ea0-11e6-b4ff-8f79726e6904" ]

        Just fieldCodes ->
            tr [] (List.map (cellView model crypto machine) fieldCodes)


headerCell : Field -> Html Msg
headerCell field =
    td [] [ text field.display ]


headerRowView : Crypto -> Model -> Html Msg
headerRowView crypto model =
    let
        maybeCryptoConfig =
            List.head model.cryptoConfigs

        maybeMachineConfig =
            Maybe.andThen maybeCryptoConfig (\cryptoConfig -> List.head cryptoConfig.machineConfigs)

        cells =
            case maybeMachineConfig of
                Nothing ->
                    [ td [] [ text "Error: 8fd366a0-6e99-11e6-8fb9-df73b88e8b22" ] ]

                Just machineConfig ->
                    List.map headerCell machineConfig.fieldSet.fields
    in
        tr [] cells


tableView : Crypto -> Model -> Html Msg
tableView crypto model =
    let
        headerRow =
            headerRowView crypto model

        machines =
            if (model.machineScope == Specific) then
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


isCrypto : Crypto -> CryptoConfig -> Bool
isCrypto crypto cryptoConfig =
    cryptoConfig.crypto == crypto


isMachine : Machine -> MachineConfig -> Bool
isMachine machine machineConfig =
    machineConfig.machine == machine


isField : String -> Field -> Bool
isField fieldCode field =
    field.code == fieldCode


view : Model -> String -> Html Msg
view model cryptoCode =
    let
        crypto =
            stringToCrypto cryptoCode
    in
        tableView crypto model
