module ConfigGroup exposing (Msg, update, view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (defaultValue)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (string2Crypto)
import List


type alias Model =
    ConfigGroup



-- UPDATE


type Msg
    = Input Crypto Machine String String


updateField : Crypto -> Machine -> String -> String -> Model -> Field
updateField crypto machine fieldCode fieldValueString model =
    let
        maybeCryptoConfig =
            List.filter (isCrypto crypto) model.cryptoConfigs
                |> List.head

        machineMapper cryptoConfig = List.filter (isMachine machine) cryptoConfig.machineConfigs

        maybeMachineConfig = Maybe.map machineMapper maybeCryptoConfig

        fieldMapper machineConfig = List.filter (isField fieldCode) machineConfig.fieldSet

        maybeField = Maybe.map fieldMapper maybeMachineConfig



    if .code field == fieldCode then
        { field | value = updateFieldValue fieldValueString field.value }
    else
        field


updateFieldSet : String -> String -> FieldSet -> FieldSet
updateFieldSet fieldCode fieldValueString fieldSet =
    let
        fields =
            fieldSet.fields

        updatedFields =
            List.map (updateField fieldCode fieldValueString) fields
    in
        { fieldSet | fields = updatedFields }


updateMachineConfig : Machine -> String -> String -> MachineConfig -> MachineConfig
updateMachineConfig machine fieldCode fieldValueString machineConfig =
    if machineConfig.machine == machine then
        { machineConfig | fieldSet = updateFieldSet fieldCode fieldValueString machineConfig.fieldSet }
    else
        machineConfig


updateMachineConfigs : Machine -> String -> String -> List MachineConfig -> List MachineConfig
updateMachineConfigs machine fieldCode fieldValueString machineConfigs =
    List.map (updateMachineConfig machine fieldCode fieldValueString) machineConfigs


updateCryptoConfig : Crypto -> Machine -> String -> String -> CryptoConfig -> CryptoConfig
updateCryptoConfig crypto machine fieldCode fieldValueString cryptoConfig =
    if cryptoConfig.crypto == crypto then
        { cryptoConfig | machineConfigs = updateMachineConfigs machine fieldCode fieldValueString cryptoConfig.machineConfigs }
    else
        cryptoConfig


updateCryptoConfigs : Crypto -> Machine -> String -> String -> List CryptoConfig -> List CryptoConfig
updateCryptoConfigs crypto machine fieldCode fieldValueString cryptoConfigs =
    List.map (updateCryptoConfig crypto machine fieldCode fieldValueString) cryptoConfigs


updateConfigGroup : Crypto -> Machine -> String -> String -> ConfigGroup -> ConfigGroup
updateConfigGroup crypto machine fieldCode fieldValueString configGroup =
    { configGroup | cryptoConfigs = updateCryptoConfigs crypto machine fieldCode fieldValueString configGroup.cryptoConfigs }


update : Msg -> Model -> ( Model, Cmd Msg )
update (Input crypto machine fieldCode valueString) model =
    updateConfigGroup crypto machine fieldCode valueString model ! []



-- View


maybeToString : Maybe x -> String
maybeToString maybe =
    case maybe of
        Nothing ->
            ""

        Just x ->
            Debug.log "DEBUG20" (toString x)


fieldComponent : Crypto -> Machine -> Field -> Html Msg
fieldComponent crypto machine field =
    case field.value of
        FieldString _ ->
            input
                [ onInput (Input crypto machine field.code) ]
                []

        FieldPercentage maybeVal ->
            input
                [ onInput (Input crypto machine field.code)
                , defaultValue (maybeToString maybeVal)
                ]
                []

        FieldInteger _ ->
            input
                [ onInput (Input crypto machine field.code) ]
                []


cellView : Crypto -> Machine -> Field -> Html Msg
cellView crypto machine field =
    td [] [ fieldComponent crypto machine field ]


rowView : Crypto -> Model -> Html Msg
rowView crypto machineConfig =
    let
        cells =
            List.map (cellView crypto machineConfig.machine) machineConfig.fieldSet.fields
    in
        tr [] cells


headerCell : Field -> Html Msg
headerCell field =
    td [] [ text field.display ]


headerRowView : Maybe MachineConfig -> Html Msg
headerRowView maybeMachineConfig =
    let
        cells =
            case maybeMachineConfig of
                Nothing ->
                    [ td [] [ text "Error: No headers" ] ]

                Just machineConfig ->
                    List.map headerCell machineConfig.fieldSet.fields
    in
        tr [] cells


machineIds : Crypto -> Model -> Maybe (List Machine)
machineIds crypto model =
    let
        maybeCryptoConfig =
            List.filter (isCrypto crypto) model.cryptoConfigs
                |> List.head

        mapper cryptoConfig =
            List.map .machine cryptoConfig.machineConfigs

        maybeMachineIds =
            Maybe.map mapper maybeCryptoConfig
    in
        Maybe.map mapper maybeCryptoConfig


tableView : Crypto -> Model -> Html Msg
tableView crypto model =
    let
        headerRow =
            headerRowView crypto model

        rows =
            List.map (rowView crypto model) (machineIds crypto model)
    in
        table []
            [ thead [] [ headerRow ]
            , tbody [] rows
            ]


isCrypto : Crypto -> CryptoConfig -> Bool
isCrypto crypto cryptoConfig =
    (Debug.log "DEBUG12" cryptoConfig.crypto) == (Debug.log "DEBUG13" crypto)


view : Model -> String -> Html Msg
view model cryptoCode =
    let
        crypto =
            string2Crypto cryptoCode
    in
        tableView model crypto
