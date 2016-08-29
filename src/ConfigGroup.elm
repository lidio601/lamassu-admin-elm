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


updateField : String -> String -> Field -> Field
updateField fieldCode fieldValueString field =
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


rowView : Crypto -> MachineConfig -> Html Msg
rowView crypto machineConfig =
    let
        cells =
            List.map (cellView crypto machineConfig.machine) machineConfig.fieldSet.fields
    in
        tr [] cells


tableView : CryptoConfig -> Html Msg
tableView cryptoConfig =
    let
        rows =
            List.map (rowView cryptoConfig.crypto) cryptoConfig.machineConfigs
    in
        table []
            [ body [] rows ]


isCrypto : Crypto -> CryptoConfig -> Bool
isCrypto crypto cryptoConfig =
    (Debug.log "DEBUG12" cryptoConfig.crypto) == (Debug.log "DEBUG13" crypto)


view : Model -> String -> Html Msg
view model cryptoCode =
    let
        crypto =
            string2Crypto cryptoCode

        maybeCryptoConfig =
            List.filter (isCrypto crypto) (Debug.log "DEBUG14" model.cryptoConfigs)
                |> List.head
    in
        case maybeCryptoConfig of
            Just cryptoConfig ->
                tableView cryptoConfig

            Nothing ->
                div [] [ text "No such cryptocurrency" ]
