module ConfigFieldSet exposing (Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import FieldSetTypes exposing (..)
import ConfigTypes exposing (..)
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
    let
        newConfigGroup =
            updateConfigGroup crypto machine fieldCode valueString model
    in
        newConfigGroup ! []


fieldComponent : Field -> Html Msg
fieldComponent field =
    case field.value of
        FieldString string ->
            label []
                [ text field.display
                , input
                    [ onInput (Input field.code), value string ]
                    []
                ]

        FieldPassword _ ->
            label []
                [ text field.display
                , input
                    [ onInput (Input field.code), type' "password" ]
                    []
                ]


fieldView : Field -> Html Msg
fieldView field =
    div [] [ fieldComponent field ]


view : Model -> Html Msg
view model =
    let
        fields =
            List.map fieldView model.fields
    in
        div []
            [ fieldset [] fields
            ]
