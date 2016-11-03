module Config exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (defaultValue, placeholder, type')
import Html.Keyed
import Navigation
import Hop
import Hop.Types
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (..)
import ConfigEncoder exposing (..)
import Css.Admin exposing (..)
import Css.Classes as C
import Selectize
import Maybe exposing (oneOf)
import FuzzyMatch
import SelectizeHelper exposing (buildConfig)


hopConfig : Hop.Types.Config
hopConfig =
    { hash = True
    , basePath = ""
    }


type alias ConfigGroupResponse =
    RemoteData (Error String) (Response ConfigGroup)


type alias WebConfigGroup =
    RemoteData (Error String) ConfigGroup


type SavingStatus
    = Saving
    | Saved
    | Editing
    | NotSaving


type alias Model =
    { webConfigGroup : WebConfigGroup
    , fieldInstances : List FieldInstance
    , crypto : Maybe Crypto
    , status : SavingStatus
    , focused : Maybe FieldLocator
    }


type alias ResolvedModel =
    { configGroup : ConfigGroup
    , fieldInstances : List FieldInstance
    , crypto : Crypto
    , status : SavingStatus
    , focused : Maybe FieldLocator
    }


toResolvedModel : Model -> ConfigGroup -> ResolvedModel
toResolvedModel model configGroup =
    { configGroup = configGroup
    , fieldInstances = model.fieldInstances
    , crypto = Maybe.withDefault GlobalCrypto model.crypto
    , status = model.status
    , focused = model.focused
    }


getForm : String -> Cmd Msg
getForm code =
    get ("http://localhost:8093/config/" ++ code)
        |> send (jsonReader configGroupDecoder) stringReader
        |> RemoteData.asCmd
        |> Cmd.map Load


postForm : String -> List FieldInstance -> Cmd Msg
postForm configGroupCode fieldInstances =
    post "http://localhost:8093/config"
        |> withHeader "Content-Type" "application/json"
        |> withJsonBody (encodeResults configGroupCode fieldInstances)
        |> send (jsonReader configGroupDecoder) stringReader
        |> RemoteData.asCmd
        |> Cmd.map Load


init : Model
init =
    { webConfigGroup = RemoteData.NotAsked
    , fieldInstances = []
    , crypto = Nothing
    , status = NotSaving
    , focused = Nothing
    }


load : Model -> String -> Maybe String -> ( Model, Cmd Msg )
load model code maybeCryptoCodeString =
    let
        crypto =
            Maybe.map stringToCrypto maybeCryptoCodeString
    in
        ( { model | crypto = crypto }, getForm code )



-- UPDATE


similar : (x -> y) -> x -> x -> Bool
similar mapper a b =
    (==) (mapper a) (mapper b)


placeField : List Field -> Field -> List Field
placeField fieldList field =
    let
        maybeOldField =
            List.filter (similar .fieldLocator field) fieldList
                |> List.head

        newField =
            case maybeOldField of
                Nothing ->
                    field

                Just oldField ->
                    { oldField | fieldValue = field.fieldValue }
    in
        newField :: (List.filter (not << (similar .fieldLocator field)) fieldList)


fieldHolderToList : FieldHolder -> List String
fieldHolderToList fieldHolder =
    case fieldHolder of
        Err _ ->
            []

        Ok maybeValue ->
            case maybeValue of
                Nothing ->
                    []

                Just fieldValue ->
                    case fieldValue of
                        FieldLanguageValue v ->
                            v

                        FieldCryptoCurrencyValue v ->
                            v

                        _ ->
                            Debug.crash "Not a list type"


emptyToNothing : List x -> Maybe (List x)
emptyToNothing list =
    if (List.isEmpty list) then
        Nothing
    else
        Just list


updateStringFieldInstance : FieldLocator -> Maybe String -> FieldInstance -> FieldInstance
updateStringFieldInstance fieldLocator maybeString fieldInstance =
    if fieldInstance.fieldLocator == fieldLocator then
        case fieldLocator.fieldType of
            FieldLanguageType ->
                let
                    list =
                        fieldHolderToList fieldInstance.fieldValue

                    newList =
                        case maybeString of
                            Nothing ->
                                List.take ((List.length list) - 1) list

                            Just s ->
                                list ++ [ s ]
                in
                    { fieldInstance | fieldValue = Ok (Maybe.map FieldLanguageValue (emptyToNothing newList)) }

            FieldCryptoCurrencyType ->
                let
                    list =
                        fieldHolderToList fieldInstance.fieldValue

                    newList =
                        case maybeString of
                            Nothing ->
                                List.take ((List.length list) - 1) list

                            Just s ->
                                list ++ [ s ]
                in
                    { fieldInstance | fieldValue = Ok (Maybe.map FieldCryptoCurrencyValue (emptyToNothing newList)) }

            _ ->
                let
                    fieldHolder =
                        case maybeString of
                            Nothing ->
                                Ok Nothing

                            Just s ->
                                stringToFieldHolder fieldLocator.fieldType s
                in
                    { fieldInstance | fieldValue = fieldHolder }
    else
        fieldInstance


updateInput : FieldLocator -> Maybe String -> Model -> Model
updateInput fieldLocator maybeValueString model =
    let
        fieldInstances =
            List.map (updateStringFieldInstance fieldLocator maybeValueString) model.fieldInstances
    in
        { model | fieldInstances = fieldInstances }



-- View


textInput : FieldLocator -> Maybe FieldValue -> Maybe FieldValue -> Html Msg
textInput fieldLocator maybeFieldValue maybeFallbackFieldValue =
    let
        maybeSpecificString =
            Maybe.map fieldValueToString maybeFieldValue

        maybeFallbackString =
            Maybe.map fieldValueToString maybeFallbackFieldValue

        defaultString =
            Maybe.withDefault "" maybeSpecificString

        fallbackString =
            Maybe.withDefault "" maybeFallbackString
    in
        input
            [ onInput (Input fieldLocator)
            , onFocus (Focus fieldLocator)
            , onBlur (Blur fieldLocator)
            , defaultValue defaultString
            , placeholder fallbackString
            , class [ C.BasicInput ]
            ]
            []


type alias LocalConfig =
    SelectizeHelper.LocalConfig Msg String DisplayRec


accountSelectizeView :
    ResolvedModel
    -> LocalConfig
    -> FieldInstance
    -> Selectize.State
    -> Maybe FieldValue
    -> Maybe FieldValue
    -> Html Msg
accountSelectizeView model localConfig fieldInstance selectizeState maybeFieldValue maybeFallbackFieldValue =
    let
        specificConfig =
            { maxItems = 1
            , selectedDisplay = .display
            , optionDisplay = .display
            , match = FuzzyMatch.match
            }

        matchAccount accountRec =
            case fieldInstance.fieldLocator.fieldClass of
                Nothing ->
                    True

                Just fieldClass ->
                    (accountRec.class
                        == fieldClass
                    )
                        && (case accountRec.cryptos of
                                Nothing ->
                                    True

                                Just cryptos ->
                                    List.member model.crypto cryptos
                           )

        availableItems =
            List.filter matchAccount model.configGroup.data.accounts
                |> List.map accountRecToDisplayRec

        selectedIds =
            Maybe.map fieldValueToString maybeFieldValue
                |> maybeToList

        fallbackIds =
            Maybe.map fieldValueToString maybeFallbackFieldValue
                |> maybeToList

        _ =
            Debug.log "DEBUG26" ( fieldInstance.fieldLocator.fieldScope.machine, fallbackIds )
    in
        Selectize.view (buildConfig localConfig specificConfig)
            selectedIds
            availableItems
            fallbackIds
            selectizeState


currencySelectizeView :
    ResolvedModel
    -> LocalConfig
    -> FieldInstance
    -> Selectize.State
    -> Maybe FieldValue
    -> Maybe FieldValue
    -> Html Msg
currencySelectizeView model localConfig fieldInstance selectizeState maybeFieldValue maybeFallbackFieldValue =
    let
        specificConfig =
            { maxItems = 1
            , selectedDisplay = .code
            , optionDisplay = .display
            , match = FuzzyMatch.match
            }

        availableItems =
            model.configGroup.data.currencies

        selectedIds =
            Maybe.map fieldValueToString maybeFieldValue
                |> maybeToList

        fallbackIds =
            Maybe.map fieldValueToString maybeFallbackFieldValue
                |> maybeToList
    in
        Selectize.view (buildConfig localConfig specificConfig)
            selectedIds
            availableItems
            fallbackIds
            selectizeState


cryptoCurrencySelectizeView :
    ResolvedModel
    -> LocalConfig
    -> FieldInstance
    -> Selectize.State
    -> Maybe FieldValue
    -> Maybe FieldValue
    -> Html Msg
cryptoCurrencySelectizeView model localConfig fieldInstance selectizeState maybeFieldValue maybeFallbackFieldValue =
    let
        specificConfig =
            { maxItems = 5
            , selectedDisplay = .code
            , optionDisplay = .display
            , match = FuzzyMatch.match
            }

        toDisplay crypto =
            { code = cryptoToString crypto.crypto, display = crypto.display }

        availableItems =
            List.map toDisplay model.configGroup.data.cryptoCurrencies

        toList maybeValue =
            case maybeValue of
                Nothing ->
                    []

                Just fieldValue ->
                    case fieldValue of
                        FieldCryptoCurrencyValue list ->
                            list

                        _ ->
                            Debug.crash "Shouldn't be here"

        selectedIds =
            toList maybeFieldValue

        fallbackIds =
            toList maybeFallbackFieldValue
    in
        Selectize.view (buildConfig localConfig specificConfig)
            selectedIds
            availableItems
            fallbackIds
            selectizeState


languageSelectizeView :
    ResolvedModel
    -> LocalConfig
    -> FieldInstance
    -> Selectize.State
    -> Maybe FieldValue
    -> Maybe FieldValue
    -> Html Msg
languageSelectizeView model localConfig fieldInstance selectizeState maybeFieldValue maybeFallbackFieldValue =
    let
        specificConfig =
            { maxItems = 5
            , selectedDisplay = .code
            , optionDisplay = .display
            , match = FuzzyMatch.match
            }

        availableItems =
            model.configGroup.data.languages

        toList maybeValue =
            case maybeValue of
                Nothing ->
                    []

                Just fieldValue ->
                    case fieldValue of
                        FieldLanguageValue list ->
                            list

                        _ ->
                            Debug.crash "Shouldn't be here"

        selectedIds =
            toList maybeFieldValue

        fallbackIds =
            toList maybeFallbackFieldValue
    in
        Selectize.view (buildConfig localConfig specificConfig)
            selectedIds
            availableItems
            fallbackIds
            selectizeState


selectizeView :
    ResolvedModel
    -> FieldInstance
    -> Selectize.State
    -> Maybe FieldValue
    -> Maybe FieldValue
    -> Html Msg
selectizeView model fieldInstance selectizeState maybeFieldValue maybeFallbackFieldValue =
    let
        fieldLocator =
            fieldInstance.fieldLocator

        localConfig =
            { toMsg = SelectizeMsg fieldLocator
            , onAdd = Add fieldLocator
            , onRemove = Remove fieldLocator
            , onFocus = FocusSelectize fieldLocator
            , onBlur = BlurSelectize fieldLocator
            , toId = .code
            }
    in
        case fieldLocator.fieldType of
            FieldAccountType ->
                accountSelectizeView model
                    localConfig
                    fieldInstance
                    selectizeState
                    maybeFieldValue
                    maybeFallbackFieldValue

            FieldCurrencyType ->
                currencySelectizeView model
                    localConfig
                    fieldInstance
                    selectizeState
                    maybeFieldValue
                    maybeFallbackFieldValue

            FieldCryptoCurrencyType ->
                cryptoCurrencySelectizeView model
                    localConfig
                    fieldInstance
                    selectizeState
                    maybeFieldValue
                    maybeFallbackFieldValue

            FieldLanguageType ->
                languageSelectizeView model
                    localConfig
                    fieldInstance
                    selectizeState
                    maybeFieldValue
                    maybeFallbackFieldValue

            FieldOnOffType ->
                onOffSelectizeView model
                    localConfig
                    fieldInstance
                    selectizeState
                    maybeFieldValue
                    maybeFallbackFieldValue

            _ ->
                Debug.crash "Not a Selectize field"


onOffSelectizeView :
    ResolvedModel
    -> LocalConfig
    -> FieldInstance
    -> Selectize.State
    -> Maybe FieldValue
    -> Maybe FieldValue
    -> Html Msg
onOffSelectizeView model localConfig fieldInstance selectizeState maybeFieldValue maybeFallbackFieldValue =
    let
        specificConfig =
            { maxItems = 1
            , selectedDisplay = .display
            , optionDisplay = .display
            , match = FuzzyMatch.match
            }

        availableItems =
            [ { display = "On", code = "on" }, { display = "Off", code = "off" } ]

        selectedIds =
            Maybe.map fieldValueToString maybeFieldValue
                |> maybeToList

        fallbackIds =
            Maybe.map fieldValueToString maybeFallbackFieldValue
                |> maybeToList
    in
        Selectize.view (buildConfig localConfig specificConfig)
            selectedIds
            availableItems
            fallbackIds
            selectizeState


fieldInput : ResolvedModel -> FieldInstance -> Maybe FieldValue -> Maybe FieldValue -> Html Msg
fieldInput model fieldInstance maybeFieldValue maybeFallbackFieldValue =
    case fieldInstance.component of
        InputBoxComponent ->
            textInput fieldInstance.fieldLocator maybeFieldValue maybeFallbackFieldValue

        SelectizeComponent selectizeState ->
            selectizeView model fieldInstance selectizeState maybeFieldValue maybeFallbackFieldValue


fieldComponent : ResolvedModel -> FieldInstance -> Html Msg
fieldComponent model fieldInstance =
    let
        fieldLocator =
            fieldInstance.fieldLocator

        fieldScope =
            fieldLocator.fieldScope

        fieldCode =
            fieldLocator.code

        fieldClass =
            fieldLocator.fieldClass

        instances : List FieldInstance
        instances =
            model.fieldInstances

        fieldType =
            fieldLocator.fieldType

        pick =
            pickFieldInstanceValue fieldType fieldCode fieldClass instances

        maybeGlobal =
            pick GlobalCrypto GlobalMachine

        maybeGlobalCrypto =
            pick GlobalCrypto fieldScope.machine

        maybeGlobalMachine =
            pick fieldScope.crypto GlobalMachine

        maybeSpecific =
            case fieldInstance.fieldValue of
                Ok maybeFieldValue ->
                    maybeFieldValue

                _ ->
                    Nothing

        maybeFallbackFieldValue =
            oneOf [ maybeSpecific, maybeGlobalMachine, maybeGlobalCrypto, maybeGlobal ]
    in
        fieldInput model fieldInstance maybeSpecific maybeFallbackFieldValue


cellView : ResolvedModel -> FieldInstance -> Html Msg
cellView model fieldInstance =
    -- Note: keying here is needed to clear out fields when switching cryptos
    let
        fieldLocator =
            fieldInstance.fieldLocator

        fieldScope =
            fieldLocator.fieldScope

        machine =
            fieldScope.machine

        crypto =
            fieldScope.crypto

        focused =
            (Just fieldLocator) == model.focused
    in
        Html.Keyed.node "td"
            []
            [ ( (cryptoToString crypto)
                    ++ "-"
                    ++ (machineToString machine)
                    ++ "-"
                    ++ fieldLocator.code
              , div [ classList [ ( C.Component, True ), ( C.FocusedComponent, focused ) ] ]
                    [ fieldComponent model fieldInstance ]
              )
            ]


rowView : ResolvedModel -> List FieldInstance -> MachineDisplay -> Html Msg
rowView model fieldInstances machineDisplay =
    let
        machine =
            machineDisplay.machine

        globalRowClass =
            case machine of
                GlobalMachine ->
                    class [ C.ConfigTableGlobalRow ]

                _ ->
                    class []

        fieldScope =
            { crypto = model.crypto
            , machine = machineDisplay.machine
            }

        toFieldLocator entry =
            { fieldScope = fieldScope
            , code = entry.code
            }

        machineScoped fieldInstance =
            fieldInstance.fieldLocator.fieldScope.machine == machine

        filteredFieldInstances : List FieldInstance
        filteredFieldInstances =
            List.filter machineScoped fieldInstances
    in
        tr [ globalRowClass ]
            ((td [] [ text (machineDisplay.display) ])
                :: (List.map (cellView model)
                        filteredFieldInstances
                   )
            )


headerCellView : FieldDescriptor -> Html Msg
headerCellView fieldDescriptor =
    td [] [ text fieldDescriptor.display ]


headerRowView : ConfigGroup -> Crypto -> Html Msg
headerRowView configGroup crypto =
    tr [] ((td [] []) :: List.map headerCellView configGroup.schema.entries)


tableView : ResolvedModel -> Html Msg
tableView model =
    let
        configGroup =
            model.configGroup

        crypto =
            model.crypto

        headerRow =
            headerRowView configGroup crypto

        machines =
            listMachines configGroup

        cryptoScoped fieldInstance =
            fieldInstance.fieldLocator.fieldScope.crypto == crypto

        instances : List FieldInstance
        instances =
            List.filter cryptoScoped model.fieldInstances

        rows =
            List.map (rowView model instances) machines
    in
        table [ class [ C.ConfigTable ] ]
            [ thead [] [ headerRow ]
            , tbody [] rows
            ]


isField : String -> Field -> Bool
isField fieldCode field =
    field.fieldLocator.code == fieldCode


type Msg
    = Load ConfigGroupResponse
    | Submit
    | Input FieldLocator String
    | CryptoSwitch Crypto
    | SelectizeMsg FieldLocator Selectize.State
    | Blur FieldLocator
    | Focus FieldLocator
    | BlurSelectize FieldLocator Selectize.State
    | FocusSelectize FieldLocator Selectize.State
    | Add FieldLocator String Selectize.State
    | Remove FieldLocator Selectize.State


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Nothing ->
            []

        Just x ->
            [ x ]


buildFieldComponent : ConfigGroup -> FieldType -> FieldScope -> Maybe FieldValue -> FieldComponent
buildFieldComponent configGroup fieldType fieldScope fieldValue =
    case fieldType of
        FieldStringType ->
            InputBoxComponent

        FieldPercentageType ->
            InputBoxComponent

        FieldIntegerType ->
            InputBoxComponent

        FieldOnOffType ->
            SelectizeComponent Selectize.initialSelectize

        FieldAccountType ->
            SelectizeComponent Selectize.initialSelectize

        FieldCurrencyType ->
            SelectizeComponent Selectize.initialSelectize

        FieldCryptoCurrencyType ->
            SelectizeComponent Selectize.initialSelectize

        FieldLanguageType ->
            SelectizeComponent Selectize.initialSelectize


initFieldInstance : ConfigGroup -> FieldDescriptor -> FieldScope -> FieldInstance
initFieldInstance configGroup fieldDescriptor fieldScope =
    let
        fieldLocator : FieldLocator
        fieldLocator =
            { fieldScope = fieldScope
            , code = fieldDescriptor.code
            , fieldType = fieldDescriptor.fieldType
            , fieldClass = fieldDescriptor.fieldClass
            }

        value =
            List.filter (((==) fieldLocator) << .fieldLocator) configGroup.values
                |> List.head
                |> Maybe.map .fieldValue

        component =
            buildFieldComponent configGroup fieldDescriptor.fieldType fieldScope value
    in
        { fieldLocator = fieldLocator
        , component = component
        , fieldValue = Ok value
        , loadedFieldValue = value
        }


initFieldInstancesPerEntry : ConfigGroup -> FieldDescriptor -> List FieldInstance
initFieldInstancesPerEntry configGroup fieldDescriptor =
    List.map (initFieldInstance configGroup fieldDescriptor) (fieldScopes configGroup)


initFieldInstances : ConfigGroup -> List FieldInstance
initFieldInstances configGroup =
    List.concatMap (initFieldInstancesPerEntry configGroup) configGroup.schema.entries


pickFieldInstance : FieldLocator -> List FieldInstance -> Maybe FieldInstance
pickFieldInstance fieldLocator fieldInstances =
    let
        sameLocation targetFieldLocator fieldInstance =
            fieldInstance.fieldLocator == targetFieldLocator
    in
        List.filter (sameLocation fieldLocator) fieldInstances
            |> List.head


fieldInstanceToMaybeFieldValue : FieldInstance -> Maybe FieldValue
fieldInstanceToMaybeFieldValue fieldInstance =
    case fieldInstance.fieldValue of
        Ok maybeFieldValue ->
            maybeFieldValue

        _ ->
            Nothing


pickFieldInstanceValue : FieldType -> String -> Maybe String -> List FieldInstance -> Crypto -> Machine -> Maybe FieldValue
pickFieldInstanceValue fieldType fieldCode fieldClass fieldInstances crypto machine =
    let
        fieldScope =
            { crypto = crypto, machine = machine }

        fieldLocator : FieldLocator
        fieldLocator =
            { fieldScope = fieldScope
            , code = fieldCode
            , fieldType = fieldType
            , fieldClass = fieldClass
            }
    in
        (pickFieldInstance fieldLocator fieldInstances)
            `Maybe.andThen` fieldInstanceToMaybeFieldValue


updateFocus : FieldLocator -> Bool -> Model -> Model
updateFocus fieldLocator focused model =
    if focused then
        { model | focused = Just fieldLocator }
    else if model.focused == Just fieldLocator then
        { model | focused = Nothing }
    else
        model


updateSelectize : FieldLocator -> Selectize.State -> Model -> Model
updateSelectize fieldLocator state model =
    let
        fieldInstances =
            model.fieldInstances

        updateInstance fieldInstance =
            if (fieldInstance.fieldLocator == fieldLocator) then
                case fieldInstance.component of
                    SelectizeComponent _ ->
                        { fieldInstance | component = SelectizeComponent state }

                    _ ->
                        Debug.crash "Shouldn't be here"
            else
                fieldInstance
    in
        { model | fieldInstances = List.map updateInstance fieldInstances }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load configGroupResponse ->
            let
                status =
                    if model.status == Saving then
                        Saved
                    else
                        model.status

                webConfigGroup =
                    RemoteData.map .data configGroupResponse

                fieldInstances : List FieldInstance
                fieldInstances =
                    case webConfigGroup of
                        Success configGroup ->
                            initFieldInstances configGroup

                        _ ->
                            []

                defaultCrypto =
                    case webConfigGroup of
                        Success configGroup ->
                            listCryptos configGroup
                                |> List.head
                                |> Maybe.map .crypto

                        _ ->
                            Nothing

                crypto =
                    case model.crypto of
                        Nothing ->
                            Debug.log "DEBUG27" defaultCrypto

                        Just crypto ->
                            Just crypto
            in
                ( { model
                    | webConfigGroup = webConfigGroup
                    , fieldInstances = fieldInstances
                    , status = status
                    , crypto = crypto
                  }
                , Cmd.none
                )

        Submit ->
            case model.webConfigGroup of
                Success configGroup ->
                    { model | status = Saving }
                        ! [ postForm configGroup.schema.code model.fieldInstances ]

                _ ->
                    model ! []

        Input fieldLocator valueString ->
            updateInput fieldLocator (Just valueString) model ! []

        CryptoSwitch crypto ->
            case model.webConfigGroup of
                Success configGroup ->
                    let
                        cryptoCode =
                            cryptoToString crypto

                        path =
                            "config/" ++ configGroup.schema.code ++ "/" ++ cryptoCode

                        command =
                            Hop.outputFromPath hopConfig (Debug.log "DEBUG26" path)
                                |> Navigation.newUrl
                    in
                        { model | crypto = Just crypto } ! [ command ]

                _ ->
                    model ! []

        Focus fieldLocator ->
            updateFocus fieldLocator True model ! []

        Blur fieldLocator ->
            updateFocus fieldLocator False model ! []

        SelectizeMsg fieldLocator selectizeState ->
            updateSelectize fieldLocator selectizeState model ! []

        BlurSelectize fieldLocator selectizeState ->
            (updateSelectize fieldLocator selectizeState model
                |> updateFocus fieldLocator False
            )
                ! []

        FocusSelectize fieldLocator selectizeState ->
            (updateSelectize fieldLocator selectizeState model
                |> updateFocus fieldLocator True
            )
                ! []

        Add fieldLocator code selectizeState ->
            (updateSelectize fieldLocator selectizeState model
                |> updateInput fieldLocator (Just code)
            )
                ! []

        Remove fieldLocator selectizeState ->
            (updateSelectize fieldLocator selectizeState model
                |> updateInput fieldLocator Nothing
            )
                ! []


cryptoView : Maybe Crypto -> CryptoDisplay -> Html Msg
cryptoView maybeActiveCrypto cryptoDisplay =
    let
        activeClass =
            case maybeActiveCrypto of
                Nothing ->
                    class []

                Just activeCrypto ->
                    if (activeCrypto == cryptoDisplay.crypto) then
                        class [ C.Active ]
                    else
                        class []
    in
        div [ activeClass, onClick (CryptoSwitch cryptoDisplay.crypto) ] [ text cryptoDisplay.display ]


cryptosView : Maybe Crypto -> ConfigGroup -> Html Msg
cryptosView activeCrypto configGroup =
    let
        cryptos =
            listCryptos configGroup
    in
        nav [ class [ C.CryptoTabs ] ] (List.map (cryptoView activeCrypto) cryptos)


view : Model -> Html Msg
view model =
    case model.webConfigGroup of
        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success configGroup ->
            let
                resolvedModel =
                    toResolvedModel model configGroup

                configGroupView =
                    div [ class [ C.ConfigContainer ] ]
                        [ tableView resolvedModel ]

                statusString =
                    case model.status of
                        Saved ->
                            "Saved"

                        _ ->
                            ""

                form =
                    Html.form []
                        [ div [] [ configGroupView ]
                        , div [ class [ C.ButtonRow ] ]
                            [ div [ onClick Submit, class [ C.Button ] ] [ text "Submit" ]
                            , div [] [ text statusString ]
                            ]
                        ]
            in
                if (configGroup.schema.cryptoScope == Global) then
                    div []
                        [ div [ class [ C.SectionLabel ] ] [ text configGroup.schema.display ]
                        , form
                        ]
                else
                    div []
                        [ div [ class [ C.SectionLabel ] ] [ text configGroup.schema.display ]
                        , div [] [ (cryptosView model.crypto configGroup) ]
                        , form
                        ]
