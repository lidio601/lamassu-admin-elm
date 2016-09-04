module Config exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (defaultValue, placeholder)
import Html.App
import Html.Keyed
import Navigation
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (..)
import ConfigEncoder exposing (..)
import Html.CssHelpers
import CssClasses
import Selectize
import Maybe exposing (oneOf)


{ id, class, classList } =
    Html.CssHelpers.withNamespace "lamassuAdmin"
type alias ConfigGroupResponse =
    RemoteData (Error String) (Response ConfigGroup)


type alias WebConfigGroup =
    RemoteData (Error String) ConfigGroup


type SavingStatus
    = Saving
    | Saved
    | Editing
    | NotSaving


type ItemRec
    = ItemValue String


type alias Selectizer =
    ( FieldScope, Selectize.Model )


type alias Model =
    { webConfigGroup : WebConfigGroup
    , fieldInstances : List FieldInstance
    , crypto : Maybe Crypto
    , status : SavingStatus
    }


type alias ResolvedModel =
    { configGroup : ConfigGroup
    , fieldInstances : List FieldInstance
    , crypto : Crypto
    , status : SavingStatus
    }


toResolvedModel : Model -> ConfigGroup -> ResolvedModel
toResolvedModel model configGroup =
    { configGroup = configGroup
    , fieldInstances = model.fieldInstances
    , crypto = Maybe.withDefault GlobalCrypto model.crypto
    , status = model.status
    }


getForm : String -> Cmd Msg
getForm code =
    get ("http://localhost:8093/config/" ++ code)
        |> send (jsonReader configGroupDecoder) stringReader
        |> RemoteData.asCmd
        |> Cmd.map Load


postForm : ConfigGroup -> Cmd Msg
postForm configGroup =
    post "http://localhost:8093/config"
        |> withHeader "Content-Type" "application/json"
        |> withJsonBody (encodeConfigGroup configGroup)
        |> send (jsonReader configGroupDecoder) stringReader
        |> RemoteData.asCmd
        |> Cmd.map Load


init : Model
init =
    { webConfigGroup = RemoteData.NotAsked
    , selectizers = []
    , crypto = Nothing
    , status = NotSaving
    }


load : Model -> String -> Maybe String -> ( Model, Cmd Msg )
load model code maybeCryptoCodeString =
    let
        crypto =
            Maybe.map stringToCrypto maybeCryptoCodeString
    in
        ( { model | crypto = crypto }, getForm code )



-- UPDATE


toMatchedFieldValue : Crypto -> Machine -> String -> Field -> Maybe FieldValue
toMatchedFieldValue crypto machine fieldCode field =
    let
        maybeFieldValue =
            case field.fieldValue of
                Err _ ->
                    field.loadedFieldValue

                Ok originalMaybeFieldValue ->
                    originalMaybeFieldValue
    in
        if (isOfFieldClass crypto machine fieldCode field) then
            maybeFieldValue
        else
            Nothing


pickField : List Field -> Crypto -> Machine -> String -> Maybe FieldValue
pickField fields crypto machine fieldCode =
    List.filterMap (toMatchedFieldValue crypto machine fieldCode) fields
        |> List.head


isOfFieldClass : Crypto -> Machine -> String -> Field -> Bool
isOfFieldClass crypto machine fieldCode field =
    field.crypto
        == crypto
        && field.machine
        == machine
        && field.code
        == fieldCode


isFieldClass : Field -> Field -> Bool
isFieldClass searchField field =
    isOfFieldClass searchField.crypto searchField.machine searchField.code field


placeField : List Field -> Field -> List Field
placeField fieldList field =
    let
        maybeOldField =
            List.filter (isFieldClass field) fieldList
                |> List.head

        newField =
            case maybeOldField of
                Nothing ->
                    field

                Just oldField ->
                    { oldField | fieldValue = field.fieldValue }
    in
        newField :: (List.filter (not << (isFieldClass field)) fieldList)


updateValues : ConfigGroup -> FieldLocator -> String -> ConfigGroup
updateValues configGroup fieldLocator valueString =
    let
        maybeFieldDescriptor =
            List.filter (\fd -> fd.code == fieldLocator.code) configGroup.schema.entries
                |> List.head
    in
        case maybeFieldDescriptor of
            Just fieldDescriptor ->
                let
                    fieldValueHolder =
                        stringToFieldValue fieldDescriptor.fieldType valueString

                    field =
                        { fieldLocator = fieldLocator
                        , fieldValue = fieldValueHolder
                        , loadedFieldValue = Nothing
                        }

                    values =
                        placeField configGroup.values field
                in
                    { configGroup | values = values }

            Nothing ->
                configGroup



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
            , defaultValue defaultString
            , placeholder fallbackString
            ]
            []


fieldInput : ResolvedModel -> FieldInstance -> Maybe FieldValue -> Maybe FieldValue -> Html Msg
fieldInput model fieldInstance maybeFieldValue maybeFallbackFieldValue =
    case fieldInstance.component of
        InputBoxComponent fieldType ->
            textInput fieldInstance.fieldLocator maybeFieldValue maybeFallbackFieldValue

        SelectizeComponent fieldType selectizeModel ->
            Html.App.map (SelectizeMsg fieldInstance.fieldLocator) (Selectize.view selectizeModel)


fieldComponent : ResolvedModel -> FieldLocator -> Html Msg
fieldComponent model fieldLocator =
    let
        fieldCode =
            fieldLocator.code

        values =
            model.values

        maybeGlobal =
            pickField values GlobalCrypto GlobalMachine fieldCode

        maybeGlobalCrypto =
            pickField values GlobalCrypto fieldLocator.machine fieldCode

        maybeGlobalMachine =
            pickField values fieldLocator.crypto GlobalMachine fieldCode

        maybeSpecific =
            pickField values fieldLocator.crypto fieldLocator.machine fieldCode

        maybeFallbackFieldValue =
            oneOf [ maybeSpecific, maybeGlobalMachine, maybeGlobalCrypto, maybeGlobal ]
    in
        fieldInput model fieldLocator maybeSpecific maybeFallbackFieldValue


cellView : ResolvedModel -> FieldLocator -> Html Msg
cellView model fieldLocator =
    -- Note: keying here is needed to clear out fields when switching cryptos
    let
        machine =
            fieldLocator.fieldScope.machine

        crypto =
            fieldLocator.fieldScope.crypto
    in
        Html.Keyed.node "td"
            []
            [ ( (cryptoToString crypto)
                    ++ "-"
                    ++ (machineToString machine)
                    ++ "-"
                    ++ fieldLocator.code
              , fieldComponent model fieldLocator
              )
            ]


rowView : ResolvedModel -> MachineDisplay -> Html Msg
rowView model machineDisplay =
    let
        globalRowClass machine =
            case machine of
                GlobalMachine ->
                    class [ CssClasses.ConfigTableGlobalRow ]

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

        fieldLocators =
            List.map toFieldLocator model.configGroup.schema.entries
    in
        tr [ globalRowClass machineDisplay.machine ]
            ((td [] [ text (machineDisplay.display) ])
                :: (List.map (cellView model)
                        fieldLocators
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

        rows =
            List.map (rowView model) machines
    in
        table [ class [ CssClasses.ConfigTable ] ]
            [ thead [] [ headerRow ]
            , tbody [] rows
            ]


isField : String -> Field -> Bool
isField fieldCode field =
    field.code == fieldCode


type Msg
    = Load ConfigGroupResponse
    | Submit
    | Input FieldLocator String
    | CryptoSwitch Crypto
    | SelectizeMsg FieldLocator Selectize.Msg


selectizeItem : DisplayRec -> Selectize.Item
selectizeItem displayRec =
    let
        code =
            displayRec.code

        itemRec =
            ItemValue code
    in
        Selectize.selectizeItem code displayRec.display []


initCurrencySelectize : FieldDescriptor -> ConfigGroup -> FieldScope -> Selectizer
initCurrencySelectize fieldDescriptor configGroup fieldScope =
    let
        availableItems =
            List.map selectizeItem configGroup.data.currencies

        selectizeModel =
            Selectize.init 1 availableItems
    in
        ( fieldScope, selectizeModel )


initAccountSelectize : ConfigGroup -> String -> FieldScope -> Selectize.Model
initAccountSelectize configGroup accountClass fieldScope =
    let
        toDisplayRec accountRec =
            if (accountClass == accountRec.class) then
                Just { code = accountRec.code, display = accountRec.display }
            else
                Nothing

        availableItems =
            List.filterMap toDisplayRec configGroup.data.accounts
                |> List.map selectizeItem
    in
        Selectize.init 1 availableItems


buildFieldComponent : ConfigGroup -> FieldType -> FieldScope -> FieldComponent
buildFieldComponent configGroup fieldType fieldScope =
    case fieldType of
        FieldStringType ->
            InputBoxComponent fieldType

        FieldPercentageType ->
            InputBoxComponent fieldType

        FieldIntegerType ->
            InputBoxComponent fieldType

        FieldOnOffType ->
            InputBoxComponent fieldType

        FieldAccountType accountClass ->
            SelectizeComponent fieldType
                (initAccountSelectize configGroup accountClass fieldScope)

        FieldCurrencyType ->
            SelectizeComponent fieldType (initCurrencySelectize configGroup fieldScope)


initFieldInstance : ConfigGroup -> FieldDescriptor -> FieldScope -> FieldInstance
initFieldInstance configGroup fieldDescriptor fieldScope =
    let
        component =
            buildFieldComponent configGroup fieldDescriptor.fieldType fieldScope
    in
        { fieldScope = fieldScope
        , code = fieldDescriptor.code
        , component = component
        }


initFieldInstancesPerEntry : ConfigGroup -> List FieldScope -> FieldDescriptor -> List FieldInstance
initFieldInstancesPerEntry configGroup fieldScopes fieldDescriptor =
    List.map (initFieldInstances configGroup fieldDescriptor) fieldScopes


initFieldInstances : ConfigGroup -> List FieldInstance
initFieldInstances configGroup =
    List.map (initFieldInstancesPerEntry (configGroup fieldScopes configGroup))
        configGroup.schema.entries


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

                fieldInstances =
                    case webConfigGroup of
                        Success configGroup ->
                            initFieldInstances configGroup

                        _ ->
                            []
            in
                ( { model
                    | webConfigGroup = webConfigGroup
                    , fieldInstances = fieldInstances
                    , status = status
                  }
                , Cmd.none
                )

        Submit ->
            case model.webConfigGroup of
                Success configGroup ->
                    { model | status = Saving } ! [ postForm configGroup ]

                _ ->
                    model ! []

        Input fieldLocator valueString ->
            case model.webConfigGroup of
                Success configGroup ->
                    let
                        newConfigGroup =
                            updateValues configGroup fieldLocator valueString

                        webConfigGroup =
                            Success newConfigGroup
                    in
                        { model
                            | webConfigGroup = webConfigGroup
                            , status = Editing
                        }
                            ! []

                _ ->
                    model ! []

        CryptoSwitch crypto ->
            case model.webConfigGroup of
                Success configGroup ->
                    let
                        cryptoCode =
                            cryptoToString crypto

                        url =
                            "/config/" ++ configGroup.schema.code ++ "/" ++ cryptoCode
                    in
                        { model | crypto = Just (Debug.log "DEBUG24" crypto) } ! [ Navigation.newUrl url ]

                _ ->
                    model ! []


cryptoView : Maybe Crypto -> CryptoDisplay -> Html Msg
cryptoView maybeActiveCrypto cryptoDisplay =
    let
        activeClass =
            case maybeActiveCrypto of
                Nothing ->
                    class []

                Just activeCrypto ->
                    if (activeCrypto == cryptoDisplay.crypto) then
                        class [ CssClasses.Active ]
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
        nav [ class [ CssClasses.CryptoTabs ] ] (List.map (cryptoView activeCrypto) cryptos)


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
                    div [ class [ CssClasses.ConfigTableContainer ] ]
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
                        , div [ class [ CssClasses.ConfigButtonRow ] ]
                            [ div [ onClick Submit, class [ CssClasses.ConfigButton ] ] [ text "Submit" ]
                            , div [] [ text statusString ]
                            ]
                        ]
            in
                if (configGroup.schema.cryptoScope == Global) then
                    div []
                        [ div [ class [ CssClasses.ConfigGroupLabel ] ] [ text configGroup.schema.display ]
                        , form
                        ]
                else
                    div []
                        [ div [ class [ CssClasses.ConfigGroupLabel ] ] [ text configGroup.schema.display ]
                        , div [] [ (cryptosView model.crypto configGroup) ]
                        , form
                        ]
