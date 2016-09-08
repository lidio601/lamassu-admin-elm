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
import Css.Admin exposing (..)
import Css.Classes
import Selectize
import Css.Selectize
import Maybe exposing (oneOf)
import String
import SelectizeHelpers exposing (..)

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
    , fieldClusters : List FieldCluster
    , crypto : Maybe Crypto
    , status : SavingStatus
    , focused : Maybe FieldLocator
    }


type alias ResolvedModel =
    { configGroup : ConfigGroup
    , fieldClusters : List FieldCluster
    , crypto : Crypto
    , status : SavingStatus
    , focused : Maybe FieldLocator
    }


type alias SelectizeModel =
    Selectize.Model String


type alias SelectizeItem =
    Selectize.Item String


type alias SelectizeMsgType =
    Selectize.Msg String


toResolvedModel : Model -> ConfigGroup -> ResolvedModel
toResolvedModel model configGroup =
    { configGroup = configGroup
    , fieldClusters = model.fieldClusters
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


postForm : String -> List FieldCluster -> Cmd Msg
postForm configGroupCode fieldClusters =
    post "http://localhost:8093/config"
        |> withHeader "Content-Type" "application/json"
        |> withJsonBody (encodeResults configGroupCode fieldClusters)
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


updateStringFieldInstance : FieldLocator -> FieldHolder -> FieldInstance -> FieldInstance
updateStringFieldInstance fieldLocator fieldHolder fieldInstance =
    if fieldInstance.fieldLocator == fieldLocator then
        { fieldInstance | fieldValue = fieldHolder }
    else
        fieldInstance


updateInput : FieldLocator -> FieldType -> String -> Model -> ( Model, Cmd Msg )
updateInput fieldLocator fieldType valueString model =
    let
        fieldValue =
            stringToFieldValue fieldType valueString

        fieldInstances =
            List.map (updateStringFieldInstance fieldLocator fieldValue) model.fieldInstances
    in
        { model | fieldInstances = fieldInstances } ! []



-- View


textInput : FieldLocator -> FieldType -> Maybe FieldValue -> Maybe FieldValue -> Html Msg
textInput fieldLocator fieldType maybeFieldValue maybeFallbackFieldValue =
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
            [ onInput (Input fieldLocator fieldType)
            , onFocus (Focus fieldLocator)
            , onBlur (Blur fieldLocator)
            , defaultValue defaultString
            , placeholder fallbackString
            , class [ Css.Classes.BasicInput ]
            ]
            []


selectizeHtmlClasses : Selectize.HtmlClasses
selectizeHtmlClasses =
    Css.Selectize.classes


selectizeHtmlOptions : Selectize.HtmlOptions
selectizeHtmlOptions =
    { instructionsForBlank = "Start typing to see options..."
    , atMaxLength = "Type backspace to edit"
    , noMatches = "No matches"
    , typeForMore = "Type for more options"
    , classes = selectizeHtmlClasses
    , noOptions = "No options"
    }


fieldInput : ResolvedModel -> FieldInstance -> Maybe FieldValue -> Maybe FieldValue -> Html Msg
fieldInput model fieldInstance maybeFieldValue maybeFallbackFieldValue =
    case fieldInstance.component of
        InputBoxComponent fieldType ->
            textInput fieldInstance.fieldLocator fieldType maybeFieldValue maybeFallbackFieldValue

        SelectizeComponent fieldType selectizeModel ->
            let
                fallbackCodes =
                    case maybeFallbackFieldValue of
                        Nothing ->
                            []

                        Just fallbackFieldValue ->
                            fieldValueToList fallbackFieldValue
            in
                Html.App.map (SelectizeMsg (Debug.log "DEBUG11" fieldInstance.fieldLocator))
                    (Selectize.view selectizeHtmlOptions fallbackCodes selectizeModel)


fieldComponent : ResolvedModel -> FieldInstance -> Html Msg
fieldComponent model fieldInstance =
    let
        fieldLocator =
            fieldInstance.fieldLocator

        fieldScope =
            fieldLocator.fieldScope

        fieldCode =
            fieldLocator.code

        instances : List FieldInstance
        instances =
            model.fieldInstances

        maybeGlobal =
            pickFieldInstanceValue GlobalCrypto GlobalMachine fieldCode instances

        maybeGlobalCrypto =
            pickFieldInstanceValue GlobalCrypto fieldScope.machine fieldCode instances

        maybeGlobalMachine =
            pickFieldInstanceValue fieldScope.crypto GlobalMachine fieldCode instances

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
              , div [ classList [ ( Css.Classes.Component, True ), ( Css.Classes.FocusedComponent, focused ) ] ]
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
                    class [ Css.Classes.ConfigTableGlobalRow ]

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
            (Debug.log "DEBUG8" model.crypto)

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
        table [ class [ Css.Classes.ConfigTable ] ]
            [ thead [] [ headerRow ]
            , tbody [] rows
            ]


isField : String -> Field -> Bool
isField fieldCode field =
    field.fieldLocator.code == fieldCode


type Msg
    = Load ConfigGroupResponse
    | Submit
    | Input FieldLocator FieldType String
    | CryptoSwitch Crypto
    | SelectizeMsg FieldLocator SelectizeMsgType
    | Blur FieldLocator
    | Focus FieldLocator


initFieldInstances : ConfigGroup -> List (FieldInstance valueType)
initFieldInstances configGroup =
    let

initFieldCluster : ConfigGroup -> FieldDescriptor -> FieldScope -> FieldCluster
initFieldCluster configGroup fieldDescriptor fieldScope =
    let
        fieldCode =
            fieldDescriptor.code

        fieldType =
            fieldDescriptor.fieldType

        fieldInstances =
            initFieldInstances configGroup
    in
        case fieldType of
            FieldStringType ->
                FieldStringCluster fieldCode (fieldInstances fieldType)

            FieldPercentageType ->
                FieldPercentageCluster fieldCode (fieldInstances fieldType)

            FieldIntegerType ->
                FieldIntegerCluster fieldCode (fieldInstances fieldType)

            FieldOnOffType ->
                FieldOnOffCluster fieldCode (fieldInstances fieldType)

            FieldAccountType accountClass ->
                FieldAccountCluster fieldCode
                    (fieldInstances fieldType)
                    (initAccountSelectize configGroup accountClass fieldScope Nothing)

            FieldCurrencyType ->
                FieldCurrencyCluster fieldType
                    (fieldInstances fieldType)
                    (initCurrencySelectize configGroup fieldScope Nothing)

            FieldLanguageType ->
                FieldLanguageCluster fieldType
                    (fieldInstances fieldType)
                    (initLanguageSelectize configGroup fieldScope Nothing)


initFieldClustersPerEntry : ConfigGroup -> FieldDescriptor -> List FieldCluster
initFieldClustersPerEntry configGroup fieldDescriptor =
    List.map (initFieldCluster configGroup fieldDescriptor) (fieldScopes configGroup)


initFieldClusters : ConfigGroup -> List FieldCluster
initFieldClusters configGroup =
    List.concatMap (initFieldClustersPerEntry configGroup) configGroup.schema.entries


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


pickFieldInstanceValue : Crypto -> Machine -> String -> List FieldInstance -> Maybe FieldValue
pickFieldInstanceValue crypto machine fieldCode fieldInstances =
    let
        fieldScope =
            { crypto = crypto, machine = machine }

        fieldLocator =
            { fieldScope = fieldScope, code = fieldCode }
    in
        (Debug.log "DEBUG13" (pickFieldInstance fieldLocator fieldInstances))
            `Maybe.andThen` fieldInstanceToMaybeFieldValue


updateSelectizeValue : FieldType -> SelectizeModel -> Maybe FieldValue
updateSelectizeValue fieldType selectizeModel =
    case fieldType of
        FieldCurrencyType ->
            Selectize.selectedIds selectizeModel
                |> List.head
                |> Maybe.map FieldCurrencyValue

        FieldAccountType accountClass ->
            Selectize.selectedIds selectizeModel
                |> List.head
                |> Maybe.map (FieldAccountValue accountClass)

        FieldLanguageType ->
            Selectize.selectedIds selectizeModel
                |> (\languages ->
                        if List.isEmpty languages then
                            Nothing
                        else
                            (Just (FieldLanguageValue languages))
                   )

        _ ->
            Debug.crash "Not a selectize field"


determineSelectizeFocus : FieldLocator -> SelectizeMsgType -> Model -> Maybe FieldLocator
determineSelectizeFocus fieldLocator selectizeMsg model =
    if Selectize.focused selectizeMsg then
        Just fieldLocator
    else if (Selectize.blurred selectizeMsg) then
        if model.focused == (Just fieldLocator) then
            Nothing
        else
            model.focused
    else
        model.focused


updateSelectize : FieldLocator -> SelectizeMsgType -> Model -> ( Model, Cmd Msg )
updateSelectize fieldLocator selectizeMsg model =
    case (pickFieldInstance fieldLocator model.fieldInstances) of
        Nothing ->
            model ! []

        Just fieldInstance ->
            case fieldInstance.component of
                SelectizeComponent fieldType selectizeModel ->
                    let
                        ( newSelectizeModel, selectizeCmd ) =
                            Selectize.update selectizeMsg selectizeModel

                        newValue =
                            updateSelectizeValue fieldType newSelectizeModel

                        modifyInstance currentFieldInstance =
                            if currentFieldInstance.fieldLocator == fieldLocator then
                                { currentFieldInstance
                                    | component = SelectizeComponent fieldType newSelectizeModel
                                    , fieldValue = Ok newValue
                                }
                            else
                                currentFieldInstance
                    in
                        { model
                            | fieldInstances = List.map modifyInstance model.fieldInstances
                            , focused = determineSelectizeFocus fieldLocator selectizeMsg model
                        }
                            ! []

                _ ->
                    model ! []


updateFocus : FieldLocator -> Bool -> Model -> ( Model, Cmd Msg )
updateFocus fieldLocator focused model =
    if focused then
        { model | focused = Just fieldLocator } ! []
    else if model.focused == Just fieldLocator then
        { model | focused = Nothing } ! []
    else
        model ! []


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

                fieldClusters : List FieldCluster
                fieldClusters =
                    case webConfigGroup of
                        Success configGroup ->
                            initFieldClusters configGroup

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
                            defaultCrypto

                        Just crypto ->
                            Just crypto
            in
                ( { model
                    | webConfigGroup = webConfigGroup
                    , fieldClusters = fieldClusters
                    , status = status
                    , crypto = (Debug.log "DEBUG3" crypto)
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

        Input fieldLocator fieldType valueString ->
            updateInput fieldLocator fieldType valueString model

        CryptoSwitch crypto ->
            case model.webConfigGroup of
                Success configGroup ->
                    let
                        cryptoCode =
                            cryptoToString crypto

                        url =
                            "/config/" ++ configGroup.schema.code ++ "/" ++ cryptoCode
                    in
                        { model | crypto = Just crypto } ! [ Navigation.newUrl url ]

                _ ->
                    model ! []

        SelectizeMsg fieldLocator selectizeMsg ->
            updateSelectize fieldLocator selectizeMsg model

        Focus fieldLocator ->
            updateFocus fieldLocator True model

        Blur fieldLocator ->
            updateFocus fieldLocator False model


cryptoView : Maybe Crypto -> CryptoDisplay -> Html Msg
cryptoView maybeActiveCrypto cryptoDisplay =
    let
        activeClass =
            case maybeActiveCrypto of
                Nothing ->
                    class []

                Just activeCrypto ->
                    if (activeCrypto == cryptoDisplay.crypto) then
                        class [ Css.Classes.Active ]
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
        nav [ class [ Css.Classes.CryptoTabs ] ] (List.map (cryptoView activeCrypto) cryptos)


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
                    div [ class [ Css.Classes.ConfigTableContainer ] ]
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
                        , div [ class [ Css.Classes.ConfigButtonRow ] ]
                            [ div [ onClick Submit, class [ Css.Classes.ConfigButton ] ] [ text "Submit" ]
                            , div [] [ text statusString ]
                            ]
                        ]
            in
                if (configGroup.schema.cryptoScope == Global) then
                    div []
                        [ div [ class [ Css.Classes.ConfigGroupLabel ] ] [ text configGroup.schema.display ]
                        , form
                        ]
                else
                    div []
                        [ div [ class [ Css.Classes.ConfigGroupLabel ] ] [ text configGroup.schema.display ]
                        , div [] [ (cryptosView model.crypto configGroup) ]
                        , form
                        ]
