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
import InitFieldGroup exposing (initFieldCluster)


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
    , fieldGroups : List FieldGroup
    , crypto : Maybe Crypto
    , status : SavingStatus
    , focused : Maybe FieldLocator
    }


type alias ResolvedModel =
    { configGroup : ConfigGroup
    , fieldGroups : List FieldGroup
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
    , fieldGroups = model.fieldGroups
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


postForm : String -> List FieldGroup -> Cmd Msg
postForm configGroupCode fieldGroups =
    post "http://localhost:8093/config"
        |> withHeader "Content-Type" "application/json"
        |> withJsonBody (encodeResults configGroupCode fieldGroups)
        |> send (jsonReader configGroupDecoder) stringReader
        |> RemoteData.asCmd
        |> Cmd.map Load


init : Model
init =
    { webConfigGroup = RemoteData.NotAsked
    , fieldGroups = []
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


toFieldHolder : (String -> Result String valueType) -> String -> FieldHolder valueType
toFieldHolder converter string =
    if String.isEmpty string then
        Nothing
    else
        Just (Result.formatError FieldParsingError (converter string))


updateWhen : (x -> Bool) -> (x -> x) -> x -> x
updateWhen predicate updater item =
    if predicate item then
        updater item
    else
        item


updateFieldInstances :
    FieldScope
    -> String
    -> List (FieldInstance keyValue componentModel)
    -> (String -> Result String keyValue)
    -> List (FieldInstance keyValue componentModel)
updateFieldInstances fieldScope string instances converter =
    (\instance ->
        (updateWhen (((==) fieldScope) << .fieldScope)
            { instance | value = toFieldHolder String.toFloat string }
        )
    )
        |> List.map instances


updateInput : FieldLocator -> String -> Model -> ( Model, Cmd Msg )
updateInput fieldLocator string model =
    let
        updateInstances =
            updateFieldInstances fieldLocator.fieldScope string

        updateCluster generalCluster =
            case generalCluster of
                FieldInputCluster cluster ->
                    case cluster of
                        FieldStringCluster instances ->
                            updateInstances instances (Ok << identity)
                                |> FieldStringCluster
                                |> FieldInputCluster

                        FieldPercentageCluster instances ->
                            updateInstances instances String.toFloat
                                |> FieldPercentageCluster
                                |> FieldInputCluster

                _ ->
                    generalCluster

        updateFieldGroup fieldGroup =
            { fieldGroup | fieldCluster = updateCluster fieldGroup.fieldCluster }

        fieldGroups =
            List.map
                (\group ->
                    updateWhen (((==) fieldLocator.fieldCode) << .fieldCode)
                        updateFieldGroup
                        group
                )
                model.fieldGroups
    in
        { model | fieldGroups = fieldGroups } ! []


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


maybeFieldHolderToMaybe : Maybe (FieldHolder valueType) -> Maybe valueType
maybeFieldHolderToMaybe maybeFieldHolder =
    case maybeFieldHolder of
        Nothing ->
            Nothing

        Just fieldHolder ->
            case fieldHolder of
                Nothing ->
                    Nothing

                Just result ->
                    case result of
                        Ok val ->
                            Just val

                        Err _ ->
                            Nothing


pickSpecific :
    List (FieldInstance keyValue componentModel)
    -> FieldScope
    -> Maybe (FieldInstance keyValue componentModel)
pickSpecific instances fieldScope =
    List.filter (((==) fieldScope) << .fieldScope) instances
        |> List.head


pickSpecificValue :
    List (FieldInstance keyValue componentModel)
    -> FieldScope
    -> Maybe keyValue
pickSpecificValue instances fieldScope =
    pickSpecific instances fieldScope
        |> (maybeFieldHolderToMaybe << (Maybe.map .fieldValue))


pickValue :
    List (FieldInstance keyValue componentModel)
    -> Crypto
    -> Machine
    -> Maybe keyValue
pickValue instances crypto machine =
    pickSpecificValue instances { crypto = crypto, machine = machine }


pickFallbackValue :
    List (FieldInstance keyValue componentModel)
    -> FieldScope
    -> Maybe keyValue
pickFallbackValue instances fieldScope =
    let
        pick =
            pickValue instances

        crypto =
            fieldScope.crypto

        machine =
            fieldScope.machine
    in
        oneOf
            [ pick crypto machine
            , pick crypto GlobalMachine
            , pick GlobalCrypto machine
            , pick GlobalCrypto GlobalMachine
            ]


selectizeView :
    FieldLocator
    -> (Maybe valueType -> List valueType)
    -> List (FieldInstance valueType SelectizeModel)
    -> Html Msg
selectizeView fieldLocator converter instances =
    let
        fieldScope =
            fieldLocator.fieldScope

        maybeSpecific =
            pickSpecific instances fieldScope

        maybeFallbackValue =
            pickFallbackValue instances fieldScope

        fallbackCodes =
            converter maybeFallbackValue

        selectizeModel =
            Maybe.map .componentModel maybeSpecific
                |> Maybe.withDefault (Debug.crash "No fieldInstance for fieldLocator")
    in
        Html.App.map (SelectizeMsg fieldLocator)
            (Selectize.view selectizeHtmlOptions fallbackCodes selectizeModel)


inputView :
    FieldLocator
    -> (valueType -> String)
    -> List (FieldInstance valueType componentModel)
    -> Html Msg
inputView fieldLocator converter instances =
    let
        fieldScope =
            fieldLocator.fieldScope

        maybeSpecific =
            pickSpecific instances fieldScope

        maybeFallbackValue =
            pickFallbackValue instances fieldScope

        maybeSpecificString =
            Maybe.map converter maybeSpecific

        maybeFallbackString =
            Maybe.map converter maybeFallbackValue

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
            , class [ Css.Classes.BasicInput ]
            ]
            []


inputClusterView : FieldLocator -> InputCluster -> Html Msg
inputClusterView fieldLocator cluster =
    let
        localView =
            (inputView fieldLocator)
    in
        case cluster of
            FieldStringCluster instances ->
                localView identity instances


selectizeClusterView : FieldLocator -> SelectizeCluster -> Html Msg
selectizeClusterView fieldLocator cluster =
    let
        localView =
            (selectizeView fieldLocator)
    in
        case cluster of
            FieldAccountCluster instances ->
                localView maybeToList instances


cellView : ResolvedModel -> Machine -> FieldGroup -> Html Msg
cellView model machine fieldGroup =
    let
        fieldCluster =
            fieldGroup.fieldCluster

        fieldScope =
            { crypto = model.crypto, machine = machine }

        fieldLocator =
            { fieldCode = fieldGroup.fieldCode, fieldScope = fieldScope }
    in
        case fieldCluster of
            FieldInputCluster cluster ->
                inputClusterView fieldLocator cluster

            FieldSelectizeCluster cluster ->
                selectizeClusterView fieldLocator cluster


rowView : ResolvedModel -> MachineDisplay -> Html Msg
rowView model machineDisplay =
    let
        machine =
            machineDisplay.machine

        globalRowClass =
            case machine of
                GlobalMachine ->
                    [ class [ Css.Classes.ConfigTableGlobalRow ] ]

                _ ->
                    []

        cells =
            List.map (cellView model machine) model.fieldGroups
    in
        tr globalRowClass
            ((td [] [ text (machineDisplay.display) ]) :: cells)


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

        rows =
            List.map (rowView model) machines
    in
        table [ class [ Css.Classes.ConfigTable ] ]
            [ thead [] [ headerRow ]
            , tbody [] rows
            ]


type Msg
    = Load ConfigGroupResponse
    | Submit
    | Input FieldLocator String
    | SelectizeMsg FieldLocator SelectizeMsgType
    | CryptoSwitch Crypto
    | Blur FieldLocator
    | Focus FieldLocator


initFieldGroups : ConfigGroup -> List FieldGroup
initFieldGroups configGroup =
    List.concatMap (initFieldCluster configGroup) configGroup.schema.fieldDescriptors



--
-- pickFieldInstance : FieldLocator -> List FieldInstance -> Maybe FieldInstance
-- pickFieldInstance fieldLocator fieldInstances =
--     let
--         sameLocation targetFieldLocator fieldInstance =
--             fieldInstance.fieldLocator == targetFieldLocator
--     in
--         List.filter (sameLocation fieldLocator) fieldInstances
--             |> List.head
--
-- fieldInstanceToMaybeFieldValue : FieldInstance -> Maybe FieldValue
-- fieldInstanceToMaybeFieldValue fieldInstance =
--     case fieldInstance.fieldValue of
--         Ok maybeFieldValue ->
--             maybeFieldValue
--
--         _ ->
--             Nothing
--
--
-- pickFieldInstanceValue : Crypto -> Machine -> String -> List FieldInstance -> Maybe FieldValue
-- pickFieldInstanceValue crypto machine fieldCode fieldInstances =
--     let
--         fieldScope =
--             { crypto = crypto, machine = machine }
--
--         fieldLocator =
--             { fieldScope = fieldScope, code = fieldCode }
--     in
--         (Debug.log "DEBUG13" (pickFieldInstance fieldLocator fieldInstances))
--             `Maybe.andThen` fieldInstanceToMaybeFieldValue
--
--
-- updateSelectizeValue : FieldType -> SelectizeModel -> Maybe FieldValue
-- updateSelectizeValue fieldType selectizeModel =
--     case fieldType of
--         FieldCurrencyType ->
--             Selectize.selectedIds selectizeModel
--                 |> List.head
--                 |> Maybe.map FieldCurrencyValue
--
--         FieldAccountType accountClass ->
--             Selectize.selectedIds selectizeModel
--                 |> List.head
--                 |> Maybe.map (FieldAccountValue accountClass)
--
--         FieldLanguageType ->
--             Selectize.selectedIds selectizeModel
--                 |> (\languages ->
--                         if List.isEmpty languages then
--                             Nothing
--                         else
--                             (Just (FieldLanguageValue languages))
--                    )
--
--         _ ->
--             Debug.crash "Not a selectize field"


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


updateSelectizeComponent :
    SelectizeMsgType
    -> (List keyValue -> Maybe keyValue)
    -> FieldInstance keyValue componentType
    -> ( FieldInstance keyValue componentModel, Cmd Msg )
updateSelectizeComponent selectizeMsg converter instance =
    let
        ( selectizeModel, selectizeCmd ) =
            Selectize.update selectizeMsg instance.fieldComponent
    in
        { instance
            | value = toFieldHolder converter (Selectize.selectedIds selectizeModel)
            , fieldComponent = selectizeModel
        }


updateSelectizeInstances :
    FieldScope
    -> SelectizeMsgType
    -> List (FieldInstance keyValue componentModel)
    -> (List String -> keyValue)
    -> List (FieldInstance keyValue componentModel)
updateSelectizeInstances fieldScope selectizeMsg instances converter =
    List.map
        (\instance ->
            (updateWhen (((==) fieldScope) << .fieldScope)
                (updateSelectizeComponent selectizeMsg converter)
                instance
            )
        )
        instances


updateSelectize : FieldLocator -> SelectizeMsgType -> Model -> ( Model, Cmd Msg )
updateSelectize fieldLocator selectizeMsg model =
    let
        updateInstances =
            updateSelectizeInstances fieldLocator.fieldScope selectizeMsg

        updateCluster generalCluster =
            case generalCluster of
                FieldSelectizeCluster cluster ->
                    case cluster of
                        FieldCurrencyCluster instances ->
                            updateInstances instances List.head
                                |> FieldCurrencyCluster
                                |> FieldSelectizeCluster

                _ ->
                    generalCluster

        updateFieldGroup fieldGroup =
            { fieldGroup | fieldCluster = updateCluster fieldGroup.fieldCluster }

        fieldGroups =
            List.map
                (\group ->
                    updateWhen (((==) fieldLocator.fieldCode) << .fieldCode)
                        updateFieldGroup
                        group
                )
                model.fieldGroups
    in
        { model | fieldGroups = fieldGroups } ! []


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

                fieldGroups : List FieldGroup
                fieldGroups =
                    case webConfigGroup of
                        Success configGroup ->
                            initFieldGroups configGroup

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
                    , fieldGroups = fieldGroups
                    , status = status
                    , crypto = (Debug.log "DEBUG3" crypto)
                  }
                , Cmd.none
                )

        Submit ->
            case model.webConfigGroup of
                Success configGroup ->
                    { model | status = Saving }
                        ! [ postForm configGroup.schema.code model.fieldGroups ]

                _ ->
                    model ! []

        Input fieldLocator valueString ->
            updateInput fieldLocator valueString model

        SelectizeMsg fieldLocator selectizeMsg ->
            updateSelectize fieldLocator selectizeMsg model

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
