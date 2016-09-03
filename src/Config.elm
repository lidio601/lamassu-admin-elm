module Config exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (defaultValue, placeholder)
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
    , selectizers : List Selectizer
    , crypto : Maybe Crypto
    , status : SavingStatus
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


updateValues : ConfigGroup -> Crypto -> Machine -> String -> String -> ConfigGroup
updateValues configGroup crypto machine fieldCode valueString =
    let
        maybeFieldDescriptor =
            List.filter (\fd -> fd.code == fieldCode) configGroup.schema.entries
                |> List.head
    in
        case maybeFieldDescriptor of
            Just fieldDescriptor ->
                let
                    fieldValueHolder =
                        stringToFieldValue fieldDescriptor.fieldType valueString

                    field =
                        { code = fieldCode
                        , crypto = crypto
                        , machine = machine
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


textInput : Crypto -> Machine -> FieldDescriptor -> Maybe FieldValue -> Maybe FieldValue -> Html Msg
textInput crypto machine fieldDescriptor maybeFieldValue maybeFallbackFieldValue =
    let
        maybeSpecificString =
            Maybe.map fieldValueToString maybeFieldValue

        maybeFallbackString =
            Maybe.map fieldValueToString maybeFallbackFieldValue

        defaultString =
            Maybe.withDefault "" maybeSpecificString

        fallbackString =
            Maybe.withDefault "" maybeFallbackString

        cryptoString =
            cryptoToString crypto

        machineString =
            machineToString machine
    in
        input
            [ onInput (Input crypto machine fieldDescriptor.code)
            , defaultValue defaultString
            , placeholder fallbackString
            ]
            []


fieldInput : Crypto -> Machine -> FieldDescriptor -> Maybe FieldValue -> Maybe FieldValue -> Html Msg
fieldInput crypto machine fieldDescriptor maybeFieldValue maybeFallbackFieldValue =
    case fieldDescriptor.fieldType of
        FieldStringType ->
            textInput crypto machine fieldDescriptor maybeFieldValue maybeFallbackFieldValue

        FieldPercentageType ->
            textInput crypto machine fieldDescriptor maybeFieldValue maybeFallbackFieldValue

        FieldIntegerType ->
            textInput crypto machine fieldDescriptor maybeFieldValue maybeFallbackFieldValue

        FieldOnOffType ->
            -- TODO: Need to make a 3-state custom component
            textInput crypto machine fieldDescriptor maybeFieldValue maybeFallbackFieldValue

        FieldAccountType _ ->
            -- TODO: Need to turn into smart search field
            textInput crypto machine fieldDescriptor maybeFieldValue maybeFallbackFieldValue

        FieldCurrencyType ->
            -- TODO: Need to turn into smart search field
            textInput crypto machine fieldDescriptor maybeFieldValue maybeFallbackFieldValue


fieldComponent : ConfigGroup -> Crypto -> Machine -> FieldDescriptor -> Html Msg
fieldComponent configGroup crypto machine fieldDescriptor =
    let
        fieldCode =
            fieldDescriptor.code

        fieldType =
            fieldDescriptor.fieldType

        fields =
            configGroup.values

        maybeGlobal =
            pickField fields GlobalCrypto GlobalMachine fieldCode

        maybeGlobalCrypto =
            pickField fields GlobalCrypto machine fieldCode

        maybeGlobalMachine =
            pickField fields crypto GlobalMachine fieldCode

        maybeSpecific =
            pickField fields crypto machine fieldCode

        maybeFallbackFieldValue =
            oneOf [ maybeSpecific, maybeGlobalMachine, maybeGlobalCrypto, maybeGlobal ]
    in
        fieldInput crypto machine fieldDescriptor maybeSpecific maybeFallbackFieldValue


cellView : ConfigGroup -> Crypto -> Machine -> FieldDescriptor -> Html Msg
cellView configGroup crypto machine fieldDescriptor =
    -- Note: keying here is needed to clear out fields when switching cryptos
    Html.Keyed.node "td"
        []
        [ ( (cryptoToString crypto)
                ++ "-"
                ++ (machineToString machine)
                ++ "-"
                ++ fieldDescriptor.code
          , fieldComponent configGroup crypto machine fieldDescriptor
          )
        ]


rowView : ConfigGroup -> Crypto -> MachineDisplay -> Html Msg
rowView configGroup crypto machineDisplay =
    let
        globalRowClass machine =
            case machine of
                GlobalMachine ->
                    class [ CssClasses.ConfigTableGlobalRow ]

                _ ->
                    class []
    in
        tr [ globalRowClass machineDisplay.machine ]
            ((td [] [ text (machineDisplay.display) ])
                :: (List.map (cellView configGroup crypto machineDisplay.machine)
                        configGroup.schema.entries
                   )
            )


headerCellView : FieldDescriptor -> Html Msg
headerCellView fieldDescriptor =
    td [] [ text fieldDescriptor.display ]


headerRowView : ConfigGroup -> Crypto -> Html Msg
headerRowView configGroup crypto =
    tr [] ((td [] []) :: List.map headerCellView configGroup.schema.entries)


tableView : ConfigGroup -> Crypto -> Html Msg
tableView configGroup crypto =
    let
        headerRow =
            headerRowView configGroup crypto

        machines =
            listMachines configGroup

        rows =
            List.map (rowView configGroup crypto) machines
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
    | Input Crypto Machine String String
    | CryptoSwitch Crypto


selectizeItem : DisplayRec -> Selectize.Item
selectizeItem displayRec =
    let
        code =
            displayRec.code

        itemRec =
            ItemValue code
    in
        Selectize.selectizeItem code displayRec.display []


initCurrencySelectizer : FieldDescriptor -> ConfigGroup -> FieldScope -> Selectizer
initCurrencySelectizer fieldDescriptor configGroup fieldScope =
    let
        availableItems =
            List.map selectizeItem configGroup.data.currencies

        selectizeModel =
            Selectize.init 1 availableItems
    in
        ( fieldScope, selectizeModel )


initAccountSelectizer : String -> FieldDescriptor -> ConfigGroup -> FieldScope -> Selectizer
initAccountSelectizer accountClass fieldDescriptor configGroup fieldScope =
    let
        toDisplayRec accountRec =
            if (accountClass == accountRec.class) then
                Just { code = accountRec.code, display = accountRec.display }
            else
                Nothing

        availableItems =
            List.filterMap toDisplayRec configGroup.data.accounts
                |> List.map selectizeItem

        selectizeModel =
            Selectize.init 1 availableItems
    in
        ( fieldScope, selectizeModel )


initSelectizersPerSchemaEntry : ConfigGroup -> FieldDescriptor -> Maybe (List Selectizer)
initSelectizersPerSchemaEntry configGroup fieldDescriptor =
    case fieldDescriptor.fieldType of
        FieldStringType ->
            Nothing

        FieldPercentageType ->
            Nothing

        FieldIntegerType ->
            Nothing

        FieldOnOffType ->
            Nothing

        FieldAccountType accountCode ->
            Just ((List.map (initAccountSelectizer accountCode fieldDescriptor configGroup) (fieldScopes configGroup)))

        FieldCurrencyType ->
            Just ((List.map (initCurrencySelectizer fieldDescriptor configGroup) (fieldScopes configGroup)))


initSelectizers : ConfigGroup -> List Selectizer
initSelectizers configGroup =
    List.filterMap (initSelectizersPerSchemaEntry configGroup) configGroup.schema.entries
        |> List.concat


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

                selectizers =
                    case webConfigGroup of
                        Success configGroup ->
                            initSelectizers configGroup

                        _ ->
                            []
            in
                ( { model
                    | webConfigGroup = webConfigGroup
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

        Input crypto machine fieldCode valueString ->
            case model.webConfigGroup of
                Success configGroup ->
                    let
                        newConfigGroup =
                            updateValues configGroup crypto machine fieldCode valueString

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
                crypto =
                    Maybe.withDefault GlobalCrypto model.crypto

                configGroupView =
                    div [ class [ CssClasses.ConfigTableContainer ] ]
                        [ tableView configGroup crypto ]

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
