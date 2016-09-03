module Config exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App
import Html.Events exposing (onClick)
import Navigation
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (..)
import ConfigEncoder exposing (..)
import ConfigGroup
import Html.CssHelpers
import CssClasses
import Selectize


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


type SelectizeItem
    = Selectize.Item ItemRec


type alias SelectizeModel =
    Selectize.Model ItemRec


type alias Selectizer =
    ( FieldLocator, SelectizeModel )


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


initModel : Model
initModel =
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


type Msg
    = Load ConfigGroupResponse
    | Submit
    | ConfigGroupMsg ConfigGroup.Msg
    | CryptoSwitch Crypto


selectizeItem : DisplayRec -> SelectizeItem
selectizeItem displayRec =
    let
        code =
            displayRec.code

        itemRec =
            ItemValue code
    in
        Selectize.selectizeItem itemRec code displayRec.display []


buildAccountSelectizers : ConfigGroup -> FieldDescriptor -> List Selectizer
buildAccountSelectizers configGroup fieldDescriptor =
    let
        machines = listMachines configGroup
        cryptos = listCryptos configGroup


buildSelectizers : ConfigGroup -> FieldDescriptor -> Maybe (List Selectizer)
buildSelectizers configGroup fieldDescriptor =
    case fieldDescriptor.fieldType of
        FieldStringType ->
            Nothing

        FieldPercentageType ->
            Nothing

        FieldIntegerType ->
            Nothing

        FieldOnOffType ->
            Nothing

        FieldAccountType ->
            Just (buildAccountSelectizer configGroup fieldDescriptor)

        FieldCurrencyType ->
            Just (buildCurrencySelectizer configGroup fieldDescriptor)


populateSelectizers : ConfigGroupResponse -> List SelectizeModel
populateSelectizers configGroup =
    List.map (buildSelectizers configGroup) configGroup.schema.entries


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
                    RemoteData.map populateSelectizers webConfigGroup
                        |> RemoteData.withDefault []
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

        ConfigGroupMsg configGroupMsg ->
            case model.webConfigGroup of
                Success configGroup ->
                    let
                        ( configGroupModel, configGroupCmd ) =
                            ConfigGroup.update configGroupMsg configGroup

                        webConfigGroup =
                            Success configGroupModel
                    in
                        { model
                            | webConfigGroup = webConfigGroup
                            , status = Editing
                        }
                            ! [ Cmd.map ConfigGroupMsg configGroupCmd ]

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
        cryptos = listCryptos configGroup
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
                configGroupView =
                    Html.App.map ConfigGroupMsg (ConfigGroup.view configGroup model.crypto)

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
