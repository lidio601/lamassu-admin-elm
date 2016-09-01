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
import AdminCss


{ id, class, classList } =
    Html.CssHelpers.withNamespace "lamassuAdmin"
type alias ConfigGroupResponse =
    RemoteData (Error String) (Response ConfigGroup)


type alias WebConfigGroup =
    RemoteData (Error String) ConfigGroup


type alias Model =
    { webConfigGroup : WebConfigGroup
    , crypto : Maybe Crypto
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
    { webConfigGroup = RemoteData.NotAsked, crypto = Nothing }


load : Model -> String -> ( Model, Cmd Msg )
load model code =
    ( model, getForm code )



-- UPDATE


type Msg
    = Load ConfigGroupResponse
    | Submit
    | ConfigGroupMsg ConfigGroup.Msg
    | CryptoSwitch Crypto


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load configGroupResponse ->
            ( { model | webConfigGroup = RemoteData.map .data configGroupResponse }, Cmd.none )

        Submit ->
            case model.webConfigGroup of
                Success configGroup ->
                    Debug.log "DEBUG1" model ! [ postForm configGroup ]

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
                        { model | webConfigGroup = webConfigGroup } ! [ Cmd.map ConfigGroupMsg configGroupCmd ]

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
                        class [ AdminCss.CryptoTabsActive ]
                    else
                        class []
    in
        div [ activeClass, onClick (CryptoSwitch cryptoDisplay.crypto) ] [ text cryptoDisplay.display ]


cryptosView : Maybe Crypto -> ConfigGroup -> Html Msg
cryptosView activeCrypto configGroup =
    let
        cryptos =
            if (configGroup.schema.cryptoScope == Specific) then
                configGroup.data.cryptos
            else
                globalCryptoDisplay :: configGroup.data.cryptos
    in
        nav [ class [ AdminCss.CryptoTabs ] ] (List.map (cryptoView activeCrypto) cryptos)


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
            in
                if (configGroup.schema.cryptoScope == Global) then
                    div []
                        [ div [ class [ AdminCss.ConfigGroupLabel ] ] [ text configGroup.schema.display ]
                        , Html.form []
                            [ div [] [ configGroupView ]
                            , div [ onClick Submit, class [ AdminCss.Button ] ] [ text "Submit" ]
                            ]
                        ]
                else
                    div []
                        [ div [ class [ AdminCss.ConfigGroupLabel ] ] [ text configGroup.schema.display ]
                        , div [] [ (cryptosView model.crypto configGroup) ]
                        , Html.form []
                            [ div [] [ configGroupView ]
                            , div [ onClick Submit, class [ AdminCss.Button ] ] [ text "Submit" ]
                            ]
                        ]
