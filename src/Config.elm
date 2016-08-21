module Config exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (..)
import ConfigEncoder exposing (..)
import ConfigGroup


type alias ConfigGroupResponse =
    RemoteData (Error String) (Response ConfigGroup)


type alias WebConfigGroup =
    RemoteData (Error String) ConfigGroup


type alias Model =
    WebConfigGroup


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
    RemoteData.NotAsked


load : String -> ( Model, Cmd Msg )
load code =
    ( RemoteData.Loading, getForm code )



-- UPDATE


type Msg
    = Load ConfigGroupResponse
    | Submit
    | ConfigGroupMsg ConfigGroup.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load configGroupResponse ->
            ( RemoteData.map .data configGroupResponse, Cmd.none )

        Submit ->
            case model of
                Success configGroup ->
                    Debug.log "DEBUG1" model ! [ postForm configGroup ]

                _ ->
                    model ! []

        ConfigGroupMsg configGroupMsg ->
            let
                mapper : ConfigGroup -> ( ConfigGroup, Cmd Msg )
                mapper configGroup =
                    let
                        ( configGroupModel, configGroupCmd ) =
                            ConfigGroup.update configGroupMsg configGroup
                    in
                        configGroupModel ! [ Cmd.map ConfigGroupMsg configGroupCmd ]
            in
                RemoteData.update mapper model


view : Model -> Html Msg
view model =
    case model of
        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success configGroup ->
            let
                configGroupView =
                    Html.App.map ConfigGroupMsg (ConfigGroup.view configGroup)
            in
                div []
                    [ div [] [ text configGroup.display ]
                    , Html.form [ onSubmit Submit ]
                        [ div [] [ configGroupView ]
                        , button [] [ text "Submit" ]
                        ]
                    ]
