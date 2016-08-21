module Config exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (..)
import ConfigEncoder exposing (..)
import FieldSet


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
    | FieldSetMsg FieldSet.Msg


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

        FieldSetMsg fieldSetMsg ->
            let
                mapper account =
                    let
                        ( fieldSet, fieldSetCmd ) =
                            FieldSet.update fieldSetMsg account.fieldSet
                    in
                        { account | fieldSet = fieldSet } ! [ Cmd.map FieldSetMsg fieldSetCmd ]
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

        Success account ->
            let
                fieldSetView =
                    Html.App.map FieldSetMsg (FieldSet.view account.fieldSet)
            in
                div []
                    [ div [] [ text ("Account: " ++ account.display) ]
                    , Html.form [ onSubmit Submit ]
                        [ fieldset [] [ fieldSetView ]
                        , button [] [ text "Submit" ]
                        ]
                    ]
