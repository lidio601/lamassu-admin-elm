module Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import AccountRecord exposing (..)
import AccountDecoder exposing (..)
import AccountEncoder exposing (..)
import List


type alias AccountResponse =
    RemoteData (Error String) (Response Account)


type alias WebAccount =
    RemoteData (Error String) Account


type alias Model =
    WebAccount


getForm : String -> Cmd Msg
getForm code =
    get ("http://localhost:8093/account/" ++ code)
        |> send (jsonReader accountDecoder) stringReader
        |> RemoteData.asCmd
        |> Cmd.map Load


postForm : Account -> Cmd Msg
postForm account =
    post "http://localhost:8093/account"
        |> withHeader "Content-Type" "application/json"
        |> withJsonBody (encodeAccount account)
        |> send (jsonReader accountDecoder) stringReader
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
    = Load AccountResponse
    | Submit
    | Input String String



-- TODO: Make it do something


updateField : Model -> String -> String -> Model
updateField model fieldCode fieldValueString =
    model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load accountResponse ->
            let
                wrapper response =
                    ( response.data, Cmd.none )
            in
                RemoteData.update wrapper accountResponse

        Input fieldCode valueString ->
            ( updateField model fieldCode valueString, Cmd.none )

        Submit ->
            ( Debug.log "DEBUG12" model, Cmd.none )


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
            accountView account


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


accountView : Account -> Html Msg
accountView account =
    let
        fields =
            List.map fieldView account.fields
    in
        div []
            [ div [] [ text ("Account: " ++ account.display) ]
            , Html.form [ onSubmit Submit ]
                [ fieldset [] fields
                , button [] [ text "Submit" ]
                ]
            ]
