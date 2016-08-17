module Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import Fields exposing (..)
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


updateField : String -> String -> Field -> Field
updateField fieldCode fieldValueString field =
    if .code field == fieldCode then
        { field | value = updateFieldValue fieldValueString field.value }
    else
        field


updateAccountField : String -> String -> Account -> Account
updateAccountField fieldCode fieldValueString account =
    let
        fields =
            account.fields

        updatedFields =
            List.map (updateField fieldCode fieldValueString) fields
    in
        { account | fields = updatedFields }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load accountResponse ->
            ( RemoteData.map .data accountResponse, Cmd.none )

        Input fieldCode valueString ->
            ( RemoteData.map (updateAccountField fieldCode valueString) model, Cmd.none )

        Submit ->
            let
                mapper account =
                    ( account, postForm account )
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
