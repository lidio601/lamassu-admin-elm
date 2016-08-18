module Account exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import AccountTypes exposing (..)
import AccountDecoder exposing (..)
import AccountEncoder exposing (..)
import FieldSet


type alias AccountResponse =
    RemoteData (Error String) (Response Account)


type alias WebAccount =
    RemoteData (Error String) Account


type alias Model =
    { webAccount : WebAccount
    , fieldSet : FieldSet.Model
    }


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
    { webAccount = RemoteData.NotAsked
    , fieldSet = FieldSet.initModel
    }


load : String -> Model -> ( Model, Cmd Msg )
load code model =
    ( { model | webAccount = RemoteData.Loading }, getForm code )



-- UPDATE


type Msg
    = Load AccountResponse
    | Submit
    | FieldSetMsg FieldSet.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load accountResponse ->
            ( { model | webAccount = RemoteData.map .data accountResponse }, Cmd.none )

        Submit ->
            case model.webAccount of
                Success account ->
                    model ! [ postForm account ]

                _ ->
                    model ! []

        FieldSetMsg fieldSetMsg ->
            let
                ( fieldSetModel, cmd ) =
                    FieldSet.update fieldSetMsg model.fieldSet
            in
                { model | fieldSet = fieldSetModel } ! [ Cmd.map FieldSetMsg cmd ]


view : Model -> Html Msg
view model =
    case model.webAccount of
        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success account ->
            let
                fieldSetView =
                    Html.App.map FieldSetMsg (FieldSet.view model.fieldSet)
            in
                div []
                    [ div [] [ text ("Account: " ++ account.display) ]
                    , Html.form [ onSubmit Submit ]
                        [ fieldset [] [ fieldSetView ]
                        , button [] [ text "Submit" ]
                        ]
                    ]
