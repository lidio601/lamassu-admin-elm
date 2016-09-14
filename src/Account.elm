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
import Css.Admin exposing (..)
import Css.Classes


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


init : Model
init =
    RemoteData.NotAsked


load : String -> ( Model, Cmd Msg )
load code =
    ( RemoteData.Loading, getForm code )



-- UPDATE


type Msg
    = Load AccountResponse
    | Submit
    | FieldSetMsg FieldSet.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load accountResponse ->
            ( RemoteData.map .data accountResponse, Cmd.none )

        Submit ->
            case model of
                Success account ->
                    Debug.log "DEBUG1" model ! [ postForm account ]

                _ ->
                    model ! []

        FieldSetMsg fieldSetMsg ->
            let
                mapper account =
                    let
                        ( fields, fieldSetCmd ) =
                            FieldSet.update fieldSetMsg account.fields
                    in
                        { account | fields = fields } ! [ Cmd.map FieldSetMsg fieldSetCmd ]
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
                    Html.App.map FieldSetMsg (FieldSet.view account.fields)
            in
                div []
                    [ div [ class [ Css.Classes.SectionLabel ] ] [ text account.display ]
                    , div []
                        [ fieldSetView
                        , div [ class [ Css.Classes.ButtonRow ] ]
                            [ div [ onClick Submit, class [ Css.Classes.Button ] ] [ text "Submit" ]
                            ]
                        ]
                    ]
