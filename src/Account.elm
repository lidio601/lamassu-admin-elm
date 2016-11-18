module Account exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (..)
import Http
import HttpBuilder exposing (..)
import AccountTypes exposing (..)
import AccountDecoder exposing (..)
import AccountEncoder exposing (..)
import FieldSet
import Css.Admin exposing (..)
import Css.Classes


type alias Model =
    RemoteData.WebData Account


getForm : String -> Cmd Msg
getForm code =
    get ("/api/account/" ++ code)
        |> withExpect (Http.expectJson accountDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map Load


postForm : Account -> Cmd Msg
postForm account =
    post "/api/account"
        |> withJsonBody (encodeAccount account)
        |> withExpect (Http.expectJson accountDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map Load


init : Model
init =
    RemoteData.NotAsked


load : String -> ( Model, Cmd Msg )
load code =
    ( RemoteData.Loading, getForm code )



-- UPDATE


type Msg
    = Load Model
    | Submit
    | FieldSetMsg FieldSet.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load newModel ->
            ( newModel, Cmd.none )

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
                    Html.map FieldSetMsg (FieldSet.view account.fields)
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
