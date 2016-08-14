module Account exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (..)
import AccountRecord exposing (..)
import HttpBuilder exposing (..)


type alias Model =
    WebRecord


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
        |> withJsonBody account
        |> send (jsonReader accountDecoder) stringReader
        |> RemoteData.asCmd
        |> Cmd.map Load


initModel : Model
initModel =
    RemoteData.NotAsked


load : String -> ( Model, Cmd Msg )
load code =
    ( RemoteData.Loading, get code )



-- UPDATE


type alias WebRecord =
    RemoteData (Error String) (Response Account)


type Msg
    = Load WebRecord
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load webdata ->
            let
                decodingMapper json =
                    ( decode json, Cmd.none )
            in
                RemoteData.update decodingMapper webdata

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

        Success accountResult ->
            case accountResult of
                Ok account ->
                    accountView account

                Err err ->
                    let
                        _ =
                            Debug.log "DEBUG11" err
                    in
                        div [] [ text "Server error (bad record)" ]


fieldComponent : Field -> Html Msg
fieldComponent field =
    case field.value of
        FieldString string ->
            label []
                [ text field.display
                , input
                    [ value string ]
                    []
                ]

        FieldPassword _ ->
            label []
                [ text field.display
                , input
                    [ type' "password" ]
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
