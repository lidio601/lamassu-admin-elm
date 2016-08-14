module Account exposing (..)

import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (..)
import AccountRecord exposing (..)


type alias Model =
    WebData AccountResult


get : String -> Cmd Msg
get code =
    Http.getString ("http://localhost:8093/account/" ++ code)
        |> RemoteData.asCmd


initModel : Model
initModel =
    RemoteData.NotAsked


load : String -> ( Model, Cmd Msg )
load code =
    ( RemoteData.Loading, get code )



-- UPDATE


type alias Msg =
    WebData String


update : Msg -> Model -> ( Model, Cmd Msg )
update webdata model =
    let
        decodingMapper json =
            ( decode json, Cmd.none )
    in
        RemoteData.update decodingMapper webdata


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
            , fieldset [] fields
            ]
