module ConfigCrypto exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.App
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import ConfigTypes exposing (..)
import ConfigDecoder exposing (..)
import ConfigEncoder exposing (..)
import FieldSet


type alias Model =
    CryptoConfig



-- UPDATE


type Msg
    = FieldSetMsg FieldSet.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FieldSetMsg fieldSetMsg ->
            let
                mapper cryptoConfig =
                    let
                        ( fieldSet, fieldSetCmd ) =
                            FieldSet.update fieldSetMsg .fieldSet
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
