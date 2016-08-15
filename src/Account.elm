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


type alias WebRecord x =
    RemoteData (Error String) (Response x)


type alias WebAccount =
    WebRecord Account


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
    = Load WebAccount
    | Submit
    | Input String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load webAccount ->
            let
                wrapper response =
                    ( response, Cmd.none )
            in
                RemoteData.update wrapper webAccount

        Input fieldCode valueString ->
            let
                updatedModel =
                    case model of
                        Success response ->
                            let
                                account =
                                    response.data

                                fields =
                                    account.fields

                                updateField f =
                                    if .code f == fieldCode then
                                        { f | value = FieldString valueString }
                                    else
                                        f

                                newFields =
                                    List.map updateField fields
                            in
                                model

                        _ ->
                            model
            in
                ( updatedModel, Cmd.none )

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

        Success accountResponse ->
            accountView accountResponse


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


accountView : Response Account -> Html Msg
accountView accountResponse =
    let
        account =
            accountResponse.data

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
