module FieldSet exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import FieldSetTypes exposing (..)
import List


type alias Model =
    Maybe FieldSet


initModel : Model
initModel =
    Nothing



-- UPDATE


type Msg
    = Load FieldSet
    | Input String String


updateField : String -> String -> Field -> Field
updateField fieldCode fieldValueString field =
    if .code field == fieldCode then
        { field | value = updateFieldValue fieldValueString field.value }
    else
        field


updateFieldSet : String -> String -> FieldSet -> FieldSet
updateFieldSet fieldCode fieldValueString fieldSet =
    let
        fields =
            fieldSet.fields

        updatedFields =
            List.map (updateField fieldCode fieldValueString) fields
    in
        { fieldSet | fields = updatedFields }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load fieldSet ->
            Just fieldSet ! []

        Input fieldCode valueString ->
            (Maybe.map (updateFieldSet fieldCode valueString) model) ! []


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


view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            div [] []

        Just fieldSet ->
            let
                fields =
                    List.map fieldView fieldSet.fields
            in
                div []
                    [ fieldset [] fields
                    ]
