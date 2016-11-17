module FieldSetTable exposing (Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import FieldSetTypes exposing (..)
import List


type alias Model a b =
    FieldSetTable a b



-- UPDATE


type Msg a b
    = Input a b String String


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


update : Msg -> Model a b -> ( Model a b, Cmd Msg )
update msg model =
    case msg of
        Input fieldCode valueString ->
            updateFieldSet fieldCode valueString model ! []


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
                    [ onInput (Input field.code), type_ "password" ]
                    []
                ]


fieldView : Field -> Html Msg
fieldView field =
    div [] [ fieldComponent field ]


view : Model -> Html Msg
view model =
    let
        fields =
            List.map fieldView model.fields
    in
        div []
            [ fieldset [] fields
            ]
