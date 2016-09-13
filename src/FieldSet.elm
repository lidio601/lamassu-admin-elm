module FieldSet exposing (Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import FieldSetTypes exposing (..)
import List


type alias Model =
    List Field



-- UPDATE


type Msg
    = Input String String


updateField : String -> String -> Field -> Field
updateField fieldCode fieldValueString field =
    if .code field == fieldCode then
        { field | value = updateFieldValue fieldValueString field.value }
    else
        field


updateFieldSet : String -> String -> List Field -> List Field
updateFieldSet fieldCode fieldValueString fields =
    List.map (updateField fieldCode fieldValueString) fields


update : Msg -> Model -> ( Model, Cmd Msg )
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
                    [ onInput (Input field.code), type' "password" ]
                    []
                ]


fieldView : Field -> Html Msg
fieldView field =
    div [] [ fieldComponent field ]


view : Model -> Html Msg
view model =
    let
        fields =
            List.map fieldView model
    in
        div []
            [ fieldset [] fields
            ]
