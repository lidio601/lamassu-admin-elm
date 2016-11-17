module FieldSet exposing (Msg, update, view)

import Html exposing (..)
import Html.Attributes exposing (value, type_)
import Html.Events exposing (..)
import FieldSetTypes exposing (..)
import List
import Css.Admin exposing (..)
import Css.Classes as C


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
    let
        inputEl =
            case field.value of
                FieldString string ->
                    input
                        [ onInput (Input field.code), value string ]
                        []

                FieldPassword _ ->
                    input
                        [ onInput (Input field.code), type_ "password" ]
                        []
    in
        label []
            [ div [] [ text field.display ]
            , inputEl
            ]


fieldView : Field -> Html Msg
fieldView field =
    div [ class [ C.FormRow ] ] [ fieldComponent field ]


view : Model -> Html Msg
view model =
    div [ class [ C.ConfigContainer ] ] (List.map fieldView model)
