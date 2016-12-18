module FieldSetEncoder exposing (..)

import Json.Encode exposing (..)
import Maybe
import FieldSetTypes exposing (..)


encodeFieldValue : FieldValue -> Maybe String
encodeFieldValue fieldValue =
    case fieldValue of
        FieldString value ->
            Just value

        FieldPassword value ->
            case value of
                Password s ->
                    Just s

                _ ->
                    Nothing


maybeString : Maybe String -> Value
maybeString maybeString =
    case maybeString of
        Nothing ->
            null

        Just s ->
            string s


encodeField : Field -> Maybe Value
encodeField field =
    let
        fieldValue =
            encodeFieldValue field.value
    in
        if isDirty field then
            Just
                (Json.Encode.object
                    [ ( "code", string field.code )
                    , ( "value", maybeString fieldValue )
                    ]
                )
        else
            Nothing


isDirty : Field -> Bool
isDirty field =
    field.value /= field.loadedValue
