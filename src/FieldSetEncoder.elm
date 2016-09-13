module FieldSetEncoder exposing (..)

import Json.Encode exposing (..)
import List
import Maybe
import FieldSetTypes exposing (..)


encodeFieldValue : FieldValue -> Maybe String
encodeFieldValue fieldValue =
    case fieldValue of
        FieldString value ->
            Just value

        FieldPassword value ->
            case value of
                Just s ->
                    Just s

                Nothing ->
                    Nothing


maybeString : Maybe String -> Value
maybeString maybeString =
    case maybeString of
        Nothing ->
            null

        Just s ->
            string s


encodeField : Field -> Value
encodeField field =
    let
        fieldValue =
            encodeFieldValue field.value
    in
        Json.Encode.object
            [ ( "code", string field.code )
            , ( "value", maybeString fieldValue )
            ]


isDirty : Field -> Bool
isDirty field =
    field.value /= field.loadedValue



-- Encodes only changed fields


encodeFieldSet : List Field -> Value
encodeFieldSet fields =
    let
        fieldValues =
            List.filter isDirty fields
                |> List.map encodeField
    in
        Json.Encode.object
            [ ( "fields", list fieldValues ) ]
