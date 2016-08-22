module FieldSetEncoder exposing (..)

import Json.Encode exposing (..)
import List
import Maybe
import FieldSetTypes exposing (..)


encodeFieldValue : FieldValue -> Value
encodeFieldValue fieldValue =
    let
        list =
            case fieldValue of
                FieldString value ->
                    [ ( "fieldType", string "string" )
                    , ( "value", string value )
                    ]

                FieldPassword value ->
                    let
                        jsonValue =
                            case value of
                                Just s ->
                                    string s

                                Nothing ->
                                    null
                    in
                        [ ( "fieldType", string "password" )
                        , ( "value", jsonValue )
                        ]

                FieldPercentage value ->
                    [ ( "fieldType", string "percentage" )
                    , ( "value", int value )
                    ]
    in
        Json.Encode.object list


encodeField : Field -> Value
encodeField field =
    -- No need to encode status or loadedValue field, they're for client-side only
    Json.Encode.object
        [ ( "code", string field.code )
        , ( "display", string field.display )
        , ( "secret", bool field.secret )
        , ( "required", bool field.required )
        , ( "value", encodeFieldValue field.value )
        ]


isDirty : Field -> Bool
isDirty field =
    field.value /= field.loadedValue



-- Encodes only changed fields


encodeFieldSet : FieldSet -> Value
encodeFieldSet fieldSet =
    let
        fieldValues =
            List.filter isDirty fieldSet.fields
                |> List.map encodeField
    in
        Json.Encode.object
            [ ( "fields", list fieldValues ) ]
