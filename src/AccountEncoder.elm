module AccountEncoder exposing (..)

import Json.Encode exposing (..)
import AccountRecord exposing (..)
import List
import Maybe


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
    in
        Json.Encode.object list


encodeField : Field -> Value
encodeField field =
    -- No need to encode status field, it's for client-side only
    Json.Encode.object
        [ ( "code", string field.code )
        , ( "display", string field.display )
        , ( "secret", bool field.secret )
        , ( "required", bool field.required )
        , ( "value", encodeFieldValue field.value )
        ]


encodeAccount : Account -> Value
encodeAccount account =
    Json.Encode.object
        [ ( "code", string account.code )
        , ( "display", string account.display )
        , ( "fields", list (List.map encodeField account.fields) )
        ]
