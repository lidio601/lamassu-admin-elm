module FieldSetDecoder exposing (..)

import Json.Decode exposing (..)
import FieldSetTypes exposing (..)


fieldValueDecoder : String -> Decoder FieldValue
fieldValueDecoder fieldType =
    case fieldType of
        "string" ->
            map FieldString string

        "password" ->
            succeed (FieldPassword Nothing)

        _ ->
            fail ("Unsupported field type: " ++ fieldType)


fieldDecoder : Decoder Field
fieldDecoder =
    ("fieldType" := string)
        `andThen`
            (\fieldType ->
                object6 Field
                    ("code" := string)
                    ("display" := string)
                    ("secret" := bool)
                    ("required" := bool)
                    ("value" := fieldValueDecoder fieldType)
                    ("value" := fieldValueDecoder fieldType)
            )
