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
    (field "fieldType" string)
        |> andThen
            (\fieldType ->
                map6 Field
                    (field "code" string)
                    (field "display" string)
                    (field "secret" bool)
                    (field "required" bool)
                    (field "value" (fieldValueDecoder fieldType))
                    (field "value" (fieldValueDecoder fieldType))
            )
