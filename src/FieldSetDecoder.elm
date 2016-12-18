module FieldSetDecoder exposing (..)

import Json.Decode exposing (..)
import FieldSetTypes exposing (..)


fieldPasswordDecoder : Bool -> FieldValue
fieldPasswordDecoder present =
    if present then
        FieldPassword PasswordHidden
    else
        FieldPassword PasswordEmpty


fieldValueDecoder : String -> Decoder FieldValue
fieldValueDecoder fieldType =
    case fieldType of
        "string" ->
            map FieldString string

        "password" ->
            map fieldPasswordDecoder bool

        _ ->
            fail ("Unsupported field type: " ++ fieldType)


fieldDecoder : Decoder Field
fieldDecoder =
    (field "fieldType" string)
        |> andThen
            (\fieldType ->
                map5 Field
                    (field "code" string)
                    (field "display" string)
                    (field "required" bool)
                    (field "value" (fieldValueDecoder fieldType))
                    (field "value" (fieldValueDecoder fieldType))
            )
