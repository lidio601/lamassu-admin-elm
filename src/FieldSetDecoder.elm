module FieldSetDecoder exposing (..)

import Json.Decode exposing (..)
import FieldSetTypes exposing (..)


fieldValueTypeDecoder : String -> Decoder FieldValue
fieldValueTypeDecoder fieldType =
    case fieldType of
        "string" ->
            map FieldString ("value" := string)

        "password" ->
            succeed (FieldPassword Nothing)

        _ ->
            fail ("Unsupported field type: " ++ fieldType)


fieldValueDecoder : Decoder FieldValue
fieldValueDecoder =
    ("fieldType" := string) `andThen` fieldValueTypeDecoder


fieldStatusValueDecoder : String -> Decoder FieldStatus
fieldStatusValueDecoder statusCode =
    case statusCode of
        "error" ->
            map FieldError ("error" := string)

        "updated" ->
            succeed FieldUpdated

        "idle" ->
            succeed FieldIdle

        _ ->
            fail ("Unsupported status code")


fieldStatusDecoder : Decoder FieldStatus
fieldStatusDecoder =
    ("code" := string) `andThen` fieldStatusValueDecoder


fieldDecoder : Decoder Field
fieldDecoder =
    object7 Field
        ("code" := string)
        ("display" := string)
        ("secret" := bool)
        ("required" := bool)
        ("value" := fieldValueDecoder)
        ("value" := fieldValueDecoder)
        ("status" := fieldStatusDecoder)


fieldSetDecoder =
    object1 FieldSet
        ("fields" := list fieldDecoder)
