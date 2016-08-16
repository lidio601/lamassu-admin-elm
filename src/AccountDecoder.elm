module AccountDecoder exposing (..)

import Json.Decode exposing (..)
import AccountRecord exposing (..)


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


fieldStatusDecoder =
    ("code" := string) `andThen` fieldStatusValueDecoder


fieldDecoder : Decoder Field
fieldDecoder =
    object6 Field
        ("code" := string)
        ("display" := string)
        ("secret" := bool)
        ("required" := bool)
        ("value" := fieldValueDecoder)
        ("status" := fieldStatusDecoder)


accountDecoder : Decoder Account
accountDecoder =
    object3 Account
        ("code" := string)
        ("display" := string)
        ("fields" := list fieldDecoder)


decodeAccount : String -> AccountResult
decodeAccount string =
    decodeString accountDecoder string
