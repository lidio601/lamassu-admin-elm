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


fieldDecoder : Decoder Field
fieldDecoder =
    object5 Field
        ("code" := string)
        ("display" := string)
        ("secret" := bool)
        ("required" := bool)
        ("value" := fieldValueDecoder)


accountDecoder : Decoder Account
accountDecoder =
    object3 Account
        ("code" := string)
        ("display" := string)
        ("fields" := list fieldDecoder)


type alias AccountResult =
    Result String Account


decode : String -> AccountResult
decode string =
    decodeString accountDecoder string
