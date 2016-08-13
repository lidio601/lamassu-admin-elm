module AccountRecord exposing (..)

import Json.Decode exposing (..)


type alias Field =
    { code : String
    , display : String
    , secret : Bool
    , required : Bool
    , value : FieldValue
    }


type FieldValue
    = FieldString (Maybe String)


type alias Account =
    { code : String
    , display : String
    , fields : List Field
    }



-- Decoders


nullOr : Decoder a -> Decoder (Maybe a)
nullOr decoder =
    oneOf
        [ null Nothing
        , map Just decoder
        ]


fieldValueTypeDecoder : String -> Decoder FieldValue
fieldValueTypeDecoder fieldType =
    case fieldType of
        "string" ->
            map FieldString (nullOr string)

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
