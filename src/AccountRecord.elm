module AccountRecord exposing (..)

import Json.Decode exposing (..)
import Dict exposing (..)


type FieldType
    = FieldString


fieldTypeDict : Dict.Dict String FieldType
fieldTypeDict =
    Dict.fromList [ ( "string", FieldString ) ]


type alias Field =
    { code : String
    , display : String
    , fieldType : FieldType
    , secret : Bool
    , required : Bool
    }


type FieldValue
    = FieldStringValue String


type alias FieldValues =
    Dict String FieldValue


type alias Account =
    { code : String
    , display : String
    , fields : List Field
    , fieldValues : FieldValues
    }



-- Decoders


fieldTypeDecoder : Decoder FieldType
fieldTypeDecoder =
    customDecoder string fieldTypeMap


fieldTypeMap : String -> Result String FieldType
fieldTypeMap fieldTypeString =
    let
        fieldTypeMaybe =
            Dict.get fieldTypeString fieldTypeDict

        error =
            fieldTypeString ++ " is not a supported field type"
    in
        Result.fromMaybe error fieldTypeMaybe


fieldDecoder : Decoder Field
fieldDecoder =
    object5 Field
        ("code" := string)
        ("display" := string)
        ("type" := fieldTypeDecoder)
        ("secret" := bool)
        ("required" := bool)


fieldValuesDecoder : Decoder FieldValues
fieldValuesDecoder =
    dict fieldValueDecoder


accountDecoder : Decoder Account
accountDecoder =
    object4 Account
        ("code" := string)
        ("display" := string)
        ("fields" := list fieldDecoder)
        ("fieldValues" := fieldValuesDecoder)


type alias AccountResult =
    Result String Account


decode : String -> AccountResult
decode string =
    decodeString accountDecoder string
