module AccountDecoder exposing (..)

import Json.Decode exposing (..)
import FieldSetDecoder exposing (..)
import AccountTypes exposing (..)


accountDecoder : Decoder Account
accountDecoder =
    object3 Account
        ("code" := string)
        ("display" := string)
        ("fieldSet" := fieldSetDecoder)


type alias AccountResult =
    Result String Account


decodeAccount : String -> AccountResult
decodeAccount string =
    decodeString accountDecoder string
