module AccountsDecoder exposing (..)

import Json.Decode exposing (..)


accountDecoder : Decoder ( String, String )
accountDecoder =
    object2 (,)
        ("code" := string)
        ("display" := string)


accountsDecoder : Decoder (List ( String, String ))
accountsDecoder =
    object1 identity
        ("accounts" := list accountDecoder)
