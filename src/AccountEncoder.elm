module AccountEncoder exposing (..)

import Json.Encode exposing (..)
import AccountRecord exposing (..)


encodeAccount : Account -> Value
encodeAccount account =
    Json.Encode.object [ ( "x", string "x" ) ]
