module AccountEncoder exposing (..)

import Json.Encode exposing (..)
import AccountRecord exposing (..)


encode : Account -> String
encode account =
    Json.Encode.encode 0 (Json.Encode.object [ ( "x", string "x" ) ])
