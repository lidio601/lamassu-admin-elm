module AccountEncoder exposing (..)

import Json.Encode exposing (..)
import AccountTypes exposing (..)
import List
import FieldSetEncoder exposing (..)


encodeAccount : Account -> Value
encodeAccount account =
    Json.Encode.object
        [ ( "code", string account.code )
        , ( "display", string account.display )
        , ( "fieldSet", encodeFieldSet account.fieldSet )
        ]
