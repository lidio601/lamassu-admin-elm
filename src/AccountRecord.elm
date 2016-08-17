module AccountRecord exposing (..)

import Fields exposing (..)


type alias Account =
    { code : String
    , display : String
    , fields : FieldList
    }
