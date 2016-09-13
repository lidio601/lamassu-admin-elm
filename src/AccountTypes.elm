module AccountTypes exposing (..)

import FieldSetTypes exposing (..)


type alias Account =
    { code : String
    , display : String
    , fields : List Field
    }
