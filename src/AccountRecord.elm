module AccountRecord exposing (..)


type alias Field =
    { code : String
    , display : String
    , secret : Bool
    , required : Bool
    , value : FieldValue
    }


type FieldValue
    = FieldString String
    | FieldPassword (Maybe String)


type alias Account =
    { code : String
    , display : String
    , fields : List Field
    }
