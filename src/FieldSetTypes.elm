module FieldSetTypes exposing (..)


type alias Field =
    { code : String
    , display : String
    , required : Bool
    , value : FieldValue
    , loadedValue : FieldValue
    }


type FieldPasswordType
    = Password String
    | PasswordEmpty
    | PasswordHidden


type FieldValue
    = FieldString String
    | FieldPassword FieldPasswordType


updateFieldValue : String -> FieldValue -> FieldValue
updateFieldValue stringValue oldFieldValue =
    case oldFieldValue of
        FieldString _ ->
            FieldString stringValue

        FieldPassword _ ->
            FieldPassword (Password stringValue)
