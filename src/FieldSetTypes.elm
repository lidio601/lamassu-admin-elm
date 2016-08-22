module FieldSetTypes exposing (..)

import Result
import String


type FieldStatus
    = FieldUpdated
    | FieldError String
    | FieldIdle


type alias Field =
    { code : String
    , display : String
    , secret : Bool
    , required : Bool
    , value : FieldValue
    , loadedValue : FieldValue
    , status : FieldStatus
    }


type FieldValue
    = FieldString String
    | FieldPassword (Maybe String)
    | FieldPercentage Int


type alias FieldSet =
    { fields : List Field
    }


updateFieldValue : String -> FieldValue -> FieldValue
updateFieldValue stringValue oldFieldValue =
    case oldFieldValue of
        FieldString _ ->
            FieldString stringValue

        FieldPassword _ ->
            FieldPassword (Just stringValue)

        FieldPercentage oldPct ->
            String.toInt stringValue
                |> Result.withDefault 0
                |> FieldPercentage
