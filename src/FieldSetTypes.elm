module FieldSetTypes exposing (..)


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


type alias FieldSet =
    { fields : List Field
    }



-- Note: We need makeFieldSetRow and makeFieldSetTable because Elm currently
-- does not auto-generate object creators for extended records. See:
--   https://groups.google.com/forum/#!topic/elm-discuss/Xu21kJMAujQ
--   https://github.com/elm-lang/error-message-catalog/issues/67


makeFieldSetRow : a -> FieldSet -> FieldSetRow a
makeFieldSetRow id fieldSet =
    { id = id
    , fieldSet = fieldSet
    }


type alias FieldSetRow a =
    { id : a
    , fieldSet : FieldSet
    }


makeFieldSetTable : a -> List (FieldSetRow b) -> FieldSetTable a b
makeFieldSetTable id rows =
    { id = id
    , rows = rows
    }


type alias FieldSetTable a b =
    { id : a
    , rows : List (FieldSetRow b)
    }


updateFieldValue : String -> FieldValue -> FieldValue
updateFieldValue stringValue oldFieldValue =
    case oldFieldValue of
        FieldString _ ->
            FieldString stringValue

        FieldPassword _ ->
            FieldPassword (Just stringValue)
