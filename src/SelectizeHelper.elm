module Config exposing (LocalConfig)

import Selectize exposing (..)
import Css.Selectize


type alias LocalConfig msg idType itemType =
    { toMsg : State -> msg
    , onAdd : idType -> State -> msg
    , onRemove : State -> msg
    , onFocus : State -> msg
    , onBlur : State -> msg
    , toId : itemType -> idType
    , selectedDisplay : itemType -> String
    , optionDisplay : itemType -> String
    , maxItems : Int
    , match : String -> List itemType -> List itemType
    }


buildConfig :
    LocalConfig msg idType itemType
    -> Config msg idType itemType
buildConfig config =
    { maxItems = config.maxItems
    , boxLength = 5
    , toMsg = config.toMsg
    , onAdd = config.onAdd
    , onRemove = config.onRemove
    , onFocus = config.onFocus
    , onBlur = config.onBlur
    , toId = config.toId
    , selectedDisplay = config.selectedDisplay
    , optionDisplay = config.optionDisplay
    , match = config.match
    , htmlOptions =
        { instructionsForBlank = "Start typing for options"
        , noMatches = "No matches"
        , atMaxLength = "Type backspace to edit"
        , typeForMore = "Type for more options"
        , noOptions = "No options"
        , classes = Css.Selectize.classes
        }
    }
