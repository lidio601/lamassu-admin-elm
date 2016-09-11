module Config exposing (..)

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
    }


localConfig : LocalConfig msg idType itemType -> Config msg idType itemType
localConfig config =
    { maxItems = 1
    , boxLength = 5
    , toMsg = config.toMsg
    , onAdd = config.onAdd
    , onRemove = config.onRemove
    , onFocus = config.onFocus
    , onBlur = config.onBlur
    , toId = config.toId
    , selectedDisplay = config.selectedDisplay
    , optionDisplay = config.optionDisplay
    , match = (\_ _ -> [])
    , htmlOptions =
        { instructionsForBlank = "Start typing for options"
        , noMatches = "No matches"
        , atMaxLength = "Type backspace to edit"
        , typeForMore = "Type for more options"
        , noOptions = "No options"
        , classes = Css.Selectize.classes
        }
    }
