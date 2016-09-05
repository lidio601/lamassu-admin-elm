module Css.Selectize exposing (..)

import Css exposing (..)
import Colors
import Selectize


type Class
    = SelectizeContainer
    | SelectBox
    | BoxItems


classes : Selectize.HtmlClasses
classes =
    { container = className SelectizeContainer
    , selectedItems = className SelectedItems
    , selectedItem = "selectedItem"
    , boxItems = "boxItems"
    , boxItem = "boxItem"
    , boxItemActive = "activeBoxItem"
    , instructionsForBlank = "instructions"
    }


component : Mixin
component =
    mixin
        [ backgroundColor Colors.white
        , borderRadius (px 3)
        , position relative
        , children
            [ (.) SelectBox
                [ displayFlex
                ]
            , (.) BoxItems
                [ position absolute ]
            ]
        ]
