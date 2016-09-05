module Css.Selectize exposing (..)

import Css exposing (..)
import Colors
import Selectize
import Css.Admin exposing (..)


type Class
    = SelectizeContainer
    | SelectBox
    | BoxItems
    | SelectedItems


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
