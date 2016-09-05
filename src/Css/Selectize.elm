module Css.Selectize exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, a, div, td, thead, tbody, input, button)
import Css.Namespace exposing (namespace)
import Css.Helpers
import Html
import Html.CssHelpers
import Colors
import Css.ColorSchemes exposing (..)
import Css.Classes exposing (..)


component : Mixin
component =
    mixin
        [ backgroundColor (hex "f00")
        ]
