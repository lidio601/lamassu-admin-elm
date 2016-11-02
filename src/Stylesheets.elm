port module Stylesheets exposing (..)

import Css.File exposing (..)
import Css.Main
import Html exposing (div)
import Html.App as Html


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "build/styles.css", compile Css.Main.css ) ]


main : Program Never
main =
    Html.program
        { init = ( (), files cssFiles )
        , view = \_ -> (div [] [])
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
