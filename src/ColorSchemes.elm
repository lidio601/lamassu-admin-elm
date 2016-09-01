module ColorSchemes exposing (..)

import Css exposing (..)
import Colors
import CssClasses exposing (..)


type alias ColorScheme =
    { bg : Color
    , fg : Color
    , bgHover : Color
    , fgActive : Color
    }


darkGreyScheme =
    { bg = Colors.darkGrey
    , fg = Colors.sandstone
    , bgHover = Colors.darkerGrey
    , fgActive = Colors.amazonite
    }


cobaltScheme =
    { bg = Colors.cobalt
    , fg = Colors.white
    , bgHover = Colors.darkCobalt
    , fgActive = Colors.amazonite
    }


colorize : ColorScheme -> Mixin
colorize scheme =
    mixin
        [ color scheme.fg
        , fontWeight bold
        , cursor pointer
        , backgroundColor scheme.bg
        , hover
            [ backgroundColor scheme.bgHover
            ]
        , active [ color scheme.fgActive ]
        , withClass Active
            [ color scheme.fgActive
            , backgroundColor scheme.bgHover
            ]
        ]
