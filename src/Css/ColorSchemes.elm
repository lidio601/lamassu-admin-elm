module Css.ColorSchemes exposing (..)

import Css exposing (..)
import Css.Colors as Colors
import Css.Classes exposing (..)


type alias ColorScheme =
    { bg : Color
    , fg : Color
    , bgHover : Color
    , fgActive : Color
    }


darkGreyScheme : { bg : Color, bgHover : Color, fg : Color, fgActive : Color }
darkGreyScheme =
    { bg = Colors.darkGrey
    , fg = Colors.sandstone
    , bgHover = Colors.darkerGrey
    , fgActive = Colors.amazonite
    }


lightGreyScheme : { bg : Color, bgHover : Color, fg : Color, fgActive : Color }
lightGreyScheme =
    { bg = Colors.lightGrey
    , fg = Colors.sandstone
    , bgHover = Colors.darkerLightGrey
    , fgActive = Colors.sandstone
    }


cobaltScheme : { bg : Color, bgHover : Color, fg : Color, fgActive : Color }
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
