module AdminCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, a, div, td, thead, tbody, input)
import Css.Namespace exposing (namespace)


type CssClasses
    = NavBar
    | MainLeft
    | MainRight
    | NavBarItemActive
    | Content
    | ConfigGroupLabel
    | ConfigTable
    | ConfigTableGlobalRow
    | ConfigTableContainer


type CssIds
    = Page


mainBackgroundColor =
    hex "f6f6f4"


contentBackgroundColor =
    hex "ffffff"


navBackgroundColor =
    hex "2d2d2d"


navItemActiveBackgroundColor =
    hex "282828"


navItemActiveColor =
    hex "56e7d7"


navItemColor =
    hex "5f5f56"


css =
    (stylesheet << namespace "lamassuAdmin")
        [ body
            [ fontFamilies [ "Brandon Text" ]
            , margin zero
            ]
        , (.) MainLeft
            [ backgroundColor navBackgroundColor
            , height (pct 100)
            ]
        , (.) MainRight
            [ backgroundColor mainBackgroundColor
            , height (pct 100)
            ]
        , (.) Content
            [ margin2 (px 20) (px 20)
            , backgroundColor contentBackgroundColor
            , padding (px 40)
            , borderRadius (px 5)
            ]
        , (.) ConfigGroupLabel
            [ fontWeight bold
            , fontSize (px 30)
            , marginBottom (px 10)
            ]
        , (.) ConfigTableContainer
            [ padding (px 10)
            , borderRadius (px 7)
            , backgroundColor mainBackgroundColor
            , margin2 (px 10) zero
            ]
        , (.) ConfigTable
            [ fontSize (px 14)
            , borderRadius (px 7)
            , margin2 (px 20) zero
            , property "border-collapse" "collapse"
            , descendants
                [ input
                    [ border zero
                    , borderRadius (px 3)
                    , padding (px 6)
                    , textAlign right
                    , width (pct 100)
                    , fontFamilies [ "Fira Code" ]
                    , fontWeight (int 600)
                    ]
                , td
                    [ padding2 (px 5) (px 10)
                    , textAlign center
                    , width (px 100)
                    , verticalAlign bottom
                    ]
                , tbody
                    [ descendants
                        [ td
                            [ textAlign right
                            ]
                        ]
                    ]
                , thead
                    [ fontWeight bold
                    , textAlign left
                    ]
                , (.) ConfigTableGlobalRow
                    [ fontWeight bold ]
                ]
            ]
        , (.) NavBar
            [ margin zero
            , padding zero
            , fontSize (px 18)
            , children
                [ div
                    [ color navItemColor
                    , height (px 60)
                    , display block
                    , lineHeight (px 60)
                    , padding2 (px 0) (px 20)
                    , textDecoration none
                    , fontWeight bold
                    , hover
                        [ backgroundColor navItemActiveBackgroundColor
                        , color navItemActiveColor
                        ]
                    , withClass NavBarItemActive
                        [ color navItemActiveColor
                        ]
                    , cursor pointer
                    ]
                ]
            ]
        ]


primaryAccentColor =
    hex "ccffaa"
