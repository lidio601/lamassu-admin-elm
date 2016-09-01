module AdminCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, a, div, td, thead, tbody, input, button)
import Css.Namespace exposing (namespace)
import Colors
import ColorSchemes exposing (..)
import CssClasses exposing (..)


type CssIds
    = Page


mainBackgroundColor =
    Colors.lightGrey


contentBackgroundColor =
    Colors.white


navBackgroundColor =
    Colors.darkGrey


navItemActiveBackgroundColor =
    Colors.darkerGrey


navItemActiveColor =
    Colors.amazonite


navItemColor =
    Colors.sandstone


cryptoTabsBackgroundColor =
    Colors.cobalt


cryptoTabsHoverBackgroundColor =
    Colors.darkCobalt


cryptoTabsColor =
    Colors.white


cryptoTabsActiveColor =
    Colors.amazonite


cobaltBG =
    Colors.cobalt


cobaltHoverBG =
    Colors.darkCobalt


cobaltColor =
    Colors.white


cobaltActiveColor =
    Colors.amazonite


css =
    (stylesheet << namespace "lamassuAdmin")
        [ body
            [ fontFamilies [ "Brandon Text" ]
            , margin zero
            ]
        , (.) ConfigButtonRow
            [ textAlign right ]
        , (.) ConfigButton
            [ colorize cobaltScheme
            , padding2 (px 10) (px 15)
            , display inlineBlock
            , borderRadius (px 5)
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
        , (.) CryptoTabs
            [ displayFlex
            , children
                [ div
                    [ padding2 (px 10) (px 15)
                    , colorize darkGreyScheme
                    , firstChild
                        [ borderRadius4 (px 5) (px 0) (px 0) (px 5)
                        ]
                    , lastChild
                        [ borderRadius4 (px 0) (px 5) (px 5) (px 0)
                        ]
                    ]
                ]
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
                    [ height (px 60)
                    , display block
                    , lineHeight (px 60)
                    , padding2 (px 0) (px 20)
                    , colorize darkGreyScheme
                    ]
                ]
            ]
        ]
