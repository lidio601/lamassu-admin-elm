module Css.Main exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, a, div, td, thead, tbody, input, button)
import Css.Namespace exposing (namespace)
import Css.Colors as Colors
import Css.ColorSchemes exposing (..)
import Css.Classes exposing (..)
import Css.Selectize


type CssIds
    = Page


mainBackgroundColor : Color
mainBackgroundColor =
    Colors.lightGrey


contentBackgroundColor : Color
contentBackgroundColor =
    Colors.white


navBackgroundColor : Color
navBackgroundColor =
    Colors.darkGrey


navItemActiveBackgroundColor : Color
navItemActiveBackgroundColor =
    Colors.darkerGrey


navItemActiveColor : Color
navItemActiveColor =
    Colors.amazonite


navItemColor : Color
navItemColor =
    Colors.sandstone


cryptoTabsBackgroundColor : Color
cryptoTabsBackgroundColor =
    Colors.cobalt


cryptoTabsHoverBackgroundColor : Color
cryptoTabsHoverBackgroundColor =
    Colors.darkCobalt


cryptoTabsColor : Color
cryptoTabsColor =
    Colors.white


cryptoTabsActiveColor : Color
cryptoTabsActiveColor =
    Colors.amazonite


cobaltBG : Color
cobaltBG =
    Colors.cobalt


cobaltHoverBG : Color
cobaltHoverBG =
    Colors.darkCobalt


cobaltColor : Color
cobaltColor =
    Colors.white


cobaltActiveColor : Color
cobaltActiveColor =
    Colors.amazonite


css : Stylesheet
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
                    , colorize lightGreyScheme
                    , firstChild
                        [ borderRadius4 (px 5) (px 0) (px 0) (px 0)
                        ]
                    , lastChild
                        [ borderRadius4 (px 0) (px 5) (px 0) (px 0)
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
            , borderRadius4 (px 0) (px 7) (px 7) (px 7)
            , backgroundColor mainBackgroundColor
            , margin3 zero zero (px 10)
            ]
        , (.) ConfigTable
            [ fontSize (px 14)
            , borderRadius (px 7)
            , margin2 (px 20) zero
            , property "border-collapse" "collapse"
            , descendants
                [ (.) Css.Selectize.SelectizeContainer
                    [ Css.Selectize.component ]
                , input
                    [ border zero
                    , borderRadius (px 3)
                    , padding (px 6)
                    , textAlign right
                    , width (pct 100)
                    , fontFamilies [ "Fira Code" ]
                    , fontWeight (int 600)
                    , property "outline" "none"
                    , property "background-color" "inherit"
                    ]
                , td
                    [ padding2 (px 3) (px 4)
                    , textAlign center
                    , width (px 100)
                    , verticalAlign bottom
                    ]
                , (.) Component
                    [ borderRadius (px 3)
                    , border3 (px 2) solid Colors.lightGrey
                    , backgroundColor Colors.white
                    ]
                , (.) FocusedComponent
                    [ borderTopColor Colors.amazonite ]
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
