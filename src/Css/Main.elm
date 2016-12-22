module Css.Main exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li, a, div, td, th, tr, thead, tbody, input, button, label, p)
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
        , p
            [ margin zero ]
        , (.) Layout
            [ displayFlex
            , height (pct 100)
            ]
        , (.) StatusBar
            [ position fixed
            , bottom zero
            , padding2 (px 10) (px 20)
            , backgroundColor Colors.sandstone
            , color Colors.white
            , width (pct 100)
            ]
        , (.) FormRow
            [ margin2 (px 20) zero
            , firstChild
                [ margin zero
                ]
            , descendants
                [ label
                    [ fontSize (px 11)
                    , fontWeight bold
                    , children
                        [ div
                            [ margin3 zero zero (px 5)
                            , color Colors.sandstone
                            ]
                        ]
                    ]
                , input
                    [ border zero
                    , backgroundColor Colors.white
                    , borderRadius (px 3)
                    , padding (px 6)
                    , textAlign left
                    , fontFamilies [ "Fira Code" ]
                    , fontWeight (int 600)
                    , width (pct 90)
                    , property "outline" "none"
                    ]
                ]
            ]
        , (.) ButtonRow
            [ textAlign left ]
        , (.) Button
            [ colorize cobaltScheme
            , padding2 (px 10) (px 15)
            , display inlineBlock
            , borderRadius (px 5)
            , withClass Disabled
                [ backgroundColor Colors.darkerLightGrey
                , color Colors.white
                , cursor default
                ]
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
            [ padding (px 20)
            , backgroundColor Colors.lightGrey
            , borderRadius (px 5)
            , width (pct 100)
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
        , (.) SectionLabel
            [ fontWeight bold
            , fontSize (px 30)
            , marginBottom (px 10)
            ]
        , (.) TxTable
            [ borderRadius (px 7)
            , margin2 (px 20) zero
            , property "border-collapse" "collapse"
            , fontSize (px 14)
            , width (pct 100)
            , backgroundColor Colors.white
            , descendants
                [ (.) NumberColumn
                    [ textAlign right
                    , width (em 10)
                    ]
                , (.) DirectionColumn
                    [ textAlign left
                    , fontWeight bold
                    , fontSize (pct 90)
                    ]
                , tbody
                    [ fontFamilies [ "Fira Code" ]
                    , color Colors.sandstone
                    , descendants
                        [ td
                            [ padding2 (px 2) (px 14)
                            , borderBottom3 (px 1) solid Colors.lightGrey
                            , whiteSpace noWrap
                            ]
                        , (.) TruncatedColumn
                            [ maxWidth zero
                            , overflow hidden
                            , width (px 300)
                            , textOverflow ellipsis
                            ]
                        , (.) TxDate [ width (em 10) ]
                        , (.) TxAddress
                            [ width (em 25)
                            ]
                        ]
                    ]
                , thead
                    [ fontSize (px 14)
                    , textAlign center
                    , color Colors.sandstone
                    , descendants
                        [ td
                            [ borderBottom3 (px 2) solid Colors.lightGrey
                            , padding (px 5)
                            ]
                        ]
                    ]
                ]
            ]
        , (.) EmptyTable
            [ fontSize (px 20)
            , fontWeight normal
            ]
        , (.) ConfigTable
            [ fontSize (px 14)
            , fontWeight bold
            , borderRadius (px 7)
            , margin2 (px 20) zero
            , property "border-collapse" "collapse"
            , descendants
                [ (.) Css.Selectize.SelectizeContainer
                    [ Css.Selectize.component ]
                , (.) InputContainer
                    [ displayFlex
                    , property "justify-content" "flex-end"
                    ]
                , (.) UnitDisplay
                    [ borderRadius4 (px 0) (px 3) (px 3) (px 0)
                    , backgroundColor Colors.darkerLightGrey
                    , color Colors.sandstone
                    , padding2 zero (px 5)
                    , fontWeight (int 700)
                    , fontSize (pct 80)
                    , lineHeight (px 25)
                    ]
                , input
                    [ border zero
                    , borderRadius (px 3)
                    , padding (px 6)
                    , textAlign right
                    , width (pct 100)
                    , fontFamilies [ "Fira Code" ]
                    , fontWeight (int 600)
                    , outline none
                    , backgroundColor Colors.white
                    ]
                , (.) BasicInputDisabled
                    [ backgroundColor Colors.lighterLightGrey
                    , height (px 25)
                    , lineHeight (px 25)
                    , fontSize (px 11)
                    , fontWeight (int 500)
                    , color Colors.sandstone
                    , textAlign center
                    , cursor default
                    ]
                , td
                    [ padding2 (px 3) (px 4)
                    , textAlign center
                    , verticalAlign middle
                    , width (em 5)
                    ]
                , (.) Component
                    [ borderRadius (px 3)
                    , border3 (px 2) solid Colors.lightGrey
                    , backgroundColor Colors.white
                    ]
                , (.) InvalidComponent
                    [ borderTopColor Colors.red
                    , descendants
                        [ input
                            [ color Colors.red
                            ]
                        ]
                    ]
                , (.) FocusedComponent
                    [ borderTopColor Colors.amazonite ]
                , tbody
                    [ descendants
                        [ td
                            [ textAlign right
                            , whiteSpace noWrap
                            ]
                        , td
                            [ firstChild
                                [ fontWeight normal
                                ]
                            ]
                        ]
                    ]
                , thead
                    [ fontWeight bold
                    , textAlign left
                    ]
                , (.) MultiDisplay
                    [ backgroundColor Colors.darkerLightGrey
                    , borderLeft3 (px 3) solid Colors.lightGrey
                    , borderRight3 (px 3) solid Colors.lightGrey
                    , borderRadius (px 3)
                    ]
                , th
                    [ padding2 (px 3) (px 4)
                    , textAlign center
                    ]
                , (.) ConfigTableGlobalRow
                    [ descendants
                        [ td
                            [ firstChild
                                [ fontWeight bold
                                ]
                            ]
                        ]
                    ]
                , (.) TextCell
                    [ textAlign left ]
                , (.) ShortCell
                    [ minWidth (em 5) ]
                , (.) MediumCell
                    [ minWidth (em 10) ]
                , (.) LongCell
                    [ minWidth (em 20) ]
                ]
            ]
        , (.) Saving
            [ fontSize (px 18)
            , fontWeight normal
            , textAlign right
            ]
        , (.) NavBar
            [ margin zero
            , padding4 zero zero (px 60) zero
            , backgroundColor Colors.darkGrey
            , fontSize (px 18)
            , width (em 15)
            , maxWidth (em 15)
            , minWidth (em 15)
            , descendants
                [ (.) NavBarRoute
                    [ height (px 60)
                    , display block
                    , lineHeight (px 60)
                    , padding2 (px 0) (px 20)
                    , colorize darkGreyScheme
                    ]
                , (.) NavBarCategory
                    [ height (px 60)
                    , display block
                    , lineHeight (px 60)
                    , padding2 (px 0) (px 20)
                    , colorize darkGreyScheme
                    ]
                , (.) InvalidGroup
                    [ color Colors.red |> important ]
                , (.) NavBarCategoryContainer
                    [ descendants
                        [ (.) NavBarRoute
                            [ colorize darkGreyScheme
                            , padding4 zero (px 20) zero (px 30)
                            , fontWeight (int 500)
                            , property "animation" "fadein 0.8s"
                            ]
                        ]
                    ]
                ]
            ]
        ]
