module Css.Main exposing (..)

import Css exposing (..)
import Css.Elements
    exposing
        ( body
        , li
        , a
        , div
        , td
        , th
        , tr
        , thead
        , tbody
        , input
        , button
        , label
        , p
        , svg
        )
import Css.Namespace exposing (namespace)
import Css.LocalColors as Colors
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


codeFonts : List String
codeFonts =
    [ "Inconsolata", "monospace" ]


css : Stylesheet
css =
    (stylesheet << namespace "lamassuAdmin")
        [ body
            [ fontFamilies [ "Nunito", "sans-serif" ]
            , margin zero
            ]
        , p
            [ margin zero ]
        , class QrCode
            [ backgroundColor Colors.lightGrey
            , padding (px 10)
            , marginBottom (px 20)
            , borderRadius (px 6)
            , descendants
                [ svg
                    [ height (px 400)
                    , width (px 400)
                    ]
                ]
            ]
        , class Layout
            []
        , class Main
            [ displayFlex
            , height (pct 100)
            ]
        , class StatusBar
            [ position fixed
            , bottom zero
            , padding2 (px 10) (px 20)
            , backgroundColor Colors.sandstone
            , color Colors.white
            , width (pct 100)
            ]
        , class CashOut
            [ backgroundColor Colors.lightGrey
            ]
        , class FormRow
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
                    , fontFamilies codeFonts
                    , fontSize (px 14)
                    , fontWeight (int 600)
                    , width (pct 90)
                    , property "outline" "none"
                    ]
                ]
            ]
        , class ButtonRow
            [ textAlign right ]
        , class Button
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
        , class MainLeft
            [ backgroundColor navBackgroundColor
            , height (pct 100)
            ]
        , class MainRight
            [ backgroundColor mainBackgroundColor
            , height (pct 100)
            ]
        , class Content
            [ margin (px 20)
            , backgroundColor contentBackgroundColor
            , borderRadius (px 5)
            ]
        , class CryptoTabs
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
        , class SectionLabel
            [ fontWeight bold
            , fontSize (px 30)
            , marginBottom (px 10)
            ]
        , class ConfigContainer
            [ padding2 (px 20) (px 60)
            , borderRadius4 (px 0) (px 7) (px 7) (px 7)
            , backgroundColor mainBackgroundColor
            , margin3 zero zero (px 10)
            , property "animation" "fadein 0.8s"
            , overflow hidden
            , minHeight (em 15)
            , minWidth (em 20)
            ]
        , class NoInput
            [ fontFamilies codeFonts
            , color Colors.sandstone
            , fontWeight normal
            , textAlign left |> important
            ]
        , class TxTable
            [ borderRadius (px 7)
            , margin2 (px 20) zero
            , property "border-collapse" "collapse"
            , fontSize (px 14)
            , width (pct 100)
            , backgroundColor Colors.white
            , descendants
                [ class NumberColumn
                    [ textAlign right
                    , width (em 10)
                    ]
                , class DirectionColumn
                    [ textAlign left
                    , fontWeight bold
                    , fontSize (pct 90)
                    ]
                , tbody
                    [ fontFamilies codeFonts
                    , color Colors.sandstone
                    , descendants
                        [ td
                            [ padding2 (px 2) (px 14)
                            , borderBottom3 (px 1) solid Colors.lightGrey
                            , whiteSpace noWrap
                            ]
                        , class TruncatedColumn
                            [ maxWidth zero
                            , overflow hidden
                            , width (px 300)
                            , textOverflow ellipsis
                            ]
                        , class TxDate [ width (em 10) ]
                        , class TxAddress
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
        , class EmptyTable
            [ fontSize (px 20)
            , fontWeight normal
            ]
        , class ConfigTable
            [ fontSize (px 14)
            , fontWeight bold
            , borderRadius (px 7)
            , margin2 (px 20) zero
            , property "border-collapse" "collapse"
            , descendants
                [ class Css.Selectize.SelectizeContainer
                    [ Css.Selectize.component
                    , border3 (px 2) solid Colors.darkerLightGrey
                    , borderRadius (px 3)
                    ]
                , class InputContainer
                    [ displayFlex
                    , property "justify-content" "flex-end"
                    , border3 (px 2) solid Colors.darkerLightGrey
                    , borderRadius (px 3)
                    ]
                , class UnitDisplay
                    [ backgroundColor Colors.darkerLightGrey
                    , color Colors.sandstone
                    , padding2 zero (px 5)
                    , fontWeight (int 700)
                    , fontSize (pct 80)
                    , lineHeight (px 25)
                    , cursor default
                    , fontFamilies [ "Nunito", "sans-serif" ]
                    ]
                , input
                    [ border zero
                    , borderRadius (px 3)
                    , padding (px 6)
                    , textAlign right
                    , width (pct 100)
                    , fontFamilies codeFonts
                    , fontWeight (int 600)
                    , fontSize (px 14)
                    , outline none
                    , backgroundColor Colors.white
                    ]
                , class CellDisabled
                    [ property "background" "repeating-linear-gradient(45deg,#dfdfdc,#dfdfdc 2px,#e6e6e3 5px)"
                    ]
                , class BasicInput
                    [ pseudoElement "placeholder"
                        [ color Colors.amazonite
                        , opacity (num 1)
                        ]
                    ]
                , class BasicInputDisabled
                    [ height (px 25)
                    , lineHeight (px 25)
                    , fontSize (px 14)
                    , fontWeight (int 500)
                    , color Colors.sandstone
                    , opacity (num 0.7)
                    , textAlign left
                    , padding2 zero (em 1)
                    , property "background" "repeating-linear-gradient(45deg,#dfdfdc,#dfdfdc 2px,#e6e6e3 5px)"
                    ]
                , class ReadOnly
                    [ lineHeight (px 25)
                    , backgroundColor Colors.lightGrey
                    , fontFamilies codeFonts
                    , fontSize (px 14)
                    , fontWeight (int 600)
                    , color Colors.sandstone
                    , cursor default
                    , children
                        [ class BasicInputReadOnly
                            [ padding2 zero (px 5)
                            ]
                        ]
                    ]
                , td
                    [ padding2 (px 3) (px 4)
                    , textAlign center
                    , verticalAlign middle
                    , width (em 5)
                    ]
                , class Component
                    [ borderRadius (px 3)
                    , border3 (px 2) solid Colors.lightGrey
                    , backgroundColor Colors.white
                    ]
                , class FocusedComponent
                    [ children
                        [ class InputContainer
                            [ borderColor Colors.amazonite ]
                        ]
                    ]
                , class InvalidComponent
                    [ children
                        [ class InputContainer [ borderColor Colors.red ]
                        ]
                    , descendants
                        [ input
                            [ color Colors.red
                            ]
                        ]
                    ]
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
                , class MultiDisplay
                    [ backgroundColor Colors.darkerLightGrey
                    , borderLeft3 (px 3) solid Colors.lightGrey
                    , borderRight3 (px 3) solid Colors.lightGrey
                    , borderRadius (px 3)
                    ]
                , th
                    [ padding2 (px 3) (px 4)
                    , textAlign center
                    ]
                , class ConfigTableGlobalRow
                    [ descendants
                        [ td
                            [ firstChild
                                [ fontWeight bold
                                ]
                            ]
                        ]
                    ]
                , class TextCell
                    [ textAlign left ]
                , class ShortCell
                    [ minWidth (em 5) ]
                , class MediumCell
                    [ minWidth (em 10) ]
                , class LongCell
                    [ minWidth (em 20) ]
                ]
            ]
        , class Saving
            [ fontSize (px 18)
            , fontWeight normal
            , textAlign right
            ]
        , class NavBar
            [ margin zero
            , padding4 zero zero (px 110) zero
            , backgroundColor Colors.darkGrey
            , fontSize (px 18)
            , width (em 15)
            , maxWidth (em 15)
            , minWidth (em 15)
            , height (pct 100)
            , descendants
                [ class NavBarRoute
                    [ height (px 60)
                    , display block
                    , lineHeight (px 60)
                    , padding2 (px 0) (px 20)
                    , colorize darkGreyScheme
                    ]
                , class NavBarCategory
                    [ height (px 60)
                    , display block
                    , lineHeight (px 60)
                    , padding2 (px 0) (px 20)
                    , colorize darkGreyScheme
                    ]
                , class InvalidGroup
                    [ color Colors.red |> important ]
                , class NavBarCategoryContainer
                    [ descendants
                        [ class NavBarRoute
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
