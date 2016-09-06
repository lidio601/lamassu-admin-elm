module Css.Selectize exposing (..)

import Css exposing (..)
import Css.Colors as Colors
import Selectize
import Css.Admin exposing (..)
import Css.Elements exposing (input)


component : Mixin
component =
    mixin
        [ borderRadius (px 3)
        , position relative
        , descendants
            [ (.) SelectBox
                [ displayFlex
                , alignItems center
                , padding2 zero (px 3)
                , property "background-color" "inherit"
                ]
            , (.) BoxContainer
                [ position absolute
                , property "z-index" "100"
                , backgroundColor Colors.white
                , textAlign left
                , fontWeight (int 500)
                , fontSize (pct 80)
                , borderRadius (px 3)
                , backgroundColor Colors.darkerLightGrey
                , color Colors.sandstone
                , width (em 15)
                ]
            , (.) BoxItems
                []
            , (.) BoxItemActive
                [ color Colors.cobalt
                , fontWeight (int 900)
                ]
            , (.) BoxItem
                [ padding2 (px 3) (px 6)
                ]
            , (.) Info
                [ position absolute
                , property "z-index" "100"
                , backgroundColor Colors.white
                , textAlign left
                , fontWeight (int 500)
                , fontSize (pct 80)
                , padding2 (px 3) (px 6)
                , width (pct 100)
                , backgroundColor Colors.darkerLightGrey
                , color Colors.sandstone
                , borderRadius (px 3)
                , width (em 15)
                ]
            , (.) MultiItemContainer
                [ descendants
                    [ (.) SelectedItem
                        [ backgroundColor Colors.cobalt
                        , color Colors.white
                        , padding3 (px 2) (px 3) (px 1)
                        , fontFamilies [ "Inconsolata" ]
                        , fontSize (pct 70)
                        , fontWeight bold
                        , borderRadius (px 3)
                        ]
                    ]
                ]
            , (.) SingleItemContainer
                [ descendants
                    [ (.) SelectedItem
                        [ fontFamilies [ "Fira Code" ]
                        , fontSize (px 11)
                        , padding zero
                        , borderRadius zero
                        ]
                    ]
                ]
            , (.) FallbackItems
                [ children
                    [ (.) SelectedItem
                        [ backgroundColor Colors.sandstone
                        ]
                    ]
                ]
            , input
                [ textAlign left
                , opacity zero
                , property "background-color" "inherit"
                ]
            , (.) InputEditing
                [ opacity (int 1)
                ]
            ]
        ]


type Class
    = SelectizeContainer
    | SelectBox
    | BoxItems
    | BoxItem
    | BoxItemActive
    | SelectedItems
    | FallbackItems
    | SelectedItem
    | InputEditing
    | SingleItemContainer
    | MultiItemContainer
    | BoxContainer
    | Info
    | InfoNoMatches


classes : Selectize.HtmlClasses
classes =
    { container = className SelectizeContainer
    , singleItemContainer = className SingleItemContainer
    , multiItemContainer = className MultiItemContainer
    , selectBox = className SelectBox
    , selectedItems = className SelectedItems
    , fallbackItems = className FallbackItems
    , selectedItem = className SelectedItem
    , boxContainer = className BoxContainer
    , boxItems = className BoxItems
    , boxItem = className BoxItem
    , boxItemActive = className BoxItemActive
    , info = className Info
    , infoNoMatches = className InfoNoMatches
    , inputEditing = className InputEditing
    }
