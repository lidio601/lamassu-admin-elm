module Customer.View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import RemoteData exposing (..)
import Css.Admin exposing (..)
import Css.Classes as C
import Common.Customer.Types exposing (..)
import Customer.Types exposing (..)
import Date exposing (..)
import Date.Extra exposing (toFormattedString)


customerActions : String -> Authorized -> Html Msg
customerActions id authorizedOverride =
    case authorizedOverride of
        Blocked ->
            button [ onClick (UnBlockCustomer id) ] [ text "Unblock" ]

        Verified ->
            button [ onClick (BlockCustomer id) ] [ text "Block" ]

        Automatic ->
            button [ onClick (UnBlockCustomer id) ] [ text "Unblock" ]


formatDate : Maybe Date -> String
formatDate date =
    case date of
        Just date ->
            toFormattedString "yyyy-MM-dd HH:mm" date

        Nothing ->
            ""


maybeText : Maybe String -> String
maybeText maybeString =
    Maybe.withDefault "" maybeString


customerView : Customer -> Html Msg
customerView customer =
    div []
        [ h1 [] [ text "Customer Details" ]
        , table [ class [ C.TxTable ] ]
            [ tbody []
                [ tr []
                    [ td [] [ text "Customer ID" ]
                    , td [] [ text customer.id ]
                    ]
                , tr []
                    [ td [] [ text "Name" ]
                    , td [] [ text (maybeText customer.name) ]
                    ]
                , tr []
                    [ td [] [ text "Phone" ]
                    , td [] [ text (maybeText customer.phone) ]
                    ]
                , tr []
                    [ td [] [ text "Completed phone at" ]
                    , td [] [ text (formatDate customer.phoneAt) ]
                    ]
                , tr []
                    [ td [] [ text "Created" ]
                    , td [] [ text (toFormattedString "yyyy-MM-dd HH:mm" customer.created) ]
                    ]
                , tr []
                    [ td [] [ text "Block Customer" ]
                    , td []
                        [ customerActions customer.id customer.authorizedOverride ]
                    ]
                , tr []
                    [ td [] [ text "Authorized at " ]
                    , td [] [ text (formatDate customer.authorizedAt) ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    case model of
        NotAsked ->
            div [] []

        Loading ->
            div [] [ text "Loading..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success customer ->
            div [] [ customerView customer ]
