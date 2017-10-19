module Logs.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Css.Admin exposing (..)
import Css.Classes as C
import RemoteData exposing (..)
import List
import Logs.Types exposing (..)
import Date exposing (..)
import Date.Extra exposing (toFormattedString)
import MaintenanceMachines.Types exposing (Machines, Machine)


machineLink : Machine -> Html Msg
machineLink machine =
    a [ href ("/#logs/" ++ machine.deviceId) ] [ text machine.name ]


formatDate : Maybe Date -> String
formatDate date =
    case date of
        Just date ->
            toFormattedString "yyyy-MM-dd HH:mm" date

        Nothing ->
            ""


maybeText : Maybe String -> Html Msg
maybeText maybeString =
    text (Maybe.withDefault "" maybeString)


rowView : Log -> Html Msg
rowView log =
    tr [ class [] ]
        [ td [] [ maybeText log.name ]
        , td [] [ text (formatDate log.timestamp) ]
        , td [] [ maybeText log.logLevel ]
        , td [] [ maybeText log.message ]
        ]


machineRowView : Machine -> Html Msg
machineRowView machine =
    tr [ class [] ]
        [ td [] [ machineLink machine ]
        ]


machinesView : Machines -> Html Msg
machinesView machines =
    if List.isEmpty machines then
        div [ class [ C.EmptyTable ] ] [ text "No paired machines." ]
    else
        div []
            [ h1 [] [ text "Paired Machines" ]
            , table [ class [ C.TxTable ] ]
                [ thead []
                    [ tr []
                        [ td [] [ text "Name" ]
                        ]
                    ]
                , tbody [] (List.map machineRowView machines)
                ]
            ]


tableView : Logs -> Html Msg
tableView logs =
    if List.isEmpty logs then
        div [] [ text "No logs yet." ]
    else
        div []
            [ h1 [] [ text "Latest logs" ]
            , table [ class [ C.TxTable ] ]
                [ thead []
                    [ tr []
                        [ td [] [ text "Machine" ]
                        , td [] [ text "Date" ]
                        , td [] [ text "Level" ]
                        , td [] [ text "Message" ]
                        ]
                    ]
                , tbody [] (List.map rowView logs)
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

        Success logs ->
            div [] [ tableView logs ]
