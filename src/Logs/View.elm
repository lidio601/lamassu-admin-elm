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


machineItemView : Machine -> Html Msg
machineItemView machine =
    li [] [ machineLink machine ]


machinesView : Machines -> Html Msg
machinesView machines =
    if List.isEmpty machines then
        div [ class [ C.EmptyTable ] ] [ text "No paired machines." ]
    else
        div []
            [ h2 [] [ text "Machines" ]
            , div [ class [ C.TxTable ] ]
                [ ul [] (List.map machineItemView machines)
                ]
            ]


logsView : Logs -> Html Msg
logsView logs =
    if List.isEmpty logs then
        div [] [ text "No logs yet." ]
    else
        div []
            [ h2 [] [ text "Logs" ]
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


machines : Model -> Html Msg
machines model =
    case model.machines of
        NotAsked ->
            div [] []

        Loading ->
            h2 [] [ text "Loading machines ..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success machines ->
            div [] [ machinesView machines ]


logs : Model -> Html Msg
logs model =
    case model.logs of
        NotAsked ->
            div [] []

        Loading ->
            h2 [] [ text "Loading logs..." ]

        Failure err ->
            div [] [ text (toString err) ]

        Success logs ->
            div [] [ logsView logs ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Latest Logs" ]
        , div [ class [ C.PaneWrapper ] ]
            [ div [ class [ C.LeftPane ] ] [ machines model ]
            , div [ class [ C.ContentPane ] ] [ logs model ]
            ]
        ]
