module Maintenance.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (defaultValue)
import Html.Events exposing (onClick, onInput)
import Css.Admin exposing (..)
import Css.Classes as C
import List
import RemoteData exposing (..)
import Maintenance.Types exposing (..)


inputCassetteView : Machine -> Position -> Int -> Html Msg
inputCassetteView machine position count =
    input
        [ class [ C.BasicInput ]
        , onInput (InputCassette machine position)
        , defaultValue (toString count)
        ]
        []


rowView : Machine -> Html Msg
rowView machine =
    let
        actions =
            if machine.paired then
                [ td []
                    [ button [ class [ C.TableButton ], onClick (Submit (ResetCashOutBills machine)) ] [ text "Reset Bills" ]
                    ]
                , td []
                    [ button [ class [ C.TableButton ], onClick (Submit (UnpairMachine machine)) ] [ text "Unpair" ] ]
                ]
            else
                [ td []
                    [ button [ class [ C.TableButton ], onClick (Submit (ResetCashOutBills machine)) ] [ text "Reset Bills" ] ]
                , td [ class [ C.NoInput ] ] [ text "Unpaired" ]
                ]
    in
        tr []
            ([ td [] [ text machine.name ]
             , td []
                [ div [ classList [ ( C.Component, True ), ( C.FocusedComponent, False ) ] ]
                    [ inputCassetteView machine Top machine.cassette1 ]
                ]
             , td []
                [ div [ classList [ ( C.Component, True ), ( C.FocusedComponent, False ) ] ]
                    [ inputCassetteView machine Bottom machine.cassette2 ]
                ]
             ]
                ++ actions
            )


tableView : Machines -> Html Msg
tableView machines =
    if List.isEmpty machines then
        div [ class [ C.EmptyTable ] ] [ text "No paired machines." ]
    else
        table [ class [ C.ConfigTable ] ]
            [ thead []
                [ tr []
                    [ td [] []
                    , td [] [ text "Top Bill Count" ]
                    , td [] [ text "Bottom Bill Count" ]
                    ]
                ]
            , tbody [] (List.map rowView machines)
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

        Success subModel ->
            let
                statusString =
                    case subModel.status of
                        Saved ->
                            "Saved"

                        _ ->
                            ""
            in
                div []
                    [ div [ class [ C.SectionLabel ] ]
                        [ div []
                            [ div [ class [ C.ConfigContainer ] ]
                                [ tableView subModel.machines
                                , div [ class [ C.Saving ] ] [ text statusString ]
                                ]
                            ]
                        ]
                    ]
