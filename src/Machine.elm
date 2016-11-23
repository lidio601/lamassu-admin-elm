module Machine exposing (..)

import Html exposing (..)
import Html.Attributes exposing (defaultValue)
import Html.Events exposing (onClick, onInput)
import Css.Admin exposing (..)
import Css.Classes as C
import RemoteData exposing (..)
import Http
import HttpBuilder exposing (..)
import MachinesDecoder exposing (machinesDecoder)
import MachinesEncoder exposing (encodeAction)
import MachineTypes exposing (..)
import String
import List


type alias Model =
    RemoteData.WebData Machines


init : Model
init =
    NotAsked


load : ( Model, Cmd Msg )
load =
    ( Loading, getForm )


getForm : Cmd Msg
getForm =
    get ("/api/machines")
        |> withExpect (Http.expectJson machinesDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map Load


postForm : MachineAction -> Cmd Msg
postForm action =
    post "/api/machines"
        |> withJsonBody (encodeAction action)
        |> withExpect (Http.expectJson machinesDecoder)
        |> send RemoteData.fromResult
        |> Cmd.map Load


type Msg
    = Action
    | Load Model
    | InputCassette Machine Position String
    | SubmitResetBills Machine


type Position
    = Top
    | Bottom


updateMachine : Machine -> Machine -> Machine
updateMachine machine oldMachine =
    if machine.deviceId == oldMachine.deviceId then
        machine
    else
        oldMachine


updateCassette : Machine -> Position -> String -> Machines -> ( Machines, Cmd Msg )
updateCassette machine position str machines =
    let
        countResult =
            String.toInt str

        updatedMachine =
            case countResult of
                Ok count ->
                    case position of
                        Top ->
                            { machine | cassette1 = count }

                        Bottom ->
                            { machine | cassette2 = count }

                Err _ ->
                    machine
    in
        (List.map (updateMachine updatedMachine) machines) ! []


updateSubmitCassette : Machine -> Machines -> ( Machines, Cmd Msg )
updateSubmitCassette machine machines =
    machines ! [ postForm (ResetCashOutBills machine) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Action ->
            model ! []

        Load loadedModel ->
            loadedModel ! []

        InputCassette machine position str ->
            RemoteData.update (updateCassette machine position str) model

        SubmitResetBills machine ->
            RemoteData.update (updateSubmitCassette machine) model


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
    tr []
        [ td [] [ text machine.name ]
        , td []
            [ div [ classList [ ( C.Component, True ), ( C.FocusedComponent, False ) ] ]
                [ inputCassetteView machine Top machine.cassette1 ]
            ]
        , td []
            [ div [ classList [ ( C.Component, True ), ( C.FocusedComponent, False ) ] ]
                [ inputCassetteView machine Bottom machine.cassette2 ]
            ]
        , td []
            [ button [ class [ C.TableButton ], onClick (SubmitResetBills machine) ] [ text "Reset Bills" ]
            ]
        ]


tableView : Machines -> Html Msg
tableView machines =
    if List.isEmpty machines then
        div [] [ text "No paired machines." ]
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

        Success machines ->
            div []
                [ div [ class [ C.SectionLabel ] ]
                    [ div []
                        [ div [ class [ C.ConfigContainer ] ]
                            [ tableView machines ]
                        ]
                    ]
                ]
