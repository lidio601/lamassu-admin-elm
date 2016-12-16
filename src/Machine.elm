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
import Process
import Task
import Time exposing (second)


type SavingStatus
    = Saving
    | Saved
    | Editing
    | NotSaving


type alias SubModel =
    { status : SavingStatus
    , machines : Machines
    }


type alias Model =
    RemoteData.WebData SubModel


init : Model
init =
    NotAsked


load : ( Model, Cmd Msg )
load =
    ( Loading, getForm )


toModel : SavingStatus -> Machines -> SubModel
toModel status machines =
    { status = status, machines = machines }


getForm : Cmd Msg
getForm =
    get ("/api/machines")
        |> withExpect (Http.expectJson machinesDecoder)
        |> send (Result.map (toModel NotSaving) >> RemoteData.fromResult)
        |> Cmd.map Load


postForm : MachineAction -> Cmd Msg
postForm action =
    post "/api/machines"
        |> withJsonBody (encodeAction action)
        |> withExpect (Http.expectJson machinesDecoder)
        |> send (Result.map (toModel Saved) >> RemoteData.fromResult)
        |> Cmd.map Load


type Msg
    = Action
    | Load Model
    | InputCassette Machine Position String
    | SubmitResetBills Machine
    | HideSaveIndication


type Position
    = Top
    | Bottom


updateMachine : Machine -> Machine -> Machine
updateMachine machine oldMachine =
    if machine.deviceId == oldMachine.deviceId then
        machine
    else
        oldMachine


updateCassette : Machine -> Position -> String -> SubModel -> ( SubModel, Cmd Msg )
updateCassette machine position str subModel =
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

        machines =
            List.map (updateMachine updatedMachine) subModel.machines
    in
        { subModel | machines = machines } ! []


updateSubmitCassette : Machine -> SubModel -> ( SubModel, Cmd Msg )
updateSubmitCassette machine subModel =
    subModel ! [ postForm (ResetCashOutBills machine) ]


saveUpdate : SubModel -> ( SubModel, Cmd Msg )
saveUpdate model =
    let
        cmd =
            if (model.status == Saved) then
                Process.sleep (2 * second)
                    |> Task.perform (\_ -> HideSaveIndication)
            else
                Cmd.none
    in
        model ! [ cmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Action ->
            model ! []

        Load newModel ->
            RemoteData.update saveUpdate newModel

        InputCassette machine position str ->
            RemoteData.update (updateCassette machine position str) model

        SubmitResetBills machine ->
            RemoteData.update (updateSubmitCassette machine) model

        HideSaveIndication ->
            RemoteData.update (\subModel -> { subModel | status = NotSaving } ! []) model


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
