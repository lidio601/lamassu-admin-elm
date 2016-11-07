module Machine exposing (..)

import Html exposing (..)
import Css.Admin exposing (..)
import Css.Classes as C
import RemoteData exposing (..)
import HttpBuilder exposing (..)
import MachinesDecoder exposing (machinesDecoder)
import MachinesEncoder exposing (encodeAction)
import MachineTypes exposing (..)
import Task


type alias Model =
    RemoteData (Error String) Machines


init : Model
init =
    NotAsked


load : ( Model, Cmd Msg )
load =
    ( Loading, getForm )


getForm : Cmd Msg
getForm =
    get ("/api/machines")
        |> send (jsonReader machinesDecoder) stringReader
        |> Task.map .data
        |> RemoteData.asCmd
        |> Cmd.map Load


postForm : MachineAction -> Cmd Msg
postForm action =
    post "/api/machines"
        |> withHeader "Content-Type" "application/json"
        |> withJsonBody (encodeAction action)
        |> send (jsonReader machinesDecoder) stringReader
        |> Task.map .data
        |> RemoteData.asCmd
        |> Cmd.map Load


type Msg
    = Action
    | Load Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Action ->
            model ! []

        Load loadedModel ->
            loadedModel ! []


rowView : Machine -> Html Msg
rowView machine =
    tr []
        [ td [] [ text machine.name ]
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
                            [ table [ class [ C.ConfigTable ] ]
                                [ thead []
                                    [ tr []
                                        [ td [] [ text "Machine" ]
                                        , td [] [ text "Top Bill Count" ]
                                        , td [] [ text "Bottom Bill Count" ]
                                        ]
                                    ]
                                , tbody [] (List.map rowView machines)
                                ]
                            ]
                        ]
                    ]
                ]
