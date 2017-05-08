module Maintenance.Types exposing (..)

import RemoteData exposing (..)


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


type alias Machine =
    { deviceId : String
    , name : String
    , cashbox : Int
    , cassette1 : Int
    , cassette2 : Int
    , paired : Bool
    }


type alias Machines =
    List Machine


type MachineAction
    = ResetCashOutBills Machine
    | UnpairMachine Machine


type Msg
    = Action
    | Load Model
    | InputCassette Machine Position String
    | Submit MachineAction
    | HideSaveIndication


type Position
    = Top
    | Bottom
