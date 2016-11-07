module MachineTypes exposing (..)


type alias Machine =
    { id : String
    , name : String
    }


type alias Machines =
    List Machine


type MachineAction
    = ResetCashOutBills Int Int
