module MachineTypes exposing (..)


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
    | RepairMachine Machine
