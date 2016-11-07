module MachinesEncoder exposing (encodeAction)

import Json.Encode exposing (..)
import MachineTypes exposing (..)


encodeAction : MachineAction -> Value
encodeAction action =
    case action of
        ResetCashOutBills top bottom ->
            object
                [ ( "action", string "resetCashOutBills" )
                , ( "cassettes", list [ int top, int bottom ] )
                ]
