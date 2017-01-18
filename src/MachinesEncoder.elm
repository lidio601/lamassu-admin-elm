module MachinesEncoder exposing (encodeAction)

import Json.Encode exposing (..)
import MachineTypes exposing (..)


encodeAction : MachineAction -> Value
encodeAction action =
    case action of
        ResetCashOutBills machine ->
            object
                [ ( "action", string "resetCashOutBills" )
                , ( "deviceId", string machine.deviceId )
                , ( "cassettes", list [ int machine.cassette1, int machine.cassette2 ] )
                ]

        UnpairMachine machine ->
            object
                [ ( "action", string "unpair" )
                , ( "deviceId", string machine.deviceId )
                ]

        RepairMachine machine ->
            object
                [ ( "action", string "repair" )
                , ( "deviceId", string machine.deviceId )
                ]
