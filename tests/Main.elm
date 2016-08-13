port module Main exposing (..)

import AccountRecordTests
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Never
main =
    run emit AccountRecordTests.all


port emit : ( String, Value ) -> Cmd msg
