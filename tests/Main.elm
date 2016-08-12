port module Main exposing (..)

import InitialRecordTests
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Never
main =
    run emit InitialRecordTests.all


port emit : ( String, Value ) -> Cmd msg
