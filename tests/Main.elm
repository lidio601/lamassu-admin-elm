port module Main exposing (..)

import AccountTypesTests
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Never
main =
    run emit AccountTypesTests.all


port emit : ( String, Value ) -> Cmd msg
