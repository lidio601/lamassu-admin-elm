module Common.Logs.Types exposing (..)

import Date exposing (Date)


type alias Machine =
    { deviceId : String
    , name : String
    }


type alias Machines =
    List Machine


type alias Log =
    { id : String
    , timestamp : Maybe Date
    , logLevel : Maybe String
    , message : Maybe String
    }


type alias SupportLog =
    { id : String
    , deviceId : String
    , timestamp : Date
    }


type alias Logs =
    { logs : List Log, currentMachine : Machine }
