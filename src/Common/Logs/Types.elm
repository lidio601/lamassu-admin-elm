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


type alias SupportLogSnapshot =
    { deviceId : String
    , timestamp : Date
    }


type alias SupportLog =
    { deviceId : String
    , timestamp : Date
    , name : String
    }


type alias SupportLogs =
    List SupportLog


type alias Logs =
    { logs : List Log, currentMachine : Machine }
