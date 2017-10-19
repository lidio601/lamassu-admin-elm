module Logs.Types exposing (..)

import Date exposing (Date)
import RemoteData exposing (..)


type alias Log =
    { id : String
    , deviceId : Maybe String
    , name : Maybe String
    , timestamp : Maybe Date
    , logLevel : Maybe String
    , message : Maybe String
    }


type alias Logs =
    List Log


type alias Model =
    RemoteData.WebData Logs


type Msg
    = LoadLogs Model
