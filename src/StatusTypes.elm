module StatusTypes exposing (..)

import RemoteData exposing (..)


type alias Rate =
    { crypto : String
    , bid : Float
    , ask : Float
    }


type alias ServerRec =
    { up : Bool
    , lastPing : Maybe String
    , rates : List Rate
    , machineStatus : String
    }


type alias StatusRec =
    { server : ServerRec
    , invalidConfigGroups : List String
    }


type alias WebStatus =
    WebData StatusRec
