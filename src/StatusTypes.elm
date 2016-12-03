module StatusTypes exposing (..)

import RemoteData exposing (..)


type alias ServerRec =
    { up : Bool
    , lastPing : Maybe String
    }


type alias StatusRec =
    { server : ServerRec
    , invalidConfigGroups : List String
    }


type alias WebStatus =
    WebData StatusRec
